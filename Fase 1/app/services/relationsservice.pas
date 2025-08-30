unit RelationsService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SparseMatrix, UserService, EmailService, User, Email,
  DoublyLinkedList, SinglyLinkedList;

type
  PEmailCount = ^TEmailCount;

  TEmailCount = record
    Count: integer;
    LastEmailDate: TDateTime;
  end;

  TEmailToIndexMap = record
    EmailList: TStringList;
    Count: integer;
  end;

  TEmailMatrixVisualizer = class
  private
    FMatrix: TSparseMatrix;
    FEmailMap: TEmailToIndexMap;
    FMaxUsers: integer;

    function GetOrCreateEmailIndex(const Email: string): integer;
    procedure InitializeEmailMap;
    procedure FreeEmailMap;

  public
    constructor Create(MaxUsers: integer = 1000);
    destructor Destroy; override;

    procedure BuildMatrixFromUsers(const UserList: TSinglyLinkedList);
    procedure GenerateVisualization(const FileName: string);
    procedure ShowMatrixStats;

    function GetEmailCount(const Sender, Receiver: string): integer;
    function GetTotalEmailsSent(const Sender: string): integer;
    function GetTotalEmailsReceived(const Receiver: string): integer;
    procedure GetTopSenders(out TopSenders: TStringList; Count: integer = 10);
    procedure GetTopReceivers(out TopReceivers: TStringList; Count: integer = 10);
  end;

implementation

constructor TEmailMatrixVisualizer.Create(MaxUsers: integer);
begin
  inherited Create;
  FMaxUsers := MaxUsers;
  InitializeEmailMap;
  SparseMatrix.Init(FMatrix, MaxUsers, MaxUsers);
end;

destructor TEmailMatrixVisualizer.Destroy;
begin
  SparseMatrix.Clear(FMatrix);
  FreeEmailMap;
  inherited Destroy;
end;

procedure TEmailMatrixVisualizer.InitializeEmailMap;
begin
  FEmailMap.EmailList := TStringList.Create;
  FEmailMap.EmailList.Sorted := False;
  FEmailMap.EmailList.Duplicates := dupIgnore;
  FEmailMap.Count := 0;
end;

procedure TEmailMatrixVisualizer.FreeEmailMap;
begin
  FEmailMap.EmailList.Free;
end;

function TEmailMatrixVisualizer.GetOrCreateEmailIndex(const Email: string): integer;
var
  E: string;
  idx: integer;
begin
  E := Trim(LowerCase(Email));
  idx := FEmailMap.EmailList.IndexOf(E);
  if idx = -1 then
  begin
    idx := FEmailMap.EmailList.Add(E);
    Inc(FEmailMap.Count);
  end;
  Result := idx;
end;

function EmailCountToString(Data: Pointer): string;
var
  EmailCount: PEmailCount;
begin
  EmailCount := PEmailCount(Data);
  if EmailCount = nil then
    Result := '0'
  else
    Result := IntToStr(EmailCount^.Count);
end;

procedure TEmailMatrixVisualizer.BuildMatrixFromUsers(const UserList: TSinglyLinkedList);
var
  UserNode: PSinglyNode;
  CurrentUser: PUser;
  EmailNode: PDoublyNode;
  CurrentEmail: PEmail;
  SenderIndex, ReceiverIndex: integer;
  ExistingCount: PEmailCount;
begin
  SparseMatrix.Clear(FMatrix);
  SparseMatrix.Init(FMatrix, FMaxUsers, FMaxUsers);

  FreeEmailMap;
  InitializeEmailMap;

  WriteLn('Construyendo matriz de correos...');

  UserNode := UserList.Head;
  while UserNode <> nil do
  begin
    CurrentUser := PUser(UserNode^.Data);
    if CurrentUser <> nil then
    begin
      WriteLn('Procesando usuario: ', CurrentUser^.Email);

      EmailNode := CurrentUser^.Inbox.Head;
      while EmailNode <> nil do
      begin
        CurrentEmail := PEmail(EmailNode^.Data);
        if CurrentEmail <> nil then
        begin
          SenderIndex := GetOrCreateEmailIndex(CurrentEmail^.Sender);
          ReceiverIndex := GetOrCreateEmailIndex(CurrentEmail^.Recipient);

          if (SenderIndex < FMaxUsers) and (ReceiverIndex < FMaxUsers) then
          begin
            ExistingCount := PEmailCount(SparseMatrix.Get(FMatrix,
              SenderIndex, ReceiverIndex));

            if ExistingCount = nil then
            begin
              New(ExistingCount);
              ExistingCount^.Count := 1;
              ExistingCount^.LastEmailDate := CurrentEmail^.Date;
              SparseMatrix.Insert(FMatrix, SenderIndex, ReceiverIndex, ExistingCount);
            end
            else
            begin
              Inc(ExistingCount^.Count);
              if CurrentEmail^.Date > ExistingCount^.LastEmailDate then
                ExistingCount^.LastEmailDate := CurrentEmail^.Date;
            end;
          end;
        end;
        EmailNode := EmailNode^.Next;
      end;
    end;
    UserNode := UserNode^.Next;
  end;

  WriteLn('Matriz construida exitosamente!');
  WriteLn('Usuarios únicos: ', FEmailMap.Count);
  WriteLn('Relaciones de correo: ', SparseMatrix.GetNodeCount(FMatrix));
end;

procedure TEmailMatrixVisualizer.GenerateVisualization(const FileName: string);
begin
  WriteLn('Generando visualización DOT...');
  SparseMatrix.GenerateDotFile(FMatrix, FileName, @EmailCountToString,
    FEmailMap.EmailList, FEmailMap.EmailList);
  WriteLn('Archivo DOT generado: ', FileName);
  WriteLn('Para convertir a imagen, usa: dot -Tpng ', FileName,
    ' -o matriz_correos.png');
end;

procedure TEmailMatrixVisualizer.ShowMatrixStats;
var
  TotalRelations: integer;
  i, j: integer;
  EmailCount: PEmailCount;
  MaxEmails, TotalEmails: integer;
  MaxSender, MaxReceiver: string;
begin
  TotalRelations := SparseMatrix.GetNodeCount(FMatrix);
  MaxEmails := 0;
  TotalEmails := 0;

  WriteLn('=== ESTADÍSTICAS DE LA MATRIZ DE CORREOS ===');
  WriteLn('Total de usuarios únicos: ', FEmailMap.Count);
  WriteLn('Total de relaciones de correo: ', TotalRelations);

  for i := 0 to FEmailMap.Count - 1 do
  begin
    for j := 0 to FEmailMap.Count - 1 do
    begin
      EmailCount := PEmailCount(SparseMatrix.Get(FMatrix, i, j));
      if EmailCount <> nil then
      begin
        Inc(TotalEmails, EmailCount^.Count);
        if EmailCount^.Count > MaxEmails then
        begin
          MaxEmails := EmailCount^.Count;
          MaxSender := FEmailMap.EmailList[i];
          MaxReceiver := FEmailMap.EmailList[j];
        end;
      end;
    end;
  end;

  WriteLn('Total de correos procesados: ', TotalEmails);
  if MaxEmails > 0 then
  begin
    WriteLn('Relación más activa: ', MaxSender, ' → ', MaxReceiver,
      ' (', MaxEmails, ' correos)');
  end;

  if TotalRelations > 0 then
    WriteLn('Promedio de correos por relación: ', TotalEmails div TotalRelations);

  WriteLn('===============================================');
end;

function TEmailMatrixVisualizer.GetEmailCount(const Sender, Receiver: string): integer;
var
  SenderIndex, ReceiverIndex: integer;
  EmailCount: PEmailCount;
begin
  Result := 0;
  if not FEmailMap.EmailList.Find(Sender, SenderIndex) then Exit;
  if not FEmailMap.EmailList.Find(Receiver, ReceiverIndex) then Exit;

  EmailCount := PEmailCount(SparseMatrix.Get(FMatrix, SenderIndex, ReceiverIndex));
  if EmailCount <> nil then
    Result := EmailCount^.Count;
end;

function TEmailMatrixVisualizer.GetTotalEmailsSent(const Sender: string): integer;
var
  SenderIndex, j: integer;
  EmailCount: PEmailCount;
begin
  Result := 0;
  if not FEmailMap.EmailList.Find(Sender, SenderIndex) then Exit;

  for j := 0 to FEmailMap.Count - 1 do
  begin
    EmailCount := PEmailCount(SparseMatrix.Get(FMatrix, SenderIndex, j));
    if EmailCount <> nil then
      Inc(Result, EmailCount^.Count);
  end;
end;

function TEmailMatrixVisualizer.GetTotalEmailsReceived(const Receiver: string): integer;
var
  ReceiverIndex, i: integer;
  EmailCount: PEmailCount;
begin
  Result := 0;
  if not FEmailMap.EmailList.Find(Receiver, ReceiverIndex) then Exit;

  for i := 0 to FEmailMap.Count - 1 do
  begin
    EmailCount := PEmailCount(SparseMatrix.Get(FMatrix, i, ReceiverIndex));
    if EmailCount <> nil then
      Inc(Result, EmailCount^.Count);
  end;
end;

procedure TEmailMatrixVisualizer.GetTopSenders(out TopSenders: TStringList;
  Count: integer);
var
  i: integer;
  UserEmail: string;
  TotalSent: integer;
begin
  TopSenders := TStringList.Create;
  TopSenders.Sorted := False;
  for i := 0 to FEmailMap.Count - 1 do
  begin
    UserEmail := FEmailMap.EmailList[i];
    TotalSent := GetTotalEmailsSent(UserEmail);
    if TotalSent > 0 then
      TopSenders.AddObject(Format('%s (%d correos)', [UserEmail, TotalSent]),
        TObject(PtrInt(TotalSent)));
  end;
end;

procedure TEmailMatrixVisualizer.GetTopReceivers(out TopReceivers: TStringList;
  Count: integer);
var
  i: integer;
  UserEmail: string;
  TotalReceived: integer;
begin
  TopReceivers := TStringList.Create;
  TopReceivers.Sorted := False;

  for i := 0 to FEmailMap.Count - 1 do
  begin
    UserEmail := FEmailMap.EmailList[i];
    TotalReceived := GetTotalEmailsReceived(UserEmail);
    if TotalReceived > 0 then
      TopReceivers.AddObject(Format('%s (%d correos)', [UserEmail, TotalReceived]),
        TObject(PtrInt(TotalReceived)));
  end;
end;

end.
