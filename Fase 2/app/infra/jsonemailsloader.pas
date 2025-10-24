unit JsonEmailsLoader;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, User, SinglyLinkedList, Email, AppState,
  DoublyLinkedList, EmailService, UserService;

function LoadEmailsFromJsonContent(const JsonText: string;
  const GlobalUsers: TSinglyLinkedList; out Skipped: integer): integer;

implementation

function ParseState(const S: string): TEmailState;
begin
  if SameText(Trim(S), 'L') then
    Result := esRead
  else
    Result := esUnread;
end;

function LoadEmailsFromJsonContent(const JsonText: string;
  const GlobalUsers: TSinglyLinkedList; out Skipped: integer): integer;
var
  Data, ArrItem: TJSONData;
  Obj, Root: TJSONObject;
  Arr: TJSONArray;
  i, JId: integer;
  JFrom, JTo, JSubject, JMsg, JState, JSendDate: string;
  Recv: PUser;
  Mail: PEmail;
begin
  Result := 0;
  Skipped := 0;

  Data := GetJSON(JsonText);
  try
    if (Data = nil) or (Data.JSONType <> jtObject) then Exit;

    Root := TJSONObject(Data);
    if not Root.Find('correos', Arr) then Exit;

    for i := 0 to Arr.Count - 1 do
    begin
      ArrItem := Arr.Items[i];
      if (ArrItem = nil) or (ArrItem.JSONType <> jtObject) then
      begin
        Inc(Skipped);
        Continue;
      end;

      Obj := TJSONObject(ArrItem);

      JId := Obj.Get('id', 0);
      JFrom := LowerCase(Trim(Obj.Get('remitente', '')));
      JTo := LowerCase(Trim(Obj.Get('destinatario', '')));
      JSubject := Obj.Get('asunto', '');
      JMsg := Obj.Get('mensaje', '');
      JState := Obj.Get('estado', 'NL');
      JSendDate := Obj.Get('fecha_envio','');

      Recv := FindUserByEmail(GlobalUsers, JTo);
      if Recv = nil then
      begin
        Inc(Skipped);
        Continue;
      end;

      Mail := CreateNewEmail(JFrom, JTo, JSubject, JMsg);
      if JId > 0 then
      begin
        Mail^.Id := JId;
        if NextEmailId <= JId then
          NextEmailId := JId + 1;
      end;
      Mail^.State := ParseState(JState);

      DoublyLinkedList.InsertLast(Recv^.Inbox, Mail);

      Inc(Result);
    end;
  finally
    Data.Free;
  end;
end;

end.
