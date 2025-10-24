unit EmailService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DoublyLinkedList, User, Email, Stack,
  CircularLinkedList, AppState, BTree, AVLTree, Process;

type
  TEmailSendResult = (esrSuccess, esrEmptyRecipient, esrNotInContacts);
  TEmailDeliveryResult = (edrSuccess, edrUserNotFound);

function CompareEmailsByID(Email1, Email2: Pointer): integer;

function SendEmailToContact(const SenderUser: PUser;
  const RecipientEmail, Subject, MessageBody: string): TEmailSendResult;
function DeliverEmailToUser(const RecipientEmail: string;
  MailPtr: PEmail): TEmailDeliveryResult;
function CreateNewEmail(const Sender, Recipient, Subject, MessageBody:
  ansistring): PEmail;
  overload;
function CreateNewEmail(const Sender, Recipient, Subject, MessageBody: ansistring;
  ScheduledDate: TDateTime): PEmail; overload;

function ValidateEmailRecipient(const RecipientEmail: string;
  var SenderContacts: TCircularLinkedList): TEmailSendResult;

procedure MarkEmailAsRead(MailPtr: PEmail);
function CountUnreadEmails(const UserInbox: TDoublyLinkedList): integer;
procedure DeleteEmailFromInbox(var UserInbox: TDoublyLinkedList;
  var UserTrash: TStack; EmailNode: PDoublyNode);

procedure SaveDraft(CurrentUser: PUser; const Subject, MessageBody, Recipient: string);
function RemoveDraft(CurrentUser: PUser; DraftID: integer): boolean;
function UpdateDraft(CurrentUser: PUser; DraftID: integer;
  const NewRecipient, NewSubject, NewBody: string): boolean;
function SendDraftMail(CurrentUser: PUser; DraftID: integer): TEmailSendResult;
procedure AddFavorite(CurrentUser: PUser; MailPtr: PEmail);
function FindDraft(CurrentUser: PUser; DraftID: integer): PEmail;
function FindFavorite(CurrentUser: PUser; FavoriteID: integer): PEmail;
function RemoveFavorite(CurrentUser: PUser; FavoriteID: integer): boolean;

function CompareEmailsBySubject(Node1, Node2: PDoublyNode): integer;
function CompareEmailsByDate(Node1, Node2: PDoublyNode): integer;
function CompareEmailsBySender(Node1, Node2: PDoublyNode): integer;
function EmailToStr(Data: Pointer): string;
function GenerateDraftsAVLReport(const U: PUser; const OutDir: string;
  out DotPath, PngPath: string): boolean;

function GenerateFavoritesBTreeReport(const U: PUser; const OutDir: string;
  out DotPath, PngPath: string): boolean;

implementation

uses
  UserService, ContactService, DotUtils;

function CompareEmailsByID(Email1, Email2: Pointer): integer;
var
  E1, E2: PEmail;
begin
  E1 := PEmail(Email1);
  E2 := PEmail(Email2);
  if (E1 = nil) and (E2 = nil) then Exit(0);
  if E1 = nil then Exit(-1);
  if E2 = nil then Exit(1);

  if E1^.Id < E2^.Id then
    Result := -1
  else if E1^.Id > E2^.Id then
    Result := 1
  else
    Result := 0;
end;

procedure SaveDraft(CurrentUser: PUser; const Subject, MessageBody, Recipient: string);
var
  DraftEmail: PEmail;
begin
  if CurrentUser = nil then Exit;
  DraftEmail := CreateNewEmail(CurrentUser^.Email, Recipient, Subject, MessageBody);
  CurrentUser^.Drafts.Insert(DraftEmail);
end;

function RemoveDraft(CurrentUser: PUser; DraftID: integer): boolean;

  procedure ReinsertExcept(Node: PAVLNode; NewTree: TAVLTree);
  var
    M: PEmail;
  begin
    if Node = nil then Exit;
    ReinsertExcept(Node^.Left, NewTree);
    M := PEmail(Node^.Data);
    if (M <> nil) and (M^.Id <> DraftID) then
      NewTree.Insert(M);
    ReinsertExcept(Node^.Right, NewTree);
  end;

var
  OldTree, NewTree: TAVLTree;
  Root: PAVLNode;
  SearchKey: TEmail;
begin
  Result := False;
  if (CurrentUser = nil) or (CurrentUser^.Drafts = nil) then Exit;

  SearchKey.Id := DraftID;
  if CurrentUser^.Drafts.Search(@SearchKey) = nil then Exit;

  OldTree := CurrentUser^.Drafts;
  NewTree := TAVLTree.Create(@CompareEmailsByID);
  try
    Root := OldTree.GetRoot;
    ReinsertExcept(Root, NewTree);
    CurrentUser^.Drafts := NewTree;
    Result := True;
  except
    NewTree.Free;
    Result := False;
  end;
end;

function UpdateDraft(CurrentUser: PUser; DraftID: integer;
  const NewRecipient, NewSubject, NewBody: string): boolean;
var
  D: PEmail;
begin
  Result := False;
  if CurrentUser = nil then Exit;

  D := FindDraft(CurrentUser, DraftID);
  if D = nil then Exit;

  D^.Recipient := NewRecipient;
  D^.Subject := NewSubject;
  D^.MessageBody := NewBody;
  Result := True;
end;

function SendDraftMail(CurrentUser: PUser; DraftID: integer): TEmailSendResult;
var
  D: PEmail;
  Delivery: TEmailDeliveryResult;
begin
  Result := esrNotInContacts;

  if CurrentUser = nil then Exit;

  D := FindDraft(CurrentUser, DraftID);
  if D = nil then Exit;

  Result := ValidateEmailRecipient(D^.Recipient, CurrentUser^.Contacts);
  if Result <> esrSuccess then Exit;

  Delivery := DeliverEmailToUser(D^.Recipient, D);
  if Delivery = edrSuccess then
  begin
    RemoveDraft(CurrentUser, DraftID);
    Result := esrSuccess;
  end
  else
    Result := esrNotInContacts;
end;


procedure AddFavorite(CurrentUser: PUser; MailPtr: PEmail);
var
  SearchKey: TEmail;
  Found: Pointer;
begin
  if (CurrentUser = nil) or (MailPtr = nil) then Exit;

  SearchKey.Id := MailPtr^.Id;
  Found := CurrentUser^.Favorites.Search(@SearchKey);
  if Found <> nil then Exit;

  CurrentUser^.Favorites.Insert(MailPtr);
end;


function FindDraft(CurrentUser: PUser; DraftID: integer): PEmail;
var
  SearchKey: TEmail;
  FoundData: Pointer;
begin
  Result := nil;
  if CurrentUser = nil then Exit;

  SearchKey.Id := DraftID;
  FoundData := CurrentUser^.Drafts.Search(@SearchKey);

  if FoundData <> nil then
    Result := PEmail(FoundData);
end;

function FindFavorite(CurrentUser: PUser; FavoriteID: integer): PEmail;
var
  SearchKey: TEmail;
  FoundData: Pointer;
begin
  Result := nil;
  if CurrentUser = nil then Exit;

  SearchKey.Id := FavoriteID;
  FoundData := CurrentUser^.Favorites.Search(@SearchKey);

  if FoundData <> nil then
    Result := PEmail(FoundData);
end;

function RemoveFavorite(CurrentUser: PUser; FavoriteID: integer): boolean;

  procedure ReinsertExcept(Node: PBTreeNode; NewTree: TBTree);
  var
    i: integer;
    M: PEmail;
  begin
    if Node = nil then Exit;

    for i := 1 to Node^.n do
    begin
      ReinsertExcept(Node^.Children[i], NewTree);

      M := PEmail(Node^.Keys[i]);
      if (M <> nil) and (M^.Id <> FavoriteID) then
        NewTree.Insert(M);
    end;

    ReinsertExcept(Node^.Children[Node^.n + 1], NewTree);
  end;

var
  OldTree, NewTree: TBTree;
  Root: PBTreeNode;
  SearchKey: TEmail;
begin
  Result := False;
  if (CurrentUser = nil) or (CurrentUser^.Favorites = nil) then Exit;
  SearchKey.Id := FavoriteID;
  if CurrentUser^.Favorites.Search(@SearchKey) = nil then Exit;

  OldTree := CurrentUser^.Favorites;
  NewTree := TBTree.Create(@CompareEmailsByID);
  try
    Root := OldTree.GetRoot;
    ReinsertExcept(Root, NewTree);
    CurrentUser^.Favorites := NewTree;
    Result := True;
  except
    on E: Exception do
    begin
      NewTree.Free;
      Result := False;
    end;
  end;
end;

function CreateNewEmail(const Sender, Recipient, Subject, MessageBody: string): PEmail;
  overload;
begin
  New(Result);
  Result^.Id := NextEmailId;
  Inc(NextEmailId);
  Result^.Sender := Sender;
  Result^.Recipient := Recipient;
  Result^.Subject := Subject;
  Result^.MessageBody := MessageBody;
  Result^.Date := Now;
  Result^.State := esUnread;
  Result^.Scheduled := False;
end;

function CreateNewEmail(const Sender, Recipient, Subject, MessageBody: string;
  ScheduledDate: TDateTime): PEmail; overload;
begin
  Result := CreateNewEmail(Sender, Recipient, Subject, MessageBody);
  Result^.Date := ScheduledDate;
  Result^.Scheduled := True;
end;


function ValidateEmailRecipient(const RecipientEmail: string;
  var SenderContacts: TCircularLinkedList): TEmailSendResult;
begin
  if Trim(RecipientEmail) = '' then
    Exit(esrEmptyRecipient);

  if not ContactExists(SenderContacts, RecipientEmail) then
    Exit(esrNotInContacts);

  Result := esrSuccess;
end;

function DeliverEmailToUser(const RecipientEmail: string;
  MailPtr: PEmail): TEmailDeliveryResult;
var
  Receiver: PUser;
begin
  Receiver := FindUserByEmail(Users, RecipientEmail);
  if Receiver = nil then
  begin
    Dispose(MailPtr);
    Exit(edrUserNotFound);
  end;

  DoublyLinkedList.InsertLast(Receiver^.Inbox, MailPtr);
  Result := edrSuccess;
end;

function SendEmailToContact(const SenderUser: PUser;
  const RecipientEmail, Subject, MessageBody: string): TEmailSendResult;
var
  MailPtr: PEmail;
  ValidationResult: TEmailSendResult;
  DeliveryResult: TEmailDeliveryResult;
begin
  ValidationResult := ValidateEmailRecipient(RecipientEmail, SenderUser^.Contacts);
  if ValidationResult <> esrSuccess then
    Exit(ValidationResult);

  MailPtr := CreateNewEmail(SenderUser^.Email, RecipientEmail, Subject, MessageBody);

  DeliveryResult := DeliverEmailToUser(RecipientEmail, MailPtr);
  if DeliveryResult = edrUserNotFound then
    Exit(esrNotInContacts);

  Result := esrSuccess;
end;

procedure MarkEmailAsRead(MailPtr: PEmail);
begin
  if MailPtr <> nil then
    MailPtr^.State := esRead;
end;

function CountUnreadEmails(const UserInbox: TDoublyLinkedList): integer;
var
  Node: PDoublyNode;
  Mail: PEmail;
begin
  Result := 0;
  Node := UserInbox.Head;
  while Node <> nil do
  begin
    Mail := PEmail(Node^.Data);
    if (Mail <> nil) and (Mail^.State = esUnread) then
      Inc(Result);
    Node := Node^.Next;
  end;
end;

procedure DeleteEmailFromInbox(var UserInbox: TDoublyLinkedList;
  var UserTrash: TStack; EmailNode: PDoublyNode);
begin
  if EmailNode = nil then Exit;

  Stack.Push(UserTrash, EmailNode^.Data);

  DoublyLinkedList.DeleteNode(UserInbox, EmailNode);
end;

function CompareEmailsBySubject(Node1, Node2: PDoublyNode): integer;
var
  M1, M2: PEmail;
begin
  M1 := PEmail(Node1^.Data);
  M2 := PEmail(Node2^.Data);
  if (M1 = nil) or (M2 = nil) then Exit(0);
  Result := CompareText(M1^.Subject, M2^.Subject);
end;

function CompareEmailsByDate(Node1, Node2: PDoublyNode): integer;
var
  M1, M2: PEmail;
begin
  M1 := PEmail(Node1^.Data);
  M2 := PEmail(Node2^.Data);
  if (M1 = nil) or (M2 = nil) then Exit(0);

  if M1^.Date < M2^.Date then
    Result := -1
  else if M1^.Date > M2^.Date then
    Result := 1
  else
    Result := 0;
end;

function CompareEmailsBySender(Node1, Node2: PDoublyNode): integer;
var
  M1, M2: PEmail;
begin
  M1 := PEmail(Node1^.Data);
  M2 := PEmail(Node2^.Data);
  if (M1 = nil) or (M2 = nil) then Exit(0);
  Result := CompareText(M1^.Sender, M2^.Sender);
end;

function EmailToStr(Data: Pointer): string;
var
  E: PEmail absolute Data;
begin
  if E = nil then Exit('(empty email)');

  Result := Format('Id: %s' + '\n' + 'From: %s' + '\n' + 'State: %s' +
    '\n' + 'Scheduled: %s' + '\n' + 'Subject: %s' + '\n' + 'Date: %s' +
    '\n' + 'Body: %s', [DotEscape(IntToStr(E^.Id)), DotEscape(E^.Sender),
    DotEscape(EmailStateToText(Ord(E^.State))), // esUnread/esRead
    BoolToYesNo(E^.Scheduled), // Yes/No
    DotEscape(E^.Subject), FormatDateTime('yyyy-mm-dd hh:nn', E^.Date),
    DotEscape(DotTrunc(E^.MessageBody, 60))]);
end;

function RunDotToPng(const DotFile, PngFile: string): boolean;
var
  P: TProcess;
begin
  Result := False;
  P := TProcess.Create(nil);
  try
    P.Executable := 'dot';
    P.Parameters.Add('-Tpng');
    P.Parameters.Add(DotFile);
    P.Parameters.Add('-o');
    P.Parameters.Add(PngFile);
    P.Options := [poWaitOnExit];
    P.Execute;
    Result := (P.ExitStatus = 0) and FileExists(PngFile);
  finally
    P.Free;
  end;
end;

function PrettyEmailLabel(E: PEmail): string;
var
  stateTxt: string;
begin
  if E = nil then Exit('N/A');
  Result :=
    'ID: ' + IntToStr(E^.Id) + '\n' + 'Remitente: ' + DotEscape(E^.Sender) +
    '\n' + 'Estado: ' + stateTxt + '\n' + 'Asunto: ' + DotEscape(E^.Subject) +
    '\n' + 'Fecha: ' + FormatDateTime('yyyy-mm-dd', E^.Date) + '\n' +
    'Mensaje: ' + DotEscape(Copy(E^.MessageBody, 1, 60));
end;

procedure AVL_ToDot(Node: PAVLNode; SL: TStrings);
var
  Me: string;
begin
  if Node = nil then Exit;

  Me := Format('node%p', [Node]);
  SL.Add(Format('%s [label="%s", shape=box, style="rounded,filled", ' +
    'fillcolor="#0b1130", color="#4e70ff", penwidth=2, ' +
    'fontname="DejaVu Sans Mono", fontcolor="white"];',
    [Me, PrettyEmailLabel(PEmail(Node^.Data))]));

  if Node^.Left <> nil then
  begin
    SL.Add(Format('%s -> node%p;', [Me, Node^.Left]));
    AVL_ToDot(Node^.Left, SL);
  end;

  if Node^.Right <> nil then
  begin
    SL.Add(Format('%s -> node%p;', [Me, Node^.Right]));
    AVL_ToDot(Node^.Right, SL);
  end;
end;

function GenerateDraftsAVLReport(const U: PUser; const OutDir: string;
  out DotPath, PngPath: string): boolean;
var
  SL: TStringList;
  Root: PAVLNode;
begin
  Result := False;
  DotPath := IncludeTrailingPathDelimiter(OutDir) + 'ReporteBorradores_AVL.dot';
  PngPath := IncludeTrailingPathDelimiter(OutDir) + 'ReporteBorradores_AVL.png';

  if (U = nil) or (U^.Drafts = nil) then Exit;

  SL := TStringList.Create;
  try
    SL.Add('digraph G {');
    SL.Add('  graph [bgcolor="#0b0f1a", labelloc="t", fontsize=34, fontname="Inter", ' +
      ' color="#3a4157", penwidth=4, pad=0.5, splines=true];');
    SL.Add('  node  [shape=box, style="rounded,filled", fillcolor="#0b1130", ' +
      ' fontcolor="white", color="#4e70ff", penwidth=2];');
    SL.Add('  edge  [color="#ccd4ff", penwidth=1.8, arrowsize=0.9];');
    SL.Add('  label="Reporte de Borradores de correos (Árbol AVL)";');

    Root := U^.Drafts.GetRoot;
    if Root = nil then
      SL.Add('empty [label="Sin borradores", shape=plaintext, fontcolor="white"];')
    else
      AVL_ToDot(Root, SL);

    SL.Add('}');
    SL.SaveToFile(DotPath);
  finally
    SL.Free;
  end;

  Result := RunDotToPng(DotPath, PngPath);
end;

function BKeyLabel(Data: Pointer): string;
begin
  Result := PrettyEmailLabel(PEmail(Data));
end;

procedure B_ToDot(Node: PBTreeNode; SL: TStrings);
var
  Me, Lab: string;
  i: integer;
begin
  if Node = nil then Exit;

  Me := Format('node%p', [Node]);
  Lab := '';
  for i := 1 to Node^.n do
    Lab := Lab + Format('<f%d> | %s |', [i - 1, BKeyLabel(Node^.Keys[i])]);
  Lab := Lab + Format('<f%d>', [Node^.n]);

  SL.Add(Format('%s [label="%s", shape=record, style="filled", ' +
    'fillcolor="#3a005a", fontcolor="white", color="#c084fc", ' +
    'penwidth=2, fontname="DejaVu Sans Mono"];', [Me, Lab]));

  if not Node^.leaf then
  begin
    for i := 1 to Node^.n + 1 do
      if Node^.Children[i] <> nil then
      begin
        SL.Add(Format('"%s":f%d -> "node%p";', [Me, i - 1, Node^.Children[i]]));
        B_ToDot(Node^.Children[i], SL);
      end;
  end;
end;

function GenerateFavoritesBTreeReport(const U: PUser; const OutDir: string;
  out DotPath, PngPath: string): boolean;
var
  SL: TStringList;
  Root: PBTreeNode;
begin
  Result := False;
  DotPath := IncludeTrailingPathDelimiter(OutDir) + 'ReporteFavoritos_BTree.dot';
  PngPath := IncludeTrailingPathDelimiter(OutDir) + 'ReporteFavoritos_BTree.png';

  if (U = nil) or (U^.Favorites = nil) then Exit;

  SL := TStringList.Create;
  try
    SL.Add('digraph G {');
    SL.Add('  graph [bgcolor="#0b0f1a", labelloc="t", fontsize=34, fontname="Inter", ' +
      ' color="#3a4157", penwidth=4, pad=0.5];');
    SL.Add('  edge  [color="#e9d5ff", penwidth=1.6, arrowsize=0.9];');
    SL.Add('  node  [shape=record, style="filled", fontcolor="white", ' +
      ' fontname="DejaVu Sans Mono"];');
    SL.Add('  label="Correos Favoritos (Árbol B)";');

    Root := U^.Favorites.GetRoot;
    if Root = nil then
      SL.Add('empty [label="Sin favoritos", shape=plaintext, fontcolor="white"];')
    else
      B_ToDot(Root, SL);

    SL.Add('}');
    SL.SaveToFile(DotPath);
  finally
    SL.Free;
  end;

  Result := RunDotToPng(DotPath, PngPath);
end;

end.
