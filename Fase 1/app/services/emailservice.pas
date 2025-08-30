unit EmailService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, DoublyLinkedList, User, Email, Stack,
  CircularLinkedList, AppState;

type
  TEmailSendResult = (esrSuccess, esrEmptyRecipient, esrNotInContacts);
  TEmailDeliveryResult = (edrSuccess, edrUserNotFound);

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

function CompareEmailsBySubject(Node1, Node2: PDoublyNode): integer;
function CompareEmailsByDate(Node1, Node2: PDoublyNode): integer;
function CompareEmailsBySender(Node1, Node2: PDoublyNode): integer;

function EmailToStr(Data: Pointer): string;

implementation

uses
  UserService, ContactService, DotUtils;

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

end.
