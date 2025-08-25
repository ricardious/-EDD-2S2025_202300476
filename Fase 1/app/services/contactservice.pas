unit ContactService;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, User, UserService, SinglyLinkedList, CircularLinkedList;

function AddContactToUser(var CurrentUser: TUser;
  const GlobalUserList: TSinglyLinkedList; const Email: string): integer;

implementation

function FindContactByEmail(var L: TCircularLinkedList;
  const Email: string): PCircularNode;
var
  Current: PCircularNode;
  User: PUser;
begin
  Result := nil;
  if L.Head = nil then Exit;

  Current := L.Head;
  repeat
    User := PUser(Current^.Data);  // Cast to PUser
    if SameText(User^.Email, Email) then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  until Current = L.Head;
end;

function AddContactToUser(var CurrentUser: TUser;
  const GlobalUserList: TSinglyLinkedList; const Email: string): integer;
var
  P: PUser;
  NormalizedEmail: string;
begin
  NormalizedEmail := LowerCase(Trim(Email));
  if NormalizedEmail = '' then Exit(-1);

  P := FindUserByEmail(GlobalUserList, NormalizedEmail);
  if P = nil then Exit(-2);

  if FindContactByEmail(CurrentUser.Contacts, NormalizedEmail) <> nil then
    Exit(-3);

  Insert(CurrentUser.Contacts, P);
  Result := 0;
end;


end.
