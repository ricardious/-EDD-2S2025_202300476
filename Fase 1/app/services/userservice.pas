unit UserService;

{$mode ObjFPC}{$H+}

interface

uses
  User, SinglyLinkedList, DoublyLinkedList, Stack, Queue, CircularLinkedList;

procedure BootstrapRoot(var L: TSinglyLinkedList);
function AddUser(var L: TSinglyLinkedList; const U: TUser): PUser;
function FindUserByEmail(const L: TSinglyLinkedList; const Email: ansistring): PUser;
function FindUserById(const L: TSinglyLinkedList; const Id: integer): PUser;
function FindUserByUsername(const L: TSinglyLinkedList;
  const Username: ansistring): PUser;

implementation

procedure BootstrapRoot(var L: TSinglyLinkedList);
var
  root: TUser;
begin
  root.Id := 0;
  root.Name := 'Administrator';
  root.Username := 'root';
  root.Password := 'root123';
  root.Email := 'root@edd.com';
  root.Phone := '00000000';
  AddUser(L, root);
end;

function GetMaxUserId(const L: TSinglyLinkedList): integer;
var
  Node: PSinglyNode;
  Curr: PUser;
  MaxId: integer;
begin
  MaxId := -1;
  Node := L.Head;
  while Node <> nil do
  begin
    Curr := PUser(Node^.Data);
    if (Curr <> nil) and (Curr^.Id > MaxId) then
      MaxId := Curr^.Id;
    Node := Node^.Next;
  end;
  Result := MaxId;
end;

function AddUser(var L: TSinglyLinkedList; const U: TUser): PUser;
var
  NewU: PUser;
  NewId: integer;
begin
  New(NewU);
  NewU^ := U;

  if NewU^.Id = -1 then
  begin
    NewId := GetMaxUserId(L) + 1;
    NewU^.Id := NewId;
  end;

  DoublyLinkedList.Init(NewU^.Inbox);
  Stack.Init(NewU^.Trash);
  Queue.Init(NewU^.ScheduledMail);
  CircularLinkedList.Init(NewU^.Contacts);

  SinglyLinkedList.InsertLast(L, Pointer(NewU));
  Result := NewU;
end;

function FindUserByEmail(const L: TSinglyLinkedList; const Email: ansistring): PUser;
var
  Node: PSinglyNode;
  Curr: PUser;
begin
  Node := L.Head;
  while Node <> nil do
  begin
    Curr := PUser(Node^.Data);
    if (Curr <> nil) and (Curr^.Email = Email) then
      exit(Curr);
    Node := Node^.Next;
  end;
  Result := nil;
end;

function FindUserById(const L: TSinglyLinkedList; const Id: integer): PUser;
var
  Node: PSinglyNode;
  Curr: PUser;
begin
  Node := L.Head;
  while Node <> nil do
  begin
    Curr := PUser(Node^.Data);
    if (Curr <> nil) and (Curr^.Id = Id) then
      exit(Curr);
    Node := Node^.Next;
  end;
  Result := nil;
end;

function FindUserByUsername(const L: TSinglyLinkedList;
  const Username: ansistring): PUser;
var
  Node: PSinglyNode;
  Curr: PUser;
begin
  Node := L.Head;
  while Node <> nil do
  begin
    Curr := PUser(Node^.Data);
    if (Curr <> nil) and (Curr^.Username = Username) then
      exit(Curr);
    Node := Node^.Next;
  end;
  Result := nil;
end;

end.
