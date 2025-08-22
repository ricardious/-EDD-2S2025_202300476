unit UserService;

{$mode ObjFPC}{$H+}

interface

uses
  User, SinglyLinkedList;

procedure BootstrapRoot(var L: TSinglyLinkedList);
function  AddUser(var L: TSinglyLinkedList; const U: TUser): PUser;
function  FindUserByEmail(const L: TSinglyLinkedList; const Email: AnsiString): PUser;

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

function AddUser(var L: TSinglyLinkedList; const U: TUser): PUser;
var
  NewU: PUser;
begin
  New(NewU);
  NewU^ := U;
  InsertLast(L, Pointer(NewU));
  Result := NewU;
end;

function FindUserByEmail(const L: TSinglyLinkedList; const Email: AnsiString): PUser;
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

end.

