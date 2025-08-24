unit AppState;

{$mode ObjFPC}{$H+}

interface

uses
  SinglyLinkedList, User, SysUtils, Classes;

var
  Users: TSinglyLinkedList;

procedure InitAppState;
function UsersCount: Integer;
function UsersDump: string;
implementation

procedure InitAppState;

begin
  Init(Users);
end;

function UsersCount: Integer;
var
  Node: PSinglyNode;
begin
  Result := 0;
  Node := Users.Head;
  while Node <> nil do
  begin
    Inc(Result);
    Node := Node^.Next;
  end;
end;

function UsersDump: string;
var
  Node: PSinglyNode;
  Curr: PUser;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    SL.Add(Format('Total users: %d', [UsersCount]));
    Node := Users.Head;
    while Node <> nil do
    begin
      Curr := PUser(Node^.Data);
      if Curr <> nil then
        SL.Add(Format('#%d  %s  <%s>  user=%s  tel=%s',
          [Curr^.Id, Curr^.Name, Curr^.Email, Curr^.Username, Curr^.Phone]));
      Node := Node^.Next;
    end;
    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

end.

