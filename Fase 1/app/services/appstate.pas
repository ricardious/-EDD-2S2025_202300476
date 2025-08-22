unit AppState;

{$mode ObjFPC}{$H+}

interface

uses
  SinglyLinkedList;

var
  Users: TSinglyLinkedList;

procedure InitAppState;

implementation

procedure InitAppState;
begin
  Init(Users);
end;

end.

