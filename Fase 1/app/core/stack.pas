unit Stack;

{$mode ObjFPC}{$H+}

interface

type
  PStackNode = ^TStackNode;

  TStackNode = record
    Data: Pointer;
    Next: PStackNode;
  end;

  TStack = record
    Top: PStackNode;
  end;

procedure Init(var S: TStack);
procedure Push(var S: TStack; Item: Pointer);
function Pop(var S: TStack): Pointer;

implementation

procedure Init(var S: TStack);
begin
  S.Top := nil;
end;

procedure Push(var S: TStack; Item: Pointer);
var
  NewNode: PStackNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := S.Top;
  S.Top := NewNode;
end;

function Pop(var S: TStack): Pointer;
var
  Temp: PStackNode;
begin
  if S.Top = nil then
  begin
    Result := nil;
    Exit;
  end;

  Temp := S.Top;
  Result := Temp^.Data;
  S.Top := Temp^.Next;
  Dispose(Temp);
end;

end.
