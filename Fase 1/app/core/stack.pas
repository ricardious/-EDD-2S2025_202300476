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
function IsEmpty(const S: TStack): Boolean;
function Count(const S: TStack): Integer;
function GetItem(const S: TStack; Index: Integer): Pointer;

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

function IsEmpty(const S: TStack): Boolean;
begin
  Result := S.Top = nil;
end;

function Count(const S: TStack): Integer;
var
  Current: PStackNode;
  Counter: Integer;
begin
  Counter := 0;
  Current := S.Top;
  while Current <> nil do
  begin
    Inc(Counter);
    Current := Current^.Next;
  end;
  Result := Counter;
end;

function GetItem(const S: TStack; Index: Integer): Pointer;
var
  Current: PStackNode;
  Counter: Integer;
begin
  Result := nil;

  // Validar índice
  if Index < 0 then
    Exit;

  Current := S.Top;
  Counter := 0;

  // Recorrer hasta llegar al índice deseado
  while (Current <> nil) and (Counter < Index) do
  begin
    Current := Current^.Next;
    Inc(Counter);
  end;

  // Si encontramos el nodo, devolver su dato
  if Current <> nil then
    Result := Current^.Data;
end;

end.
