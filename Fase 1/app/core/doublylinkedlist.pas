unit DoublyLinkedList;

{$mode ObjFPC}{$H+}


interface



type
  PDoublyNode = ^TDoublyNode;

  TDoublyNode = record
    Data: Pointer;
    Prev, Next: PDoublyNode;
  end;

  TDoublyLinkedList = record
    Head, Tail: PDoublyNode;
  end;

  TDataToString = function(Data: Pointer): string;

  TCompareFunc = function(Node1, Node2: PDoublyNode): integer;

procedure Init(var L: TDoublyLinkedList);
procedure InsertLast(var L: TDoublyLinkedList; Item: Pointer);
procedure InsertFirst(var L: TDoublyLinkedList; Item: Pointer);
procedure InsertAfter(var L: TDoublyLinkedList; Node: PDoublyNode; Item: Pointer);
procedure InsertBefore(var L: TDoublyLinkedList; Node: PDoublyNode; Item: Pointer);
function Find(var L: TDoublyLinkedList; Match: Pointer): PDoublyNode;
procedure Delete(var L: TDoublyLinkedList; Item: Pointer);
procedure DeleteNode(var L: TDoublyLinkedList; Node: PDoublyNode);
function Count(var L: TDoublyLinkedList): integer;
procedure Clear(var L: TDoublyLinkedList);
function IsEmpty(var L: TDoublyLinkedList): boolean;
procedure GenerateDotFile(var L: TDoublyLinkedList; const FileName: string;
  DataToString: TDataToString);
procedure Sort(CompareFunc: TCompareFunc; var List: TDoublyLinkedList);

implementation

uses
  SysUtils;

procedure Init(var L: TDoublyLinkedList);
begin
  L.Head := nil;
  L.Tail := nil;
end;

procedure InsertLast(var L: TDoublyLinkedList; Item: Pointer);
var
  NewNode: PDoublyNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := nil;
  NewNode^.Prev := L.Tail;

  if L.Tail <> nil then
    L.Tail^.Next := NewNode;

  L.Tail := NewNode;

  if L.Head = nil then
    L.Head := NewNode;
end;

procedure InsertFirst(var L: TDoublyLinkedList; Item: Pointer);
var
  NewNode: PDoublyNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Prev := nil;
  NewNode^.Next := L.Head;

  if L.Head <> nil then
    L.Head^.Prev := NewNode;

  L.Head := NewNode;

  if L.Tail = nil then
    L.Tail := NewNode;
end;

procedure InsertAfter(var L: TDoublyLinkedList; Node: PDoublyNode; Item: Pointer);
var
  NewNode: PDoublyNode;
begin
  if Node = nil then
  begin
    InsertFirst(L, Item);
    Exit;
  end;

  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := Node^.Next;
  NewNode^.Prev := Node;

  if Node^.Next <> nil then
    Node^.Next^.Prev := NewNode
  else
    L.Tail := NewNode;

  Node^.Next := NewNode;
end;

procedure InsertBefore(var L: TDoublyLinkedList; Node: PDoublyNode; Item: Pointer);
var
  NewNode: PDoublyNode;
begin
  if Node = nil then
  begin
    InsertLast(L, Item);
    Exit;
  end;

  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := Node;
  NewNode^.Prev := Node^.Prev;

  if Node^.Prev <> nil then
    Node^.Prev^.Next := NewNode
  else
    L.Head := NewNode;

  Node^.Prev := NewNode;
end;

function Find(var L: TDoublyLinkedList; Match: Pointer): PDoublyNode;
var
  Temp: PDoublyNode;
begin
  Temp := L.Head;
  while Temp <> nil do
  begin
    if Temp^.Data = Match then
    begin
      Result := Temp;
      Exit;
    end;
    Temp := Temp^.Next;
  end;
  Result := nil;
end;

procedure DeleteNode(var L: TDoublyLinkedList; Node: PDoublyNode);
begin
  if Node = nil then Exit;

  if Node^.Prev <> nil then
    Node^.Prev^.Next := Node^.Next
  else
    L.Head := Node^.Next;

  if Node^.Next <> nil then
    Node^.Next^.Prev := Node^.Prev
  else
    L.Tail := Node^.Prev;

  Dispose(Node);
end;

procedure Delete(var L: TDoublyLinkedList; Item: Pointer);
var
  NodeToDelete: PDoublyNode;
begin
  NodeToDelete := Find(L, Item);
  if NodeToDelete <> nil then
    DeleteNode(L, NodeToDelete);
end;

function Count(var L: TDoublyLinkedList): integer;
var
  Temp: PDoublyNode;
  Counter: integer;
begin
  Counter := 0;
  Temp := L.Head;
  while Temp <> nil do
  begin
    Inc(Counter);
    Temp := Temp^.Next;
  end;
  Result := Counter;
end;

function IsEmpty(var L: TDoublyLinkedList): boolean;
begin
  Result := (L.Head = nil) and (L.Tail = nil);
end;

procedure Clear(var L: TDoublyLinkedList);
var
  Temp, Next: PDoublyNode;
begin
  Temp := L.Head;
  while Temp <> nil do
  begin
    Next := Temp^.Next;
    Dispose(Temp);
    Temp := Next;
  end;
  L.Head := nil;
  L.Tail := nil;
end;

procedure GenerateDotFile(var L: TDoublyLinkedList; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Temp: PDoublyNode;
  NodeIndex, N: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'digraph DoublyLinkedList {');
    WriteLn(F, '    rankdir=LR;');
    WriteLn(F, '    node [shape=record, style=filled, fillcolor=lightgreen];');
    WriteLn(F, '    edge [color=blue];');
    WriteLn(F, '');

    if IsEmpty(L) then
    begin
      WriteLn(F, '    empty [label="Empty list", shape=ellipse, fillcolor=lightgray];');
    end
    else
    begin
      WriteLn(F, '    head [label="HEAD", shape=ellipse, fillcolor=yellow];');
      WriteLn(F, '    tail [label="TAIL", shape=ellipse, fillcolor=orange];');
      WriteLn(F, '');

      // Nodes
      Temp := L.Head;
      NodeIndex := 0;
      while Temp <> nil do
      begin
        WriteLn(F, Format('    node%d [label="<prev>|<data>%s|<next>"];',
          [NodeIndex, DataToString(Temp^.Data)]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');
      WriteLn(F, '    null_left [label="NULL", shape=ellipse, fillcolor=lightcoral];');
      WriteLn(F, '    null_right [label="NULL", shape=ellipse, fillcolor=lightcoral];');
      WriteLn(F, '');

      // Next edges (left -> right)
      Temp := L.Head;
      NodeIndex := 0;
      while (Temp <> nil) and (Temp^.Next <> nil) do
      begin
        WriteLn(F, Format('    node%d:next -> node%d:data [color=blue, label="next"];',
          [NodeIndex, NodeIndex + 1]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');

      // Prev edges (right -> left)
      Temp := L.Tail;
      N := Count(L);           // cache the count
      NodeIndex := N - 1;
      while (Temp <> nil) and (Temp^.Prev <> nil) do
      begin
        WriteLn(F, Format('    node%d:prev -> node%d:data [color=red, label="prev"];',
          [NodeIndex, NodeIndex - 1]));
        Temp := Temp^.Prev;
        Dec(NodeIndex);
      end;

      WriteLn(F, '');

      if N > 0 then
      begin
        // These two lines do NOT need Format
        WriteLn(F, '    node0:prev -> null_left [color=red];');
        WriteLn(F, Format('    node%d:next -> null_right [color=blue];', [N - 1]));
      end;

      WriteLn(F, '');

      WriteLn(F, '    head -> node0:data [color=darkgreen, style=bold];');
      WriteLn(F, Format('    tail -> node%d:data [color=darkorange, style=bold];',
        [N - 1]));

      WriteLn(F, '');
      WriteLn(F, '    // Group elements for better visualization');
      WriteLn(F, '    {rank=same; head; node0;}');
      WriteLn(F, Format('    {rank=same; tail; node%d;}', [N - 1]));
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

procedure Sort(CompareFunc: TCompareFunc; var List: TDoublyLinkedList);
var
  Changed: boolean;
  Current, Next: PDoublyNode;
  TempData: Pointer;
begin
  if (List.Head = nil) or (List.Head^.Next = nil) then Exit;

  repeat
    Changed := False;
    Current := List.Head;

    while (Current <> nil) and (Current^.Next <> nil) do
    begin
      Next := Current^.Next;

      if CompareFunc(Current, Next) > 0 then
      begin
        TempData := Current^.Data;
        Current^.Data := Next^.Data;
        Next^.Data := TempData;
        Changed := True;
      end;

      Current := Current^.Next;
    end;
  until not Changed;
end;

end.
