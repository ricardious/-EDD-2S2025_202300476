unit CircularLinkedList;

{$mode ObjFPC}{$H+}

interface

type
  PCircularNode = ^TCircularNode;

  TCircularNode = record
    Data: Pointer;
    Next: PCircularNode;
    Prev: PCircularNode;
  end;

  TCircularLinkedList = record
    Head: PCircularNode;
  end;

  TDataToString = function(Data: Pointer): string;

procedure Init(var L: TCircularLinkedList);
procedure Insert(var L: TCircularLinkedList; Item: Pointer);
procedure InsertFirst(var L: TCircularLinkedList; Item: Pointer);
procedure InsertAfter(var L: TCircularLinkedList; Node: PCircularNode; Item: Pointer);
function Find(var L: TCircularLinkedList; Match: Pointer): PCircularNode;
procedure Delete(var L: TCircularLinkedList; Item: Pointer);
procedure DeleteNode(var L: TCircularLinkedList; Node: PCircularNode);
function Count(var L: TCircularLinkedList): integer;
procedure Clear(var L: TCircularLinkedList);
function IsEmpty(var L: TCircularLinkedList): boolean;
function GetTail(var L: TCircularLinkedList): PCircularNode;
procedure GenerateDotFile(var L: TCircularLinkedList; const FileName: string;
  DataToString: TDataToString);

implementation

uses
  SysUtils;

procedure Init(var L: TCircularLinkedList);
begin
  L.Head := nil;
end;

procedure Insert(var L: TCircularLinkedList; Item: Pointer);
var
  NewNode, Tail: PCircularNode;
begin
  New(NewNode);
  NewNode^.Data := Item;

  if L.Head = nil then
  begin
    L.Head := NewNode;
    NewNode^.Next := NewNode;
    NewNode^.Prev := NewNode;
  end
  else
  begin
    Tail := L.Head^.Prev;

    NewNode^.Next := L.Head;
    NewNode^.Prev := Tail;

    Tail^.Next := NewNode;
    L.Head^.Prev := NewNode;
  end;
end;

procedure InsertFirst(var L: TCircularLinkedList; Item: Pointer);
var
  NewNode, Tail: PCircularNode;
begin
  New(NewNode);
  NewNode^.Data := Item;

  if L.Head = nil then
  begin
    L.Head := NewNode;
    NewNode^.Next := NewNode;
    NewNode^.Prev := NewNode;
  end
  else
  begin
    Tail := L.Head^.Prev;

    NewNode^.Next := L.Head;
    NewNode^.Prev := Tail;

    L.Head^.Prev := NewNode;
    Tail^.Next := NewNode;

    L.Head := NewNode;
  end;
end;

procedure InsertAfter(var L: TCircularLinkedList; Node: PCircularNode; Item: Pointer);
var
  NewNode: PCircularNode;
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

  Node^.Next^.Prev := NewNode;
  Node^.Next := NewNode;
end;

function Find(var L: TCircularLinkedList; Match: Pointer): PCircularNode;
var
  Current: PCircularNode;
begin
  Result := nil;
  if L.Head = nil then Exit;

  Current := L.Head;
  repeat
    if Current^.Data = Match then
    begin
      Result := Current;
      Exit;
    end;
    Current := Current^.Next;
  until Current = L.Head;
end;

function GetTail(var L: TCircularLinkedList): PCircularNode;
begin
  if L.Head = nil then
    Result := nil
  else
    Result := L.Head^.Prev;
end;

procedure DeleteNode(var L: TCircularLinkedList; Node: PCircularNode);
begin
  if (L.Head = nil) or (Node = nil) then Exit;

  if Node^.Next = Node then
  begin
    Dispose(Node);
    L.Head := nil;
    Exit;
  end;

  Node^.Prev^.Next := Node^.Next;
  Node^.Next^.Prev := Node^.Prev;

  if L.Head = Node then
    L.Head := Node^.Next;

  Dispose(Node);
end;

procedure Delete(var L: TCircularLinkedList; Item: Pointer);
var
  NodeToDelete: PCircularNode;
begin
  NodeToDelete := Find(L, Item);
  if NodeToDelete <> nil then
    DeleteNode(L, NodeToDelete);
end;

function Count(var L: TCircularLinkedList): integer;
var
  Current: PCircularNode;
  Counter: integer;
begin
  Counter := 0;
  if L.Head = nil then
  begin
    Result := 0;
    Exit;
  end;

  Current := L.Head;
  repeat
    Inc(Counter);
    Current := Current^.Next;
  until Current = L.Head;

  Result := Counter;
end;

function IsEmpty(var L: TCircularLinkedList): boolean;
begin
  Result := L.Head = nil;
end;

procedure Clear(var L: TCircularLinkedList);
var
  Current, Next: PCircularNode;
  NodeCount, i: integer;
begin
  if L.Head = nil then Exit;

  NodeCount := Count(L);
  Current := L.Head;

  for i := 1 to NodeCount do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;

  L.Head := nil;
end;

procedure GenerateDotFile(var L: TCircularLinkedList; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Current: PCircularNode;
  NodeIndex, TotalNodes: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);

  try
    WriteLn(F, 'digraph CircularDoublyLinkedList {');
    WriteLn(F, '    rankdir=LR;');
    WriteLn(F, '    node [shape=record, style=filled, fillcolor=lightcyan];');
    WriteLn(F, '    edge [color=blue];');
    WriteLn(F, '');

    if IsEmpty(L) then
    begin
      WriteLn(F, '    empty [label="Lista Vacía", shape=ellipse, fillcolor=lightgray];');
    end
    else
    begin
      TotalNodes := Count(L);

      WriteLn(F, '    head [label="HEAD", shape=ellipse, fillcolor=yellow];');
      WriteLn(F, '');

      Current := L.Head;
      NodeIndex := 0;
      repeat
        if Current = L.Head then
          WriteLn(F, Format(
            '    node%d [label="<prev>|<data>%s|<next>", fillcolor=lightyellow];',
            [NodeIndex, DataToString(Current^.Data)]))
        else
          WriteLn(F, Format('    node%d [label="<prev>|<data>%s|<next>"];',
            [NodeIndex, DataToString(Current^.Data)]));
        Current := Current^.Next;
        Inc(NodeIndex);
      until Current = L.Head;

      WriteLn(F, '');

      Current := L.Head;
      NodeIndex := 0;
      repeat
        if Current^.Next = L.Head then
          WriteLn(F, Format(
            '    node%d:next -> node0:data [color=red, style=bold, label="next"];',
            [NodeIndex]))
        else
          WriteLn(F, Format('    node%d:next -> node%d:data [color=blue, label="next"];',
            [NodeIndex, NodeIndex + 1]));

        Current := Current^.Next;
        Inc(NodeIndex);
      until Current = L.Head;

      WriteLn(F, '');

      Current := L.Head;
      NodeIndex := 0;
      repeat
        if Current^.Prev = GetTail(L) then
        begin
          if NodeIndex = 0 then
            WriteLn(F, Format(
              '    node0:prev -> node%d:data [color=darkgreen, style=bold, label="prev"];',
              [TotalNodes - 1]));
        end
        else
          WriteLn(F, Format(
            '    node%d:prev -> node%d:data [color=green, label="prev"];',
            [NodeIndex, NodeIndex - 1]));

        Current := Current^.Next;
        Inc(NodeIndex);
      until Current = L.Head;

      WriteLn(F, '');
      WriteLn(F, '    head -> node0:data [color=darkgreen, style=bold];');

      WriteLn(F, '');
      WriteLn(F, '    // Configuración para layout circular');
      if TotalNodes <= 4 then
      begin
        WriteLn(F, '    {rank=same; ');
        for NodeIndex := 0 to TotalNodes - 1 do
        begin
          if NodeIndex = TotalNodes - 1 then
            WriteLn(F, Format('node%d;}', [NodeIndex]))
          else
            Write(F, Format('node%d; ', [NodeIndex]));
        end;
      end;

      WriteLn(F, '');
      WriteLn(F, '    // Flechas rojas: conexiones circulares Next');
      WriteLn(F, '    // Flechas verdes: conexiones Prev');
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
