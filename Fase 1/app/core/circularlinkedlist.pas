unit CircularLinkedList;

{$mode ObjFPC}{$H+}

interface

type
  PCircularNode = ^TCircularNode;
  TCircularNode = record
    Data: Pointer;
    Next: PCircularNode;
  end;

  TCircularLinkedList = record
    Head: PCircularNode;
  end;

procedure Init(var L: TCircularLinkedList);
procedure Insert(var L: TCircularLinkedList; Item: Pointer);
procedure InsertFirst(var L: TCircularLinkedList; Item: Pointer);
procedure InsertAfter(var L: TCircularLinkedList; Node: PCircularNode; Item: Pointer);
function Find(var L: TCircularLinkedList; Match: Pointer): PCircularNode;
procedure Delete(var L: TCircularLinkedList; Item: Pointer);
procedure DeleteNode(var L: TCircularLinkedList; Node: PCircularNode);
function Count(var L: TCircularLinkedList): Integer;
procedure Clear(var L: TCircularLinkedList);
function IsEmpty(var L: TCircularLinkedList): Boolean;
function GetTail(var L: TCircularLinkedList): PCircularNode;
procedure GenerateDotFile(var L: TCircularLinkedList; const FileName: string; DataToString: function(Data: Pointer): string);

implementation

uses
  SysUtils;

procedure Init(var L: TCircularLinkedList);
begin
  L.Head := nil;
end;

procedure Insert(var L: TCircularLinkedList; Item: Pointer);
var
  NewNode, Temp: PCircularNode;
begin
  New(NewNode);
  NewNode^.Data := Item;

  if L.Head = nil then
  begin
    L.Head := NewNode;
    NewNode^.Next := L.Head;
  end
  else
  begin
    Temp := L.Head;
    while Temp^.Next <> L.Head do
      Temp := Temp^.Next;
    Temp^.Next := NewNode;
    NewNode^.Next := L.Head;
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
    NewNode^.Next := L.Head;
  end
  else
  begin
    Tail := GetTail(L);
    NewNode^.Next := L.Head;
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
var
  Current: PCircularNode;
begin
  Result := nil;
  if L.Head = nil then Exit;

  Current := L.Head;
  while Current^.Next <> L.Head do
    Current := Current^.Next;
  Result := Current;
end;

procedure DeleteNode(var L: TCircularLinkedList; Node: PCircularNode);
var
  Current, Prev: PCircularNode;
begin
  if (L.Head = nil) or (Node = nil) then Exit;

  if L.Head^.Next = L.Head then
  begin
    if L.Head = Node then
    begin
      Dispose(L.Head);
      L.Head := nil;
    end;
    Exit;
  end;

  if L.Head = Node then
  begin
    Current := L.Head;
    while Current^.Next <> L.Head do
      Current := Current^.Next;
    Current^.Next := L.Head^.Next;
    L.Head := L.Head^.Next;
    Dispose(Node);
    Exit;
  end;

  Prev := L.Head;
  Current := L.Head^.Next;

  repeat
    if Current = Node then
    begin
      Prev^.Next := Current^.Next;
      Dispose(Current);
      Exit;
    end;
    Prev := Current;
    Current := Current^.Next;
  until Current = L.Head;
end;

procedure Delete(var L: TCircularLinkedList; Item: Pointer);
var
  NodeToDelete: PCircularNode;
begin
  NodeToDelete := Find(L, Item);
  if NodeToDelete <> nil then
    DeleteNode(L, NodeToDelete);
end;

function Count(var L: TCircularLinkedList): Integer;
var
  Current: PCircularNode;
  Counter: Integer;
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

function IsEmpty(var L: TCircularLinkedList): Boolean;
begin
  Result := L.Head = nil;
end;

procedure Clear(var L: TCircularLinkedList);
var
  Current, Next: PCircularNode;
  NodeCount, i: Integer;
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

procedure GenerateDotFile(var L: TCircularLinkedList; const FileName: string; DataToString: function(Data: Pointer): string);
var
  F: TextFile;
  Current: PCircularNode;
  NodeIndex, TotalNodes: Integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);

  try
    WriteLn(F, 'digraph CircularLinkedList {');
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
          WriteLn(F, Format('    node%d [label="<data>%s|<next>", fillcolor=lightyellow];',
                   [NodeIndex, DataToString(Current^.Data)]))
        else
          WriteLn(F, Format('    node%d [label="<data>%s|<next>"];',
                   [NodeIndex, DataToString(Current^.Data)]));
        Current := Current^.Next;
        Inc(NodeIndex);
      until Current = L.Head;

      WriteLn(F, '');

      Current := L.Head;
      NodeIndex := 0;
      repeat
        if Current^.Next = L.Head then
          WriteLn(F, Format('    node%d:next -> node0:data [color=red, style=bold, label="circular"];',
                   [NodeIndex]))
        else
          WriteLn(F, Format('    node%d:next -> node%d:data [color=blue];',
                   [NodeIndex, NodeIndex + 1]));

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
      WriteLn(F, '    // Sugerencia: la flecha roja muestra la conexión circular');
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
