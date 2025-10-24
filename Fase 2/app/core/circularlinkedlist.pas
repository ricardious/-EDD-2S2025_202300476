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
  Temp: PCircularNode;
  NodeIndex, N: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'digraph CircularDoublyLinkedList {');
    WriteLn(F, '    rankdir=LR;');
    WriteLn(F, '    splines=ortho;');
    WriteLn(F, '    nodesep=0.8;');
    WriteLn(F, '    bgcolor=transparent;');
    WriteLn(F, '    node [');
    WriteLn(F, '        shape=record,');
    WriteLn(F, '        style="filled,rounded",');
    WriteLn(F, '        fillcolor="#667eea:#764ba2",');
    WriteLn(F, '        gradientangle=45,');
    WriteLn(F, '        color="#5a67d8",');
    WriteLn(F, '        penwidth=0.8,');
    WriteLn(F, '        fontname="Segoe UI",');
    WriteLn(F, '        fontsize=12,');
    WriteLn(F, '        fontcolor="#FFFFFF",');
    WriteLn(F, '        margin=0.2');
    WriteLn(F, '    ];');
    WriteLn(F, '');

    if IsEmpty(L) then
    begin
      WriteLn(F, '    empty [');
      WriteLn(F, '        label="Lista Vacía",');
      WriteLn(F, '        shape=ellipse,');
      WriteLn(F, '        fillcolor="#f093fb:#f5576c",');
      WriteLn(F, '        gradientangle=90,');
      WriteLn(F, '        color="#e53e3e"');
      WriteLn(F, '    ];');
    end
    else
    begin
      N := Count(L);

      Temp := L.Head;
      NodeIndex := 0;
      repeat
        WriteLn(F, Format('    node%d [label="<prev>•|<data>%s|<next>•"];',
          [NodeIndex, DataToString(Temp^.Data)]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      until Temp = L.Head;
      WriteLn(F, '');

      if N = 1 then
      begin
        WriteLn(F, '    // Conexiones para un solo nodo circular');
        WriteLn(F, '    edge [color="#667eea", penwidth=2, arrowsize=1, arrowhead=vee];');
        WriteLn(F, '    node0:next -> node0:data [');
        WriteLn(F, '        tailport=w,');
        WriteLn(F, '        headport=e,');
        WriteLn(F, '        constraint=false,');
        WriteLn(F, '        minlen=2,');
        WriteLn(F, '        label="next",');
        WriteLn(F, '        fontsize=10,');
        WriteLn(F, '        fontcolor="#667eea",');
        WriteLn(F, '        labeldistance=2,');
        WriteLn(F, '        labelangle=45');
        WriteLn(F, '    ];');
        WriteLn(F, '');
        WriteLn(F,
          '    edge [color="#4facfe", style=dashed, penwidth=2, arrowsize=1, arrowhead=normal];');
        WriteLn(F, '    node0:prev -> node0:data [');
        WriteLn(F, '        tailport=w,');
        WriteLn(F, '        headport=e,');
        WriteLn(F, '        constraint=false,');
        WriteLn(F, '        minlen=2,');
        WriteLn(F, '        label="prev",');
        WriteLn(F, '        fontsize=10,');
        WriteLn(F, '        fontcolor="#4facfe",');
        WriteLn(F, '        labeldistance=2,');
        WriteLn(F, '        labelangle=-45');
        WriteLn(F, '    ];');
      end
      else
      begin
        WriteLn(F, '    edge [color="#667eea", penwidth=1.5, arrowsize=0.8, arrowhead=vee];');
        for NodeIndex := 0 to N - 2 do
        begin
          WriteLn(F, Format('    node%d:next -> node%d:prev [constraint=true];',
            [NodeIndex, NodeIndex + 1]));
        end;

        WriteLn(F, Format(
          '    node%d:next -> node0:prev [constraint=false, tailport=n, headport=e];', [N - 1]));
        WriteLn(F, '');

        WriteLn(F,
          '    edge [color="#4facfe", style=dashed, penwidth=1.2, arrowsize=0.7, arrowhead=normal];');
        for NodeIndex := 1 to N - 1 do
        begin
          WriteLn(F, Format('    node%d:prev -> node%d:next [constraint=true];',
            [NodeIndex, NodeIndex - 1]));
        end;

        WriteLn(F, Format(
          '    node0:prev -> node%d:next [constraint=false, tailport=s, headport=w];', [N - 1]));
      end;
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
