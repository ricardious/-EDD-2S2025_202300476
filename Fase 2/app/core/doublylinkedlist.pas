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
    WriteLn(F, '    rankdir=TB;'); // <--- CAMBIO AQUÍ
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
    WriteLn(F, '    edge [');
    WriteLn(F, '        color="#667eea",');
    WriteLn(F, '        penwidth=1.5,');
    WriteLn(F, '        arrowsize=0.8,');
    WriteLn(F, '        arrowhead=vee');
    WriteLn(F, '    ];');
    WriteLn(F, '');

    if IsEmpty(L) then
    begin
      WriteLn(F, '    empty [');
      WriteLn(F, '        label="Lista Vacía",');
      WriteLn(F, '        shape=ellipse,');
      WriteLn(F, '        style="filled,rounded",');
      WriteLn(F, '        fillcolor="#f093fb:#f5576c",');
      WriteLn(F, '        gradientangle=90,');
      WriteLn(F, '        color="#e53e3e",');
      WriteLn(F, '        fontcolor="#FFFFFF",');
      WriteLn(F, '        penwidth=0.8');
      WriteLn(F, '    ];');
    end
    else
    begin
      N := Count(L);

      // Nodos con gradientes alternos profesionales
      Temp := L.Head;
      NodeIndex := 0;
      while Temp <> nil do
      begin
        WriteLn(F, Format('    node%d [', [NodeIndex]));
        WriteLn(F, Format('        label="<prev>•|<data>%s|<next>•",', [DataToString(Temp^.Data)]));
        if NodeIndex mod 2 = 0 then
        begin
          WriteLn(F, '        fillcolor="#667eea:#764ba2",');
          WriteLn(F, '        gradientangle=45');
        end
        else
        begin
          WriteLn(F, '        fillcolor="#4facfe:#00f2fe",');
          WriteLn(F, '        gradientangle=135');
        end;
        WriteLn(F, '    ];');
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');

      // Nodos NULL profesionales
      WriteLn(F, '    null_top ['); // Cambiado de null_left
      WriteLn(F, '        label="∅",');
      WriteLn(F, '        shape=circle,');
      WriteLn(F, '        style="filled",');
      WriteLn(F, '        fillcolor="#a8edea:#fed6e3",');
      WriteLn(F, '        gradientangle=180,');
      WriteLn(F, '        color="#667eea",');
      WriteLn(F, '        fontcolor="#5a67d8",');
      WriteLn(F, '        fontsize=14,');
      WriteLn(F, '        penwidth=0.8,');
      WriteLn(F, '        width=0.5,');
      WriteLn(F, '        height=0.5');
      WriteLn(F, '    ];');
      WriteLn(F, '');
      WriteLn(F, '    null_bottom ['); // Cambiado de null_right
      WriteLn(F, '        label="∅",');
      WriteLn(F, '        shape=circle,');
      WriteLn(F, '        style="filled",');
      WriteLn(F, '        fillcolor="#a8edea:#fed6e3",');
      WriteLn(F, '        gradientangle=180,');
      WriteLn(F, '        color="#667eea",');
      WriteLn(F, '        fontcolor="#5a67d8",');
      WriteLn(F, '        fontsize=14,');
      WriteLn(F, '        penwidth=0.8,');
      WriteLn(F, '        width=0.5,');
      WriteLn(F, '        height=0.5');
      WriteLn(F, '    ];');
      WriteLn(F, '');

      // Conexiones NEXT (hacia adelante, ahora hacia abajo)
      Temp := L.Head;
      NodeIndex := 0;
      while (Temp <> nil) and (Temp^.Next <> nil) do
      begin
        // La conexión sigue siendo la misma, pero la orientación la hará vertical
        WriteLn(F, Format('    node%d:next -> node%d:prev [color="#667eea", constraint=true];',
          [NodeIndex, NodeIndex + 1]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');

      // Conexiones PREV (hacia atrás, ahora hacia arriba) - con estilo diferente
      Temp := L.Tail;
      NodeIndex := N - 1;
      while (Temp <> nil) and (Temp^.Prev <> nil) do
      begin
        WriteLn(F, Format('    node%d:prev -> node%d:next [color="#4facfe", style=dashed, constraint=false];',
          [NodeIndex, NodeIndex - 1]));
        Temp := Temp^.Prev;
        Dec(NodeIndex);
      end;

      WriteLn(F, '');

      // Conexiones a NULL
      if N > 0 then
      begin
        // Conexiones a los nodos nulos superior e inferior
        WriteLn(F, '    node0:prev -> null_top [color="#a8edea"];');
        WriteLn(F, Format('    node%d:next -> null_bottom [color="#a8edea"];', [N - 1]));
      end;
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
