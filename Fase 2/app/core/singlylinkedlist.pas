unit SinglyLinkedList;

{$mode ObjFPC}{$H+}

interface

type
  PSinglyNode = ^TSinglyNode;

  TSinglyNode = record
    Data: Pointer;          // generic pointer to any record
    Next: PSinglyNode;
  end;

  TSinglyLinkedList = record
    Head: PSinglyNode;
  end;

  TDataToString = function(Data: Pointer): string;

procedure Init(var L: TSinglyLinkedList);
procedure InsertLast(var L: TSinglyLinkedList; Item: Pointer);
procedure InsertFirst(var L: TSinglyLinkedList; Item: Pointer);
function Find(var L: TSinglyLinkedList; Match: Pointer): PSinglyNode;
procedure Delete(var L: TSinglyLinkedList; Item: Pointer);
function Count(var L: TSinglyLinkedList): integer;
procedure Clear(var L: TSinglyLinkedList);
procedure GenerateDotFile(var L: TSinglyLinkedList; const FileName: string;
  DataToString: TDataToString);

implementation

uses
  SysUtils;

procedure Init(var L: TSinglyLinkedList);
begin
  L.Head := nil;
end;

procedure InsertLast(var L: TSinglyLinkedList; Item: Pointer);
var
  NewNode, Temp: PSinglyNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := nil;

  if L.Head = nil then
    L.Head := NewNode
  else
  begin
    Temp := L.Head;
    while Temp^.Next <> nil do
      Temp := Temp^.Next;
    Temp^.Next := NewNode;
  end;
end;

procedure InsertFirst(var L: TSinglyLinkedList; Item: Pointer);
var
  NewNode: PSinglyNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := L.Head;
  L.Head := NewNode;
end;

function Find(var L: TSinglyLinkedList; Match: Pointer): PSinglyNode;
var
  Temp: PSinglyNode;
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

procedure Delete(var L: TSinglyLinkedList; Item: Pointer);
var
  Temp, Prev: PSinglyNode;
begin
  if L.Head = nil then Exit;

  if L.Head^.Data = Item then
  begin
    Temp := L.Head;
    L.Head := L.Head^.Next;
    Dispose(Temp);
    Exit;
  end;

  Prev := L.Head;
  Temp := L.Head^.Next;

  while Temp <> nil do
  begin
    if Temp^.Data = Item then
    begin
      Prev^.Next := Temp^.Next;
      Dispose(Temp);
      Exit;
    end;
    Prev := Temp;
    Temp := Temp^.Next;
  end;
end;

function Count(var L: TSinglyLinkedList): integer;
var
  Temp: PSinglyNode;
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

procedure Clear(var L: TSinglyLinkedList);
var
  Temp, Next: PSinglyNode;
begin
  Temp := L.Head;
  while Temp <> nil do
  begin
    Next := Temp^.Next;
    Dispose(Temp);
    Temp := Next;
  end;
  L.Head := nil;
end;

procedure GenerateDotFile(var L: TSinglyLinkedList; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Temp: PSinglyNode;
  NodeIndex, LastIndex: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'digraph LinkedList {');
    WriteLn(F, '    rankdir=LR;');
    WriteLn(F, '    bgcolor=transparent;');
    WriteLn(F, '    node [');
    WriteLn(F, '        shape=box,');
    WriteLn(F, '        style="filled,rounded",');
    WriteLn(F, '        fillcolor="#667eea:#764ba2",');
    WriteLn(F, '        gradientangle=45,');
    WriteLn(F, '        color="#5a67d8",');
    WriteLn(F, '        penwidth=0.8,');
    WriteLn(F, '        fontname="Segoe UI",');
    WriteLn(F, '        fontsize=14,');
    WriteLn(F, '        fontcolor="#FFFFFF",');
    WriteLn(F, '        margin=0.3');
    WriteLn(F, '    ];');
    WriteLn(F, '    edge [');
    WriteLn(F, '        color="#667eea",');
    WriteLn(F, '        penwidth=2,');
    WriteLn(F, '        arrowsize=1,');
    WriteLn(F, '        arrowhead=vee');
    WriteLn(F, '    ];');
    WriteLn(F, '');

    if L.Head = nil then
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
      Temp := L.Head;
      NodeIndex := 0;
      while Temp <> nil do
      begin
        WriteLn(F, Format('    node%d [', [NodeIndex]));
        WriteLn(F, Format('        label="%s",', [DataToString(Temp^.Data)]));
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

      LastIndex := NodeIndex - 1;
      WriteLn(F, '');

      Temp := L.Head;
      NodeIndex := 0;
      while (Temp <> nil) and (Temp^.Next <> nil) do
      begin
        WriteLn(F, Format('    node%d -> node%d;', [NodeIndex, NodeIndex + 1]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '    null [');
      WriteLn(F, '        label="∅",');
      WriteLn(F, '        shape=circle,');
      WriteLn(F, '        style="filled",');
      WriteLn(F, '        fillcolor="#a8edea:#fed6e3",');
      WriteLn(F, '        gradientangle=180,');
      WriteLn(F, '        color="#667eea",');
      WriteLn(F, '        fontcolor="#5a67d8",');
      WriteLn(F, '        fontsize=18,');
      WriteLn(F, '        penwidth=0.8,');
      WriteLn(F, '        width=0.6,');
      WriteLn(F, '        height=0.6');
      WriteLn(F, '    ];');
      WriteLn(F, Format('    node%d -> null;', [LastIndex]));
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
