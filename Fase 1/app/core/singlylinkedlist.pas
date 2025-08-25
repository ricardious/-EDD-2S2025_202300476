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
    WriteLn(F, '    node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(F, '');

    if L.Head = nil then
    begin
      WriteLn(F, '    empty [label="Lista Vacía", shape=ellipse, fillcolor=lightgray];');
    end
    else
    begin
      Temp := L.Head;
      NodeIndex := 0;
      while Temp <> nil do
      begin
        WriteLn(F, Format('    node%d [label="<data>%s|<next>"];',
          [NodeIndex, DataToString(Temp^.Data)]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      LastIndex := NodeIndex - 1;
      WriteLn(F, '');

      Temp := L.Head;
      NodeIndex := 0;
      while (Temp <> nil) and (Temp^.Next <> nil) do
      begin
        WriteLn(F, Format('    node%d:next -> node%d:data;',
          [NodeIndex, NodeIndex + 1]));
        Temp := Temp^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '    null [label="NULL", shape=ellipse, fillcolor=lightcoral];');
      WriteLn(F, Format('    node%d:next -> null;', [LastIndex]));
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
