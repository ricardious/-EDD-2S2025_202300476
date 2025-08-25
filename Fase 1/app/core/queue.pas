unit Queue;

{$mode ObjFPC}{$H+}

interface

type
  PQueueNode = ^TQueueNode;

  TQueueNode = record
    Data: Pointer;
    Next: PQueueNode;
  end;

  TQueue = record
    Front, Rear: PQueueNode;
  end;

  TDataToString = function(Data: Pointer): string;

procedure Init(var Q: TQueue);
procedure Enqueue(var Q: TQueue; Item: Pointer);
function Dequeue(var Q: TQueue): Pointer;
function Peek(var Q: TQueue): Pointer;
function IsEmpty(var Q: TQueue): boolean;
function Count(var Q: TQueue): integer;
procedure Clear(var Q: TQueue);
function GetRear(var Q: TQueue): Pointer;
procedure GenerateDotFile(var Q: TQueue; const FileName: string;
  DataToString: TDataToString);

implementation

uses
  SysUtils;

procedure Init(var Q: TQueue);
begin
  Q.Front := nil;
  Q.Rear := nil;
end;

procedure Enqueue(var Q: TQueue; Item: Pointer);
var
  NewNode: PQueueNode;
begin
  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := nil;

  if Q.Rear = nil then
    Q.Front := NewNode
  else
    Q.Rear^.Next := NewNode;

  Q.Rear := NewNode;
end;

function Dequeue(var Q: TQueue): Pointer;
var
  Temp: PQueueNode;
begin
  if Q.Front = nil then
  begin
    Result := nil;
    Exit;
  end;

  Temp := Q.Front;
  Result := Temp^.Data;
  Q.Front := Temp^.Next;

  if Q.Front = nil then
    Q.Rear := nil;

  Dispose(Temp);
end;

function Peek(var Q: TQueue): Pointer;
begin
  if Q.Front = nil then
    Result := nil
  else
    Result := Q.Front^.Data;
end;

function GetRear(var Q: TQueue): Pointer;
begin
  if Q.Rear = nil then
    Result := nil
  else
    Result := Q.Rear^.Data;
end;

function IsEmpty(var Q: TQueue): boolean;
begin
  Result := Q.Front = nil;
end;

function Count(var Q: TQueue): integer;
var
  Current: PQueueNode;
  Counter: integer;
begin
  Counter := 0;
  Current := Q.Front;
  while Current <> nil do
  begin
    Inc(Counter);
    Current := Current^.Next;
  end;
  Result := Counter;
end;

procedure Clear(var Q: TQueue);
var
  Current, Next: PQueueNode;
begin
  Current := Q.Front;
  while Current <> nil do
  begin
    Next := Current^.Next;
    Dispose(Current);
    Current := Next;
  end;
  Q.Front := nil;
  Q.Rear := nil;
end;

procedure GenerateDotFile(var Q: TQueue; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Current: PQueueNode;
  NodeIndex: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);

  try
    WriteLn(F, 'digraph Queue {');
    WriteLn(F, '    rankdir=LR;');
    WriteLn(F, '    node [shape=record, style=filled, fillcolor=lightblue];');
    WriteLn(F, '    edge [color=darkblue];');
    WriteLn(F, '');

    if IsEmpty(Q) then
    begin
      WriteLn(F,
        '    empty [label="Cola Vacía\\nFRONT = NULL\\nREAR = NULL", shape=ellipse, fillcolor=lightgray];');
    end
    else
    begin
      WriteLn(F, '    front [label="FRONT\\n(Dequeue)", shape=ellipse, fillcolor=lightgreen];');
      WriteLn(F, '    rear [label="REAR\\n(Enqueue)", shape=ellipse, fillcolor=lightcoral];');
      WriteLn(F, '');

      Current := Q.Front;
      NodeIndex := 0;
      while Current <> nil do
      begin
        if Current = Q.Front then
          WriteLn(F, Format(
            '    node%d [label="<data>%s|<next>", fillcolor=lightgreen];',
            [NodeIndex, DataToString(Current^.Data)]))
        else if Current = Q.Rear then
          WriteLn(F, Format(
            '    node%d [label="<data>%s|<next>", fillcolor=lightcoral];',
            [NodeIndex, DataToString(Current^.Data)]))
        else
          WriteLn(F, Format('    node%d [label="<data>%s|<next>"];',
            [NodeIndex, DataToString(Current^.Data)]));
        Current := Current^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');
      WriteLn(F, '    null [label="NULL", shape=ellipse, fillcolor=gray];');
      WriteLn(F, '');

      Current := Q.Front;
      NodeIndex := 0;
      while Current <> nil do
      begin
        if Current^.Next <> nil then
          WriteLn(F, Format('    node%d:next -> node%d:data [color=darkblue];',
            [NodeIndex, NodeIndex + 1]))
        else
          WriteLn(F, Format('    node%d:next -> null [color=gray];', [NodeIndex]));
        Current := Current^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');

      WriteLn(F, '    front -> node0:data [color=darkgreen, style=bold, label="dequeue"];');
      WriteLn(F, Format(
        '    rear -> node%d:data [color=darkred, style=bold, label="enqueue"];',
        [Count(Q) - 1]));

      WriteLn(F, '');

      WriteLn(F, Format(
        '    info [label="Tamaño: %d\\nFIFO: First In, First Out", shape=note, fillcolor=lightyellow];',
        [Count(Q)]));

      WriteLn(F, '');
      WriteLn(F, '    // Agrupar elementos para mejor visualización');
      WriteLn(F, '    {rank=same; front; node0;}');
      WriteLn(F, Format('    {rank=same; rear; node%d;}', [Count(Q) - 1]));
      WriteLn(F, '    {rank=min; info;}');
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
