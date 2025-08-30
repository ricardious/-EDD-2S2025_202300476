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
function Peek(const Q: TQueue): Pointer;
function GetRear(const Q: TQueue): Pointer;
function IsEmpty(const Q: TQueue): boolean;
function Count(const Q: TQueue): integer;
procedure Clear(var Q: TQueue);

function GetItem(const Q: TQueue; Index: integer): Pointer;

procedure GenerateDotFile(const Q: TQueue; const FileName: string;
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
  begin
    Q.Front := NewNode;
    Q.Rear := NewNode;
  end
  else
  begin
    Q.Rear^.Next := NewNode;
    Q.Rear := NewNode;
  end;
end;

function Dequeue(var Q: TQueue): Pointer;
var
  Temp: PQueueNode;
begin
  if Q.Front = nil then
    Exit(nil);

  Temp := Q.Front;
  Result := Temp^.Data;
  Q.Front := Temp^.Next;
  if Q.Front = nil then
    Q.Rear := nil;
  Dispose(Temp);
end;

function Peek(const Q: TQueue): Pointer;
begin
  if Q.Front <> nil then
    Result := Q.Front^.Data
  else
    Result := nil;
end;

function GetRear(const Q: TQueue): Pointer;
begin
  if Q.Rear <> nil then
    Result := Q.Rear^.Data
  else
    Result := nil;
end;

function IsEmpty(const Q: TQueue): boolean;
begin
  Result := Q.Front = nil;
end;

function Count(const Q: TQueue): integer;
var
  Cur: PQueueNode;
begin
  Result := 0;
  Cur := Q.Front;
  while Cur <> nil do
  begin
    Inc(Result);
    Cur := Cur^.Next;
  end;
end;

procedure Clear(var Q: TQueue);
var
  Cur, Nx: PQueueNode;
begin
  Cur := Q.Front;
  while Cur <> nil do
  begin
    Nx := Cur^.Next;
    Dispose(Cur);
    Cur := Nx;
  end;
  Q.Front := nil;
  Q.Rear := nil;
end;

function GetItem(const Q: TQueue; Index: integer): Pointer;
var
  Cur: PQueueNode;
  i: integer;
begin
  if Index < 0 then Exit(nil);
  Cur := Q.Front;
  i := 0;
  while (Cur <> nil) and (i < Index) do
  begin
    Cur := Cur^.Next;
    Inc(i);
  end;
  if Cur <> nil then
    Result := Cur^.Data
  else
    Result := nil;
end;

procedure GenerateDotFile(const Q: TQueue; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Cur: PQueueNode;
  N, idx: integer;

  procedure WriteHeader;
  begin
    Writeln(F, 'digraph Queue {');
    Writeln(F, '    rankdir=TB;');
    Writeln(F, '    bgcolor=transparent;');
    Writeln(F, '    nodesep=0.6;');
    Writeln(F, '    node [');
    Writeln(F, '        shape=record,');
    Writeln(F, '        style="filled,rounded",');
    Writeln(F, '        fillcolor="#667eea:#764ba2",');
    Writeln(F, '        gradientangle=45,');
    Writeln(F, '        color="#5a67d8",');
    Writeln(F, '        penwidth=0.8,');
    Writeln(F, '        fontname="Segoe UI",');
    Writeln(F, '        fontsize=12,');
    Writeln(F, '        fontcolor="#FFFFFF",');
    Writeln(F, '        margin=0.2');
    Writeln(F, '    ];');
    Writeln(F, '    edge [');
    Writeln(F, '        color="#667eea",');
    Writeln(F, '        penwidth=1.5,');
    Writeln(F, '        arrowsize=0.8,');
    Writeln(F, '        arrowhead=vee');
    Writeln(F, '    ];');
    Writeln(F);
  end;

  procedure WriteEmpty;
  begin
    Writeln(F, '    empty [');
    Writeln(F, '        label="Cola Vacía",');
    Writeln(F, '        shape=ellipse,');
    Writeln(F, '        style="filled,rounded",');
    Writeln(F, '        fillcolor="#f093fb:#f5576c",');
    Writeln(F, '        gradientangle=90,');
    Writeln(F, '        color="#e53e3e",');
    Writeln(F, '        fontcolor="#FFFFFF",');
    Writeln(F, '        penwidth=0.8');
    Writeln(F, '    ];');
  end;

  procedure WritePointers;
  begin
    Writeln(F, '    front_ptr [');
    Writeln(F, '        label="FRONT",');
    Writeln(F, '        shape=plaintext,');
    Writeln(F, '        fontname="Segoe UI",');
    Writeln(F, '        fontsize=14,');
    Writeln(F, '        fontcolor="#5a67d8"');
    Writeln(F, '    ];');

    Writeln(F, '    rear_ptr [');
    Writeln(F, '        label="REAR",');
    Writeln(F, '        shape=plaintext,');
    Writeln(F, '        fontname="Segoe UI",');
    Writeln(F, '        fontsize=14,');
    Writeln(F, '        fontcolor="#5a67d8"');
    Writeln(F, '    ];');
    Writeln(F);
  end;

  procedure WriteNullBottom;
  begin
    Writeln(F, '    null_bottom [');
    Writeln(F, '        label="∅",');
    Writeln(F, '        shape=circle,');
    Writeln(F, '        style="filled",');
    Writeln(F, '        fillcolor="#a8edea:#fed6e3",');
    Writeln(F, '        gradientangle=180,');
    Writeln(F, '        color="#667eea",');
    Writeln(F, '        fontcolor="#5a67d8",');
    Writeln(F, '        fontsize=14,');
    Writeln(F, '        penwidth=0.8,');
    Writeln(F, '        width=0.5,');
    Writeln(F, '        height=0.5');
    Writeln(F, '    ];');
    Writeln(F);
  end;

begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteHeader;

    if IsEmpty(Q) then
    begin
      WriteEmpty;
      Writeln(F, '}');
      Exit;
    end;

    N := Count(Q);
    WritePointers;
    WriteNullBottom;

    Cur := Q.Front;
    idx := 0;
    while Cur <> nil do
    begin
      Writeln(F, Format('    n%d [', [idx]));
      Writeln(F, Format('        label="<data>%s",', [DataToString(Cur^.Data)]));
      if (idx mod 2) = 0 then
      begin
        Writeln(F, '        fillcolor="#667eea:#764ba2",');
        Writeln(F, '        gradientangle=45');
      end
      else
      begin
        Writeln(F, '        fillcolor="#4facfe:#00f2fe",');
        Writeln(F, '        gradientangle=135');
      end;
      Writeln(F, '    ];');

      Cur := Cur^.Next;
      Inc(idx);
    end;

    Writeln(F);

    Writeln(F, '    front_ptr -> n0 [style=dashed, color="#5a67d8", constraint=false];');
    Writeln(F, Format(
      '    rear_ptr  -> n%d [style=dashed, color="#5a67d8", constraint=false];',
      [N - 1]));
    Writeln(F, '    {rank=same; front_ptr; n0;}');
    Writeln(F, Format('    {rank=same; rear_ptr;  n%d;}', [N - 1]));
    Writeln(F);

    for idx := 0 to N - 2 do
      Writeln(F, Format('    n%d -> n%d [constraint=true];', [idx, idx + 1]));

    Writeln(F, Format('    n%d -> null_bottom [style=dashed, color="#a8edea"];',
      [N - 1]));

    Writeln(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
