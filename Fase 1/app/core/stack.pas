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
    Size: integer;
  end;

  TDataToString = function(Data: Pointer): string;
  TCompareFunc = function(Data1, Data2: Pointer): integer;
  TProcessFunc = procedure(Data: Pointer);

procedure Init(var S: TStack);
procedure Push(var S: TStack; Item: Pointer);
function Pop(var S: TStack): Pointer;
function Peek(const S: TStack): Pointer;
function IsEmpty(const S: TStack): boolean;
function Count(const S: TStack): integer;
procedure Clear(var S: TStack);

function Contains(const S: TStack; Item: Pointer): boolean;
function IndexOf(const S: TStack; Item: Pointer): integer;
function GetItem(const S: TStack; Index: integer): Pointer;

procedure GenerateDotFile(const S: TStack; const FileName: string;
  DataToString: TDataToString);
procedure PrintStack(const S: TStack; DataToString: TDataToString);

implementation

uses
  SysUtils;

procedure Init(var S: TStack);
begin
  S.Top := nil;
  S.Size := 0;
end;

procedure Push(var S: TStack; Item: Pointer);
var
  NewNode: PStackNode;
begin
  if Item = nil then
    raise Exception.Create('Cannot push nil item to stack');

  New(NewNode);
  NewNode^.Data := Item;
  NewNode^.Next := S.Top;
  S.Top := NewNode;
  Inc(S.Size);
end;

function Pop(var S: TStack): Pointer;
var
  Temp: PStackNode;
begin
  if IsEmpty(S) then
  begin
    Result := nil;
    Exit;
  end;

  Temp := S.Top;
  Result := Temp^.Data;
  S.Top := Temp^.Next;
  Dispose(Temp);
  Dec(S.Size);
end;

function Peek(const S: TStack): Pointer;
begin
  if IsEmpty(S) then
    Result := nil
  else
    Result := S.Top^.Data;
end;

function IsEmpty(const S: TStack): boolean;
begin
  Result := S.Top = nil;
end;

function Count(const S: TStack): integer;
begin
  Result := S.Size;
end;

procedure Clear(var S: TStack);
var
  Temp, Next: PStackNode;
begin
  Temp := S.Top;
  while Temp <> nil do
  begin
    Next := Temp^.Next;
    Dispose(Temp);
    Temp := Next;
  end;
  S.Top := nil;
  S.Size := 0;
end;

function Contains(const S: TStack; Item: Pointer): boolean;
var
  Current: PStackNode;
begin
  Result := False;
  Current := S.Top;
  while Current <> nil do
  begin
    if Current^.Data = Item then
    begin
      Result := True;
      Exit;
    end;
    Current := Current^.Next;
  end;
end;

function IndexOf(const S: TStack; Item: Pointer): integer;
var
  Current: PStackNode;
  Index: integer;
begin
  Result := -1;
  Current := S.Top;
  Index := 0;

  while Current <> nil do
  begin
    if Current^.Data = Item then
    begin
      Result := Index;
      Exit;
    end;
    Current := Current^.Next;
    Inc(Index);
  end;
end;

function GetItem(const S: TStack; Index: integer): Pointer;
var
  Current: PStackNode;
  Counter: integer;
begin
  Result := nil;

  if (Index < 0) or (Index >= S.Size) then
    Exit;

  Current := S.Top;
  Counter := 0;

  while (Current <> nil) and (Counter < Index) do
  begin
    Current := Current^.Next;
    Inc(Counter);
  end;

  if Current <> nil then
    Result := Current^.Data;
end;



procedure PrintStack(const S: TStack; DataToString: TDataToString);
var
  Current: PStackNode;
  Index: integer;
begin
  WriteLn('=== STACK STATUS ===');
  WriteLn('Size: ', S.Size);
  WriteLn('Empty: ', IsEmpty(S));
  WriteLn('Contents (TOP to BOTTOM):');

  if IsEmpty(S) then
  begin
    WriteLn('  [Stack is empty]');
  end
  else
  begin
    Current := S.Top;
    Index := 0;
    while Current <> nil do
    begin
      if Index = 0 then
        WriteLn('  [TOP] ', DataToString(Current^.Data))
      else
        WriteLn('  [', Index, '] ', DataToString(Current^.Data));
      Current := Current^.Next;
      Inc(Index);
    end;
  end;
  WriteLn('===================');
end;

procedure GenerateDotFile(const S: TStack; const FileName: string;
  DataToString: TDataToString);
var
  F: TextFile;
  Current: PStackNode;
  NodeIndex, Total: integer;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'digraph Stack {');
    WriteLn(F, '    rankdir=TB;');
    WriteLn(F, '    bgcolor=transparent;');
    WriteLn(F, '    node [');
    WriteLn(F, '        shape=record,');
    WriteLn(F, '        style="filled,rounded",');
    WriteLn(F, '        fontname="Segoe UI",');
    WriteLn(F, '        fontsize=12,');
    WriteLn(F, '        fontcolor="#FFFFFF",');
    WriteLn(F, '        margin=0.2,');
    WriteLn(F, '        penwidth=0.8');
    WriteLn(F, '    ];');
    WriteLn(F, '    edge [');
    WriteLn(F, '        color="#667eea",');
    WriteLn(F, '        penwidth=1.5,');
    WriteLn(F, '        arrowsize=0.8,');
    WriteLn(F, '        arrowhead=vee');
    WriteLn(F, '    ];');
    WriteLn(F, '');

    Total := Count(S);

    if IsEmpty(S) then
    begin
      WriteLn(F, '    empty [');
      WriteLn(F, '        label="Stack Vacío",');
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
      WriteLn(F, '    top_label [');
      WriteLn(F, '        label="TOP",');
      WriteLn(F, '        shape=plaintext,');
      WriteLn(F, '        fontsize=14,');
      WriteLn(F, '        fontcolor="#667eea",');
      WriteLn(F, '        fontname="Segoe UI Bold"');
      WriteLn(F, '    ];');
      WriteLn(F, '');

      Current := S.Top;
      NodeIndex := 0;
      while Current <> nil do
      begin
        WriteLn(F, Format('    node%d [', [NodeIndex]));
        WriteLn(F, Format('        label="%s",', [DataToString(Current^.Data)]));

        if NodeIndex = 0 then
        begin
          WriteLn(F, '        fillcolor="#667eea:#764ba2",');
          WriteLn(F, '        gradientangle=45,');
          WriteLn(F, '        color="#5a67d8"');
        end
        else if NodeIndex mod 3 = 1 then
        begin
          WriteLn(F, '        fillcolor="#4facfe:#00f2fe",');
          WriteLn(F, '        gradientangle=135,');
          WriteLn(F, '        color="#0ea5e9"');
        end
        else
        begin
          WriteLn(F, '        fillcolor="#fa709a:#fee140",');
          WriteLn(F, '        gradientangle=90,');
          WriteLn(F, '        color="#f59e0b"');
        end;

        WriteLn(F, '    ];');
        Current := Current^.Next;
        Inc(NodeIndex);
      end;

      WriteLn(F, '');

      WriteLn(F, '    null_node [');
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

      if Total > 0 then
      begin
        WriteLn(F, '    top_label -> node0 [');
        WriteLn(F, '        color="#667eea",');
        WriteLn(F, '        style=bold,');
        WriteLn(F, '        penwidth=2.0');
        WriteLn(F, '    ];');

        for NodeIndex := 0 to Total - 2 do
        begin
          WriteLn(F, Format('    node%d -> node%d [', [NodeIndex, NodeIndex + 1]));
          WriteLn(F, '        constraint=true');
          WriteLn(F, '    ];');
        end;

        WriteLn(F, Format('    node%d -> null_node [', [Total - 1]));
        WriteLn(F, '        color="#a8edea"');
        WriteLn(F, '    ];');
      end;

      WriteLn(F, '');
      WriteLn(F, '    info [');
      WriteLn(F, Format('        label="Stack\lSize: %d\nType: LIFO",', [Total]));
      WriteLn(F, '        shape=box,');
      WriteLn(F, '        style="filled,rounded",');
      WriteLn(F, '        fillcolor="#e0e7ff",');
      WriteLn(F, '        color="#667eea",');
      WriteLn(F, '        fontcolor="#5a67d8",');
      WriteLn(F, '        fontsize=10,');
      WriteLn(F, '        penwidth=0.8');
      WriteLn(F, '    ];');
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
