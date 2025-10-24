unit LinkedListOfLists;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

type
  PInnerNode = ^TInnerNode;

  TInnerNode = record
    Data: Pointer;
    Next: PInnerNode;
  end;

  POuterNode = ^TOuterNode;

  TOuterNode = record
    Name: ansistring;
    Inner: PInnerNode;
    Next: POuterNode;
  end;

  TLinkedListOfLists = record
    Head: POuterNode;
  end;

  TDataToString = function(Data: Pointer): ansistring;

procedure Init(var L: TLinkedListOfLists);
procedure Clear(var L: TLinkedListOfLists);

function AddOuter(var L: TLinkedListOfLists; const Name: ansistring): POuterNode;
function FindOuter(const L: TLinkedListOfLists; const Name: ansistring): POuterNode;

procedure AddInner(Outer: POuterNode; Item: Pointer);
procedure AddInnerUnique(Outer: POuterNode; Item: Pointer);
function InnerContains(const Outer: POuterNode; Item: Pointer): boolean;

procedure GenerateDotFile(const L: TLinkedListOfLists; const FileName: string;
  ToString: TDataToString);

implementation

procedure Init(var L: TLinkedListOfLists);
begin
  L.Head := nil;
end;

procedure Clear(var L: TLinkedListOfLists);
var
  o, oTmp: POuterNode;
  i, iTmp: PInnerNode;
begin
  o := L.Head;
  while o <> nil do
  begin
    i := o^.Inner;
    while i <> nil do
    begin
      iTmp := i;
      i := i^.Next;
      Dispose(iTmp);
    end;
    oTmp := o;
    o := o^.Next;
    Dispose(oTmp);
  end;
  L.Head := nil;
end;

function FindOuter(const L: TLinkedListOfLists; const Name: ansistring): POuterNode;
var
  cur: POuterNode;
begin
  cur := L.Head;
  Result := nil;
  while cur <> nil do
  begin
    if cur^.Name = Name then
    begin
      Result := cur;
      Exit;
    end;
    cur := cur^.Next;
  end;
end;

function AddOuter(var L: TLinkedListOfLists; const Name: ansistring): POuterNode;
var
  n, cur: POuterNode;
begin
  if FindOuter(L, Name) <> nil then
  begin
    Result := nil;
    Exit;
  end;

  New(n);
  n^.Name := Name;
  n^.Inner := nil;
  n^.Next := nil;

  if L.Head = nil then
    L.Head := n
  else
  begin
    cur := L.Head;
    while cur^.Next <> nil do cur := cur^.Next;
    cur^.Next := n;
  end;

  Result := n;
end;

function InnerContains(const Outer: POuterNode; Item: Pointer): boolean;
var
  cur: PInnerNode;
begin
  Result := False;
  if Outer = nil then Exit;
  cur := Outer^.Inner;
  while cur <> nil do
  begin
    if cur^.Data = Item then
    begin
      Result := True;
      Exit;
    end;
    cur := cur^.Next;
  end;
end;

procedure AddInner(Outer: POuterNode; Item: Pointer);
var
  n, cur: PInnerNode;
begin
  if Outer = nil then Exit;

  New(n);
  n^.Data := Item;
  n^.Next := nil;

  if Outer^.Inner = nil then
    Outer^.Inner := n
  else
  begin
    cur := Outer^.Inner;
    while cur^.Next <> nil do cur := cur^.Next;
    cur^.Next := n;
  end;
end;

procedure AddInnerUnique(Outer: POuterNode; Item: Pointer);
begin
  if Outer = nil then Exit;
  if not InnerContains(Outer, Item) then
    AddInner(Outer, Item);
end;

procedure GenerateDotFile(const L: TLinkedListOfLists; const FileName: string;
  ToString: TDataToString);
var
  F: TextFile;
  o: POuterNode;
  i: PInnerNode;
  outerIdx, innerIdx: integer;

  procedure WriteHeader;
  begin
    Writeln(F, 'digraph LinkedListOfLists {');
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
    Writeln(F, '        label="Lista de Listas Vacía",');
    Writeln(F, '        shape=ellipse,');
    Writeln(F, '        style="filled,rounded",');
    Writeln(F, '        fillcolor="#f093fb:#f5576c",');
    Writeln(F, '        gradientangle=90,');
    Writeln(F, '        color="#e53e3e",');
    Writeln(F, '        fontcolor="#FFFFFF",');
    Writeln(F, '        penwidth=0.8');
    Writeln(F, '    ];');
  end;

begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteHeader;

    if L.Head = nil then
    begin
      WriteEmpty;
      Writeln(F, '}');
      Exit;
    end;

    o := L.Head;
    outerIdx := 0;
    while o <> nil do
    begin
      Writeln(F, Format('    o%d [label="<name>%s"];', [outerIdx, o^.Name]));

      i := o^.Inner;
      innerIdx := 0;
      while i <> nil do
      begin
        if Assigned(ToString) then
          Writeln(F, Format('    u%d_%d [label="<data>%s"];',
            [outerIdx, innerIdx, ToString(i^.Data)]))
        else
          Writeln(F, Format('    u%d_%d [label="<data>Item"];', [outerIdx, innerIdx]));

        if (innerIdx mod 2) = 0 then
        begin
          Writeln(F, '    ', Format(
            'u%d_%d [fillcolor="#667eea:#764ba2", gradientangle=45];',
            [outerIdx, innerIdx]));
        end
        else
        begin
          Writeln(F, '    ', Format(
            'u%d_%d [fillcolor="#4facfe:#00f2fe", gradientangle=135];',
            [outerIdx, innerIdx]));
        end;

        i := i^.Next;
        Inc(innerIdx);
      end;

      if (outerIdx mod 2) = 0 then
      begin
        Writeln(F, Format('    o%d [fillcolor="#667eea:#764ba2", gradientangle=45];',
          [outerIdx]));
      end
      else
      begin
        Writeln(F, Format('    o%d [fillcolor="#4facfe:#00f2fe", gradientangle=135];',
          [outerIdx]));
      end;

      o := o^.Next;
      Inc(outerIdx);
    end;

    Writeln(F);

    for outerIdx := 0 to High(outerIdx) do ;

    o := L.Head;
    outerIdx := 0;
    while o <> nil do
    begin
      if o^.Next <> nil then
        Writeln(F, Format('    o%d -> o%d [constraint=true];',
          [outerIdx, outerIdx + 1]));

      i := o^.Inner;
      if i <> nil then
      begin
        Writeln(F, Format('    o%d -> u%d_0 [constraint=false];', [outerIdx, outerIdx]));

        innerIdx := 0;
        while (i <> nil) and (i^.Next <> nil) do
        begin
          Writeln(F, Format('    u%d_%d -> u%d_%d [constraint=true];',
            [outerIdx, innerIdx, outerIdx, innerIdx + 1]));
          i := i^.Next;
          Inc(innerIdx);
        end;

        Writeln(F, Format('    {rank=same; o%d; u%d_0;}', [outerIdx, outerIdx]));
      end;

      o := o^.Next;
      Inc(outerIdx);
    end;

    Writeln(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
