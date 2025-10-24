unit BTree;

interface

uses
  SysUtils, Classes;

const
  B_TREE_ORDER = 5;

type
  TCompareFunction = function(Data1, Data2: Pointer): integer;
  TDataToStringFunction = function(Data: Pointer): string;

  PBTreeNode = ^TBTreeNode;

  TBTreeNode = record
    Keys: array[1..B_TREE_ORDER - 1] of Pointer;
    Children: array[1..B_TREE_ORDER] of PBTreeNode;
    n: integer;
    leaf: boolean;
  end;

  TBTree = class
  private
    FRoot: PBTreeNode;
    FCompare: TCompareFunction;
    procedure InsertNonFull(Node: PBTreeNode; NewData: Pointer);
    procedure SplitChild(Parent: PBTreeNode; i: integer; FullChild: PBTreeNode);
    function SearchNode(Node: PBTreeNode; Key: Pointer): Pointer;
    procedure GenerateDotRecursive(Node: PBTreeNode; DotContent: TStrings;
      DataToString: TDataToStringFunction);
  public
    constructor Create(ACompareFunc: TCompareFunction);
    procedure Insert(NewData: Pointer);
    function Search(Key: Pointer): Pointer;
    function GetRoot: PBTreeNode;
    procedure GenerateDotFile(const FileName: string;
      DataToString: TDataToStringFunction);
  end;

implementation

procedure ZeroNode(N: PBTreeNode);
begin
  if N = nil then Exit;
  FillChar(N^, SizeOf(TBTreeNode), 0);
  // n=0, leaf=false, Keys[]=nil, Children[]=nil
end;

constructor TBTree.Create(ACompareFunc: TCompareFunction);
var
  NewNode: PBTreeNode;
begin
  FCompare := ACompareFunc;
  New(NewNode);
  ZeroNode(NewNode);
  NewNode^.leaf := True;
  NewNode^.n := 0;
  FRoot := NewNode;
end;

function TBTree.Search(Key: Pointer): Pointer;
begin
  if FRoot = nil then
    Result := nil
  else
    Result := SearchNode(FRoot, Key);
end;

function TBTree.SearchNode(Node: PBTreeNode; Key: Pointer): Pointer;
var
  i: integer;
begin
  i := 1;
  while (i <= Node^.n) and (FCompare(Key, Node^.Keys[i]) = 1) do
    Inc(i);

  if (i <= Node^.n) and (FCompare(Key, Node^.Keys[i]) = 0) then
    Result := Node^.Keys[i]
  else if Node^.leaf then
    Result := nil
  else
    Result := SearchNode(Node^.Children[i], Key);
end;

procedure TBTree.SplitChild(Parent: PBTreeNode; i: integer; FullChild: PBTreeNode);
var
  NewNode: PBTreeNode;
  j, t: integer;
begin
  t := (B_TREE_ORDER - 1) div 2;

  New(NewNode);
  ZeroNode(NewNode);
  NewNode^.leaf := FullChild^.leaf;
  NewNode^.n := t;

  for j := 1 to t do
  begin
    NewNode^.Keys[j] := FullChild^.Keys[j + t + 1];
    FullChild^.Keys[j + t + 1] := nil;
  end;

  if not FullChild^.leaf then
  begin
    for j := 1 to t + 1 do
    begin
      NewNode^.Children[j] := FullChild^.Children[j + t + 1];
      FullChild^.Children[j + t + 1] := nil;
    end;
  end;

  FullChild^.n := t;

  for j := Parent^.n + 1 downto i + 1 do
    Parent^.Children[j + 1] := Parent^.Children[j];
  Parent^.Children[i + 1] := NewNode;

  for j := Parent^.n downto i do
    Parent^.Keys[j + 1] := Parent^.Keys[j];

  Parent^.Keys[i] := FullChild^.Keys[t + 1];
  FullChild^.Keys[t + 1] := nil;

  Inc(Parent^.n);
end;

procedure TBTree.InsertNonFull(Node: PBTreeNode; NewData: Pointer);
var
  i: integer;
begin
  i := Node^.n;
  if Node^.leaf then
  begin
    while (i >= 1) and (FCompare(NewData, Node^.Keys[i]) = -1) do
    begin
      Node^.Keys[i + 1] := Node^.Keys[i];
      Dec(i);
    end;
    Node^.Keys[i + 1] := NewData;
    Inc(Node^.n);
  end
  else
  begin
    while (i >= 1) and (FCompare(NewData, Node^.Keys[i]) = -1) do
      Dec(i);
    Inc(i);

    if Node^.Children[i]^.n = B_TREE_ORDER - 1 then
    begin
      SplitChild(Node, i, Node^.Children[i]);
      if FCompare(NewData, Node^.Keys[i]) = 1 then
        Inc(i);
    end;
    InsertNonFull(Node^.Children[i], NewData);
  end;
end;

procedure TBTree.Insert(NewData: Pointer);
var
  RootNode, NewRoot: PBTreeNode;
begin
  RootNode := FRoot;
  if RootNode^.n = B_TREE_ORDER - 1 then
  begin
    New(NewRoot);
    ZeroNode(NewRoot);
    FRoot := NewRoot;
    NewRoot^.leaf := False;
    NewRoot^.n := 0;
    NewRoot^.Children[1] := RootNode;
    SplitChild(NewRoot, 1, RootNode);
    InsertNonFull(NewRoot, NewData);
  end
  else
    InsertNonFull(RootNode, NewData);
end;

function TBTree.GetRoot: PBTreeNode;
begin
  Result := FRoot;
end;

procedure TBTree.GenerateDotRecursive(Node: PBTreeNode; DotContent: TStrings;
  DataToString: TDataToStringFunction);
var
  NodeID, NodeLabel: string;
  i: integer;
begin
  if Node = nil then Exit;

  NodeID := Format('node%p', [Node]);

  NodeLabel := '';
  for i := 1 to Node^.n do
  begin
    NodeLabel := NodeLabel + Format('<f%d> | %s |',
      [i - 1, DataToString(Node^.Keys[i])]);
  end;
  NodeLabel := NodeLabel + Format('<f%d>', [Node^.n]);

  DotContent.Add(Format('%s [label="%s", shape=record];', [NodeID, NodeLabel]));

  if not Node^.leaf then
  begin
    for i := 1 to Node^.n + 1 do
    begin
      if Node^.Children[i] <> nil then
      begin
        DotContent.Add(Format('"%s":f%d -> "node%p";',
          [NodeID, i - 1, Node^.Children[i]]));
        GenerateDotRecursive(Node^.Children[i], DotContent, DataToString);
      end;
    end;
  end;
end;

procedure TBTree.GenerateDotFile(const FileName: string;
  DataToString: TDataToStringFunction);
var
  DotContent: TStringList;
begin
  DotContent := TStringList.Create;
  try
    DotContent.Add('digraph BTree {');
    DotContent.Add('node [fontname="Arial"];');
    if FRoot <> nil then
    begin
      GenerateDotRecursive(FRoot, DotContent, DataToString);
    end;
    DotContent.Add('}');
    DotContent.SaveToFile(FileName);
  finally
    DotContent.Free;
  end;
end;

end.
