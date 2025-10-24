unit BST;

interface

uses
  SysUtils, Classes;

type
  TCompareFunction = function(Data1, Data2: Pointer): integer;
  TProcessNodeProc = procedure(Data: Pointer);
  TDataToStringFunction = function(Data: Pointer): string;

  PBSTNode = ^TBSTNode;

  TBSTNode = record
    Data: Pointer;
    Left: PBSTNode;
    Right: PBSTNode;
  end;

  TBSTree = class
  private
    FRoot: PBSTNode;
    FCompare: TCompareFunction;
    function InsertNode(Node: PBSTNode; NewData: Pointer): PBSTNode;
    procedure InOrderTraversal(Node: PBSTNode; ProcessProc: TProcessNodeProc);
    function SearchNode(Node: PBSTNode; Key: Pointer): PBSTNode;
    procedure GenerateDotRecursive(Node: PBSTNode; DotContent: TStrings;
      DataToString: TDataToStringFunction);
    function DeleteNode(Node: PBSTNode; Key: Pointer): PBSTNode;
    function FindMin(Node: PBSTNode): PBSTNode;
    procedure GenerateDotRecursivePorts(Node: PBSTNode; DotContent: TStrings;
  DataToString: TDataToStringFunction; var NullIdx: integer);
  public
    constructor Create(ACompareFunc: TCompareFunction);
    procedure Insert(NewData: Pointer);
    function Search(Key: Pointer): Pointer;
    procedure TraverseInOrder(ProcessProc: TProcessNodeProc);
    function GetRoot: PBSTNode;
    procedure GenerateDotFile(const FileName: string;
      DataToString: TDataToStringFunction);
    procedure Delete(Key: Pointer);
  end;

implementation

constructor TBSTree.Create(ACompareFunc: TCompareFunction);
begin
  FRoot := nil;
  FCompare := ACompareFunc;
end;

function TBSTree.InsertNode(Node: PBSTNode; NewData: Pointer): PBSTNode;
begin
  if Node = nil then
  begin
    New(Node);
    Node^.Data := NewData;
    Node^.Left := nil;
    Node^.Right := nil;
    Result := Node;
    Exit;
  end;

  if FCompare(NewData, Node^.Data) = -1 then
    Node^.Left := InsertNode(Node^.Left, NewData)
  else if FCompare(NewData, Node^.Data) = 1 then
    Node^.Right := InsertNode(Node^.Right, NewData);
  Result := Node;
end;

procedure TBSTree.Insert(NewData: Pointer);
begin
  FRoot := InsertNode(FRoot, NewData);
end;

function TBSTree.SearchNode(Node: PBSTNode; Key: Pointer): PBSTNode;
begin
  if (Node = nil) or (FCompare(Key, Node^.Data) = 0) then
    Result := Node
  else if FCompare(Key, Node^.Data) = -1 then
    Result := SearchNode(Node^.Left, Key)
  else
    Result := SearchNode(Node^.Right, Key);
end;

function TBSTree.Search(Key: Pointer): Pointer;
var
  Node: PBSTNode;
begin
  Node := SearchNode(FRoot, Key);
  if Node <> nil then
    Result := Node^.Data
  else
    Result := nil;
end;

procedure TBSTree.InOrderTraversal(Node: PBSTNode; ProcessProc: TProcessNodeProc);
begin
  if Node <> nil then
  begin
    InOrderTraversal(Node^.Left, ProcessProc);
    ProcessProc(Node^.Data);
    InOrderTraversal(Node^.Right, ProcessProc);
  end;
end;

procedure TBSTree.TraverseInOrder(ProcessProc: TProcessNodeProc);
begin
  InOrderTraversal(FRoot, ProcessProc);
end;

function TBSTree.GetRoot: PBSTNode;
begin
  Result := FRoot;
end;

function TBSTree.FindMin(Node: PBSTNode): PBSTNode;
begin
  while (Node <> nil) and (Node^.Left <> nil) do
    Node := Node^.Left;
  Result := Node;
end;

function TBSTree.DeleteNode(Node: PBSTNode; Key: Pointer): PBSTNode;
var
  Temp, MinRight: PBSTNode;
begin
  if Node = nil then Exit(nil);

  if FCompare(Key, Node^.Data) < 0 then
    Node^.Left := DeleteNode(Node^.Left, Key)
  else if FCompare(Key, Node^.Data) > 0 then
    Node^.Right := DeleteNode(Node^.Right, Key)
  else
  begin
    if (Node^.Left = nil) and (Node^.Right = nil) then
    begin
      Dispose(Node);
      Exit(nil);
    end
    else if Node^.Left = nil then
    begin
      Temp := Node^.Right;
      Dispose(Node);
      Exit(Temp);
    end
    else if Node^.Right = nil then
    begin
      Temp := Node^.Left;
      Dispose(Node);
      Exit(Temp);
    end
    else
    begin
      MinRight := FindMin(Node^.Right);
      Node^.Data := MinRight^.Data;
      Node^.Right := DeleteNode(Node^.Right, MinRight^.Data);
    end;
  end;
  Result := Node;
end;

procedure TBSTree.Delete(Key: Pointer);
begin
  FRoot := DeleteNode(FRoot, Key);
end;


procedure TBSTree.GenerateDotRecursive(Node: PBSTNode; DotContent: TStrings;
  DataToString: TDataToStringFunction);
var
  NodeID: string;
begin
  if Node = nil then Exit;

  NodeID := Format('node%p', [Node]);

  DotContent.Add(Format('%s [label="%s"];', [NodeID, DataToString(Node^.Data)]));

  if Node^.Left <> nil then
  begin
    DotContent.Add(Format('%s -> node%p;', [NodeID, Node^.Left]));
    GenerateDotRecursive(Node^.Left, DotContent, DataToString);
  end;

  if Node^.Right <> nil then
  begin
    DotContent.Add(Format('%s -> node%p;', [NodeID, Node^.Right]));
    GenerateDotRecursive(Node^.Right, DotContent, DataToString);
  end;
end;

procedure TBSTree.GenerateDotRecursivePorts(Node: PBSTNode; DotContent: TStrings;
  DataToString: TDataToStringFunction; var NullIdx: integer);
var
  NodeID: string;
  NullID: string;
begin
  if Node = nil then Exit;

  NodeID := Format('node%p', [Node]);

  DotContent.Add(Format('"%s" [label="<L> | %s | <R>"];',
    [NodeID, StringReplace(DataToString(Node^.Data), '"', '\"', [rfReplaceAll])]));

  if Node^.Left <> nil then
  begin
    DotContent.Add(Format('"%s":L -> "node%p";', [NodeID, Node^.Left]));
    GenerateDotRecursivePorts(Node^.Left, DotContent, DataToString, NullIdx);
  end
  else
  begin
    Inc(NullIdx);
    NullID := Format('null%d', [NullIdx]);
    DotContent.Add(Format('"%s" [label="∅", shape=circle, fontsize=9, width=0.3, height=0.3, style="filled", fillcolor="#eeeeee", color="#bbbbbb"];', [NullID]));
    DotContent.Add(Format('"%s":L -> "%s" [style=dashed, color="#bbbbbb"];', [NodeID, NullID]));
  end;

  if Node^.Right <> nil then
  begin
    DotContent.Add(Format('"%s":R -> "node%p";', [NodeID, Node^.Right]));
    GenerateDotRecursivePorts(Node^.Right, DotContent, DataToString, NullIdx);
  end
  else
  begin
    Inc(NullIdx);
    NullID := Format('null%d', [NullIdx]);
    DotContent.Add(Format('"%s" [label="∅", shape=circle, fontsize=9, width=0.3, height=0.3, style="filled", fillcolor="#eeeeee", color="#bbbbbb"];', [NullID]));
    DotContent.Add(Format('"%s":R -> "%s" [style=dashed, color="#bbbbbb"];', [NodeID, NullID]));
  end;
end;


procedure TBSTree.GenerateDotFile(const FileName: string;
  DataToString: TDataToStringFunction);
var
  DotContent: TStringList;
  NullIdx: integer;
begin
  DotContent := TStringList.Create;
  try
    DotContent.Add('digraph BSTree {');
    DotContent.Add('  graph [rankdir=TB, splines=ortho];');
    DotContent.Add('  node [shape=record, fontname="Arial", fontsize=11, height=0.2, margin="0.06,0.04"];');
    DotContent.Add('  edge [arrowsize=0.7];');
    DotContent.Add('  nodesep=0.35;');
    DotContent.Add('  ranksep=0.45;');
    DotContent.Add('  ordering=out;');

    if FRoot <> nil then
    begin
      NullIdx := 0;
      GenerateDotRecursivePorts(FRoot, DotContent, DataToString, NullIdx);
    end;

    DotContent.Add('}');
    DotContent.SaveToFile(FileName);
  finally
    DotContent.Free;
  end;
end;


end.
