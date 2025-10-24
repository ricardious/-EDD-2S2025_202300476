unit AVLTree;

interface

uses
  SysUtils, Classes;

type
  TCompareFunction = function(Data1, Data2: Pointer): integer;
  TProcessNodeProc = procedure(Data: Pointer);
  TDataToStringFunction = function(Data: Pointer): string;

  PAVLNode = ^TAVLNode;

  TAVLNode = record
    Data: Pointer;
    Left: PAVLNode;
    Right: PAVLNode;
    Height: integer;
  end;

  TAVLTree = class
  private
    FRoot: PAVLNode;
    FCompare: TCompareFunction;
    function GetHeight(Node: PAVLNode): integer;
    function GetBalance(Node: PAVLNode): integer;
    function Max(a, b: integer): integer;
    function RightRotate(y: PAVLNode): PAVLNode;
    function LeftRotate(x: PAVLNode): PAVLNode;
    function InsertNode(Node: PAVLNode; NewData: Pointer): PAVLNode;
    procedure InOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
    procedure PreOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
    procedure PostOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
    function SearchNode(Node: PAVLNode; Key: Pointer): PAVLNode;
    procedure GenerateDotRecursive(Node: PAVLNode; DotContent: TStrings;
      DataToString: TDataToStringFunction);
  public
    constructor Create(ACompareFunc: TCompareFunction);
    procedure Insert(NewData: Pointer);
    function Search(Key: Pointer): Pointer;
    procedure TraverseInOrder(ProcessProc: TProcessNodeProc);
    procedure TraversePreOrder(ProcessProc: TProcessNodeProc);
    procedure TraversePostOrder(ProcessProc: TProcessNodeProc);
    function GetRoot: PAVLNode;
    procedure GenerateDotFile(const FileName: string;
      DataToString: TDataToStringFunction);
  end;

implementation

constructor TAVLTree.Create(ACompareFunc: TCompareFunction);
begin
  FRoot := nil;
  FCompare := ACompareFunc;
end;

function TAVLTree.GetHeight(Node: PAVLNode): integer;
begin
  if Node = nil then
    Result := 0
  else
    Result := Node^.Height;
end;

function TAVLTree.Max(a, b: integer): integer;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function TAVLTree.RightRotate(y: PAVLNode): PAVLNode;
var
  x, T2: PAVLNode;
begin
  x := y^.Left;
  T2 := x^.Right;
  x^.Right := y;
  y^.Left := T2;
  y^.Height := Max(GetHeight(y^.Left), GetHeight(y^.Right)) + 1;
  x^.Height := Max(GetHeight(x^.Left), GetHeight(x^.Right)) + 1;
  Result := x;
end;

function TAVLTree.LeftRotate(x: PAVLNode): PAVLNode;
var
  y, T2: PAVLNode;
begin
  y := x^.Right;
  T2 := y^.Left;
  y^.Left := x;
  x^.Right := T2;
  x^.Height := Max(GetHeight(x^.Left), GetHeight(x^.Right)) + 1;
  y^.Height := Max(GetHeight(y^.Left), GetHeight(y^.Right)) + 1;
  Result := y;
end;

function TAVLTree.GetBalance(Node: PAVLNode): integer;
begin
  if Node = nil then
    Result := 0
  else
    Result := GetHeight(Node^.Left) - GetHeight(Node^.Right);
end;

function TAVLTree.InsertNode(Node: PAVLNode; NewData: Pointer): PAVLNode;
var
  Balance: integer;
begin
  if Node = nil then
  begin
    New(Node);
    Node^.Data := NewData;
    Node^.Left := nil;
    Node^.Right := nil;
    Node^.Height := 1;
    Result := Node;
    Exit;
  end;

  if FCompare(NewData, Node^.Data) = -1 then
    Node^.Left := InsertNode(Node^.Left, NewData)
  else if FCompare(NewData, Node^.Data) = 1 then
    Node^.Right := InsertNode(Node^.Right, NewData)
  else
  begin
    Result := Node;
    Exit;
  end;

  Node^.Height := 1 + Max(GetHeight(Node^.Left), GetHeight(Node^.Right));
  Balance := GetBalance(Node);

  if (Balance > 1) and (FCompare(NewData, Node^.Left^.Data) = -1) then
    Result := RightRotate(Node)
  else if (Balance < -1) and (FCompare(NewData, Node^.Right^.Data) = 1) then
    Result := LeftRotate(Node)
  else if (Balance > 1) and (FCompare(NewData, Node^.Left^.Data) = 1) then
  begin
    Node^.Left := LeftRotate(Node^.Left);
    Result := RightRotate(Node);
  end
  else if (Balance < -1) and (FCompare(NewData, Node^.Right^.Data) = -1) then
  begin
    Node^.Right := RightRotate(Node^.Right);
    Result := LeftRotate(Node);
  end
  else
    Result := Node;
end;

procedure TAVLTree.Insert(NewData: Pointer);
begin
  FRoot := InsertNode(FRoot, NewData);
end;

function TAVLTree.SearchNode(Node: PAVLNode; Key: Pointer): PAVLNode;
begin
  if Node = nil then
    Result := nil
  else if FCompare(Key, Node^.Data) = 0 then
    Result := Node
  else if FCompare(Key, Node^.Data) = -1 then
    Result := SearchNode(Node^.Left, Key)
  else
    Result := SearchNode(Node^.Right, Key);
end;

function TAVLTree.Search(Key: Pointer): Pointer;
var
  Node: PAVLNode;
begin
  Node := SearchNode(FRoot, Key);
  if Node <> nil then
    Result := Node^.Data
  else
    Result := nil;
end;

procedure TAVLTree.InOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
begin
  if Node <> nil then
  begin
    InOrderTraversal(Node^.Left, ProcessProc);
    ProcessProc(Node^.Data);
    InOrderTraversal(Node^.Right, ProcessProc);
  end;
end;

procedure TAVLTree.TraverseInOrder(ProcessProc: TProcessNodeProc);
begin
  InOrderTraversal(FRoot, ProcessProc);
end;

procedure TAVLTree.PreOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
begin
  if Node <> nil then
  begin
    ProcessProc(Node^.Data);
    PreOrderTraversal(Node^.Left, ProcessProc);
    PreOrderTraversal(Node^.Right, ProcessProc);
  end;
end;

procedure TAVLTree.TraversePreOrder(ProcessProc: TProcessNodeProc);
begin
  PreOrderTraversal(FRoot, ProcessProc);
end;

procedure TAVLTree.PostOrderTraversal(Node: PAVLNode; ProcessProc: TProcessNodeProc);
begin
  if Node <> nil then
  begin
    PostOrderTraversal(Node^.Left, ProcessProc);
    PostOrderTraversal(Node^.Right, ProcessProc);
    ProcessProc(Node^.Data);
  end;
end;

procedure TAVLTree.TraversePostOrder(ProcessProc: TProcessNodeProc);
begin
  PostOrderTraversal(FRoot, ProcessProc);
end;

function TAVLTree.GetRoot: PAVLNode;
begin
  Result := FRoot;
end;

procedure TAVLTree.GenerateDotRecursive(Node: PAVLNode; DotContent: TStrings;
  DataToString: TDataToStringFunction);
var
  NodeID, NodeLabel: string;
  Balance: integer;
begin
  if Node = nil then Exit;

  NodeID := Format('node%p', [Node]);
  Balance := GetBalance(Node);

  NodeLabel := Format('Dato: %s | H: %d | FB: %d',
    [DataToString(Node^.Data), Node^.Height, Balance]);
  DotContent.Add(Format('%s [label="%s", shape=record];', [NodeID, NodeLabel]));

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

procedure TAVLTree.GenerateDotFile(const FileName: string;
  DataToString: TDataToStringFunction);
var
  DotContent: TStringList;
begin
  DotContent := TStringList.Create;
  try
    DotContent.Add('digraph AVLTree {');
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
