unit uBST;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils;

type
  PNode = ^TNode;
  TNodeVisitProcedure = procedure(const N: PNode);

  TNode = record
    id: integer;
    first_name: unicodestring;
    last_name: unicodestring;
    email: unicodestring;
    left, right: PNode;
  end;

function NewNode(const AId: integer; const AFName, ALName, AEmail: unicodestring): PNode;
procedure Insert(var Root: PNode; Node: PNode);
procedure InOrder(const Root: PNode; const OnVisit: TNodeVisitProcedure);
procedure FreeTree(var Root: PNode);
function CountNodes(const Root: PNode): SizeInt;

implementation

function NewNode(const AId: integer; const AFName, ALName, AEmail: unicodestring): PNode;
var
  N: PNode;
begin
  New(N);
  N^.id := AId;
  N^.first_name := AFName;
  N^.last_name := ALName;
  N^.email := AEmail;
  N^.left := nil;
  N^.right := nil;
  Result := N;
end;

procedure Insert(var Root: PNode; Node: PNode);
begin
  if Root = nil then
  begin
    Root := Node;
    Exit;
  end;
  if Node^.id < Root^.id then
    Insert(Root^.left, Node)
  else if Node^.id > Root^.id then
    Insert(Root^.right, Node)
  else
  begin
    Root^.first_name := Node^.first_name;
    Root^.last_name := Node^.last_name;
    Root^.email := Node^.email;
    Dispose(Node);
  end;
end;

procedure InOrder(const Root: PNode; const OnVisit: TNodeVisitProcedure);
begin
  if Root = nil then Exit;
  InOrder(Root^.left, OnVisit);
  if Assigned(OnVisit) then OnVisit(Root);
  InOrder(Root^.right, OnVisit);
end;

procedure FreeTree(var Root: PNode);
begin
  if Root = nil then Exit;
  FreeTree(Root^.left);
  FreeTree(Root^.right);
  Dispose(Root);
  Root := nil;
end;

function CountNodes(const Root: PNode): SizeInt;
begin
  if Root = nil then Exit(0);
  Result := 1 + CountNodes(Root^.left) + CountNodes(Root^.right);
end;

end.
