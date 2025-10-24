unit SparseMatrix;

{$mode ObjFPC}{$H+}

interface

uses
  Classes;

type
  PSparseNode = ^TSparseNode;

  TSparseNode = record
    Row: integer;
    Col: integer;
    Data: Pointer;
    NextRow: PSparseNode;
    NextCol: PSparseNode;
  end;

  PRowHeader = ^TRowHeader;
  PColHeader = ^TColHeader;

  TRowHeader = record
    RowIndex: integer;
    FirstNode: PSparseNode;
    NextRow: PRowHeader;
  end;

  TColHeader = record
    ColIndex: integer;
    FirstNode: PSparseNode;
    NextCol: PColHeader;
  end;

  TSparseMatrix = record
    RowHeaders: PRowHeader;
    ColHeaders: PColHeader;
    MaxRows: integer;
    MaxCols: integer;
    NodeCount: integer;
  end;

  TDataToString = function(Data: Pointer): string;
  TCompareData = function(Data1, Data2: Pointer): boolean;
  TTraverseProc = procedure(Row, Col: integer; Data: Pointer);

procedure Init(var M: TSparseMatrix; MaxRows, MaxCols: integer);
procedure Insert(var M: TSparseMatrix; Row, Col: integer; Data: Pointer);
function Get(var M: TSparseMatrix; Row, Col: integer): Pointer;
procedure Delete(var M: TSparseMatrix; Row, Col: integer);
procedure Clear(var M: TSparseMatrix);

function GetNodeCount(var M: TSparseMatrix): integer;
function IsEmpty(var M: TSparseMatrix): boolean;
procedure GetDimensions(var M: TSparseMatrix; out Rows, Cols: integer);

procedure TraverseByRows(var M: TSparseMatrix; ProcessData: TTraverseProc);
procedure TraverseByCols(var M: TSparseMatrix; ProcessData: TTraverseProc);

procedure GenerateDotFile(var M: TSparseMatrix; const FileName: string;
  DataToString: TDataToString); overload;

procedure GenerateDotFile(var M: TSparseMatrix; const FileName: string;
  DataToString: TDataToString; RowLabels, ColLabels: TStringList); overload;

implementation

uses
  SysUtils;

procedure Init(var M: TSparseMatrix; MaxRows, MaxCols: integer);
begin
  M.RowHeaders := nil;
  M.ColHeaders := nil;
  M.MaxRows := MaxRows;
  M.MaxCols := MaxCols;
  M.NodeCount := 0;
end;

function FindRowHeader(var M: TSparseMatrix; Row: integer): PRowHeader;
var
  Temp: PRowHeader;
begin
  Temp := M.RowHeaders;
  while Temp <> nil do
  begin
    if Temp^.RowIndex = Row then
    begin
      Result := Temp;
      Exit;
    end;
    Temp := Temp^.NextRow;
  end;
  Result := nil;
end;

function FindColHeader(var M: TSparseMatrix; Col: integer): PColHeader;
var
  Temp: PColHeader;
begin
  Temp := M.ColHeaders;
  while Temp <> nil do
  begin
    if Temp^.ColIndex = Col then
    begin
      Result := Temp;
      Exit;
    end;
    Temp := Temp^.NextCol;
  end;
  Result := nil;
end;

function CreateRowHeader(var M: TSparseMatrix; Row: integer): PRowHeader;
var
  NewHeader, Temp, Prev: PRowHeader;
begin
  New(NewHeader);
  NewHeader^.RowIndex := Row;
  NewHeader^.FirstNode := nil;
  NewHeader^.NextRow := nil;

  if (M.RowHeaders = nil) or (M.RowHeaders^.RowIndex > Row) then
  begin
    NewHeader^.NextRow := M.RowHeaders;
    M.RowHeaders := NewHeader;
  end
  else
  begin
    Prev := M.RowHeaders;
    Temp := M.RowHeaders^.NextRow;
    while (Temp <> nil) and (Temp^.RowIndex < Row) do
    begin
      Prev := Temp;
      Temp := Temp^.NextRow;
    end;
    NewHeader^.NextRow := Temp;
    Prev^.NextRow := NewHeader;
  end;

  Result := NewHeader;
end;

function CreateColHeader(var M: TSparseMatrix; Col: integer): PColHeader;
var
  NewHeader, Temp, Prev: PColHeader;
begin
  New(NewHeader);
  NewHeader^.ColIndex := Col;
  NewHeader^.FirstNode := nil;
  NewHeader^.NextCol := nil;

  if (M.ColHeaders = nil) or (M.ColHeaders^.ColIndex > Col) then
  begin
    NewHeader^.NextCol := M.ColHeaders;
    M.ColHeaders := NewHeader;
  end
  else
  begin
    Prev := M.ColHeaders;
    Temp := M.ColHeaders^.NextCol;
    while (Temp <> nil) and (Temp^.ColIndex < Col) do
    begin
      Prev := Temp;
      Temp := Temp^.NextCol;
    end;
    NewHeader^.NextCol := Temp;
    Prev^.NextCol := NewHeader;
  end;

  Result := NewHeader;
end;

procedure Insert(var M: TSparseMatrix; Row, Col: integer; Data: Pointer);
var
  NewNode: PSparseNode;
  RowHdr: PRowHeader;
  ColHdr: PColHeader;
  Temp, Prev: PSparseNode;
begin
  if (Row < 0) or (Row >= M.MaxRows) or (Col < 0) or (Col >= M.MaxCols) then
    Exit;

  if Get(M, Row, Col) <> nil then
    Delete(M, Row, Col);

  New(NewNode);
  NewNode^.Row := Row;
  NewNode^.Col := Col;
  NewNode^.Data := Data;
  NewNode^.NextRow := nil;
  NewNode^.NextCol := nil;

  RowHdr := FindRowHeader(M, Row);
  if RowHdr = nil then
    RowHdr := CreateRowHeader(M, Row);

  ColHdr := FindColHeader(M, Col);
  if ColHdr = nil then
    ColHdr := CreateColHeader(M, Col);

  if (RowHdr^.FirstNode = nil) or (RowHdr^.FirstNode^.Col > Col) then
  begin
    NewNode^.NextRow := RowHdr^.FirstNode;
    RowHdr^.FirstNode := NewNode;
  end
  else
  begin
    Prev := RowHdr^.FirstNode;
    Temp := RowHdr^.FirstNode^.NextRow;
    while (Temp <> nil) and (Temp^.Col < Col) do
    begin
      Prev := Temp;
      Temp := Temp^.NextRow;
    end;
    NewNode^.NextRow := Temp;
    Prev^.NextRow := NewNode;
  end;

  if (ColHdr^.FirstNode = nil) or (ColHdr^.FirstNode^.Row > Row) then
  begin
    NewNode^.NextCol := ColHdr^.FirstNode;
    ColHdr^.FirstNode := NewNode;
  end
  else
  begin
    Prev := ColHdr^.FirstNode;
    Temp := ColHdr^.FirstNode^.NextCol;
    while (Temp <> nil) and (Temp^.Row < Row) do
    begin
      Prev := Temp;
      Temp := Temp^.NextCol;
    end;
    NewNode^.NextCol := Temp;
    Prev^.NextCol := NewNode;
  end;

  Inc(M.NodeCount);
end;

function Get(var M: TSparseMatrix; Row, Col: integer): Pointer;
var
  RowHdr: PRowHeader;
  Temp: PSparseNode;
begin
  Result := nil;
  if (Row < 0) or (Row >= M.MaxRows) or (Col < 0) or (Col >= M.MaxCols) then
    Exit;

  RowHdr := FindRowHeader(M, Row);
  if RowHdr = nil then
    Exit;

  Temp := RowHdr^.FirstNode;
  while Temp <> nil do
  begin
    if Temp^.Col = Col then
    begin
      Result := Temp^.Data;
      Exit;
    end;
    if Temp^.Col > Col then
      Exit;
    Temp := Temp^.NextRow;
  end;
end;

procedure Delete(var M: TSparseMatrix; Row, Col: integer);
var
  RowHdr: PRowHeader;
  ColHdr: PColHeader;
  NodeToDelete: PSparseNode;
  Temp, Prev: PSparseNode;
begin
  if (Row < 0) or (Row >= M.MaxRows) or (Col < 0) or (Col >= M.MaxCols) then
    Exit;

  RowHdr := FindRowHeader(M, Row);
  ColHdr := FindColHeader(M, Col);
  if (RowHdr = nil) or (ColHdr = nil) then
    Exit;

  NodeToDelete := nil;
  if RowHdr^.FirstNode^.Col = Col then
  begin
    NodeToDelete := RowHdr^.FirstNode;
    RowHdr^.FirstNode := RowHdr^.FirstNode^.NextRow;
  end
  else
  begin
    Prev := RowHdr^.FirstNode;
    Temp := RowHdr^.FirstNode^.NextRow;
    while Temp <> nil do
    begin
      if Temp^.Col = Col then
      begin
        NodeToDelete := Temp;
        Prev^.NextRow := Temp^.NextRow;
        Break;
      end;
      Prev := Temp;
      Temp := Temp^.NextRow;
    end;
  end;

  if NodeToDelete = nil then
    Exit;

  if ColHdr^.FirstNode = NodeToDelete then
    ColHdr^.FirstNode := NodeToDelete^.NextCol
  else
  begin
    Prev := ColHdr^.FirstNode;
    Temp := ColHdr^.FirstNode^.NextCol;
    while Temp <> nil do
    begin
      if Temp = NodeToDelete then
      begin
        Prev^.NextCol := Temp^.NextCol;
        Break;
      end;
      Prev := Temp;
      Temp := Temp^.NextCol;
    end;
  end;

  Dispose(NodeToDelete);
  Dec(M.NodeCount);
end;

procedure Clear(var M: TSparseMatrix);
var
  RowHdr, NextRowHdr: PRowHeader;
  ColHdr, NextColHdr: PColHeader;
  Node, NextNode: PSparseNode;
begin
  RowHdr := M.RowHeaders;
  while RowHdr <> nil do
  begin
    Node := RowHdr^.FirstNode;
    while Node <> nil do
    begin
      NextNode := Node^.NextRow;
      Dispose(Node);
      Node := NextNode;
    end;
    NextRowHdr := RowHdr^.NextRow;
    Dispose(RowHdr);
    RowHdr := NextRowHdr;
  end;

  ColHdr := M.ColHeaders;
  while ColHdr <> nil do
  begin
    NextColHdr := ColHdr^.NextCol;
    Dispose(ColHdr);
    ColHdr := NextColHdr;
  end;

  M.RowHeaders := nil;
  M.ColHeaders := nil;
  M.NodeCount := 0;
end;

function GetNodeCount(var M: TSparseMatrix): integer;
begin
  Result := M.NodeCount;
end;

function IsEmpty(var M: TSparseMatrix): boolean;
begin
  Result := M.NodeCount = 0;
end;

procedure GetDimensions(var M: TSparseMatrix; out Rows, Cols: integer);
begin
  Rows := M.MaxRows;
  Cols := M.MaxCols;
end;

procedure TraverseByRows(var M: TSparseMatrix; ProcessData: TTraverseProc);
var
  RowHdr: PRowHeader;
  Node: PSparseNode;
begin
  RowHdr := M.RowHeaders;
  while RowHdr <> nil do
  begin
    Node := RowHdr^.FirstNode;
    while Node <> nil do
    begin
      ProcessData(Node^.Row, Node^.Col, Node^.Data);
      Node := Node^.NextRow;
    end;
    RowHdr := RowHdr^.NextRow;
  end;
end;

procedure TraverseByCols(var M: TSparseMatrix; ProcessData: TTraverseProc);
var
  ColHdr: PColHeader;
  Node: PSparseNode;
begin
  ColHdr := M.ColHeaders;
  while ColHdr <> nil do
  begin
    Node := ColHdr^.FirstNode;
    while Node <> nil do
    begin
      ProcessData(Node^.Row, Node^.Col, Node^.Data);
      Node := Node^.NextCol;
    end;
    ColHdr := ColHdr^.NextCol;
  end;
end;

procedure GenerateDotFile(var M: TSparseMatrix; const FileName: string;
  DataToString: TDataToString);
begin
  GenerateDotFile(M, FileName, DataToString, nil, nil);
end;

procedure GenerateDotFile(var M: TSparseMatrix; const FileName: string;
  DataToString: TDataToString; RowLabels, ColLabels: TStringList);
var
  F: TextFile;
  RowHdr, PrevRowHdr: PRowHeader;
  ColHdr, PrevColHdr: PColHeader;
  Node: PSparseNode;
  RowLabel, ColLabel: string;
begin
  AssignFile(F, FileName);
  Rewrite(F);
  try
    WriteLn(F, 'digraph SparseMatrix {');
    WriteLn(F, '    graph [splines=ortho, nodesep=0.6, ranksep=1.0, bgcolor=transparent];');
    WriteLn(F,
      '    node [shape=box, style="filled,rounded", fontname="Segoe UI", fontsize=10, margin=0.25];');
    WriteLn(F, '    edge [penwidth=1.5, arrowsize=0.8];');
    WriteLn(F, '');

    if IsEmpty(M) then
    begin
      WriteLn(F, '    empty [label="Matriz Dispersa Vacía\n' +
        Format('%dx%d', [M.MaxRows, M.MaxCols]) +
        '", shape=ellipse, style="filled", fillcolor="#f093fb:#f5576c", gradientangle=90, fontcolor="#FFFFFF"];');
    end
    else
    begin
      WriteLn(F, '    M_root [style=invis, width=0, height=0, shape=point];');
      WriteLn(F, '');

      RowHdr := M.RowHeaders;
      while RowHdr <> nil do
      begin
        if (RowLabels <> nil) and (RowHdr^.RowIndex < RowLabels.Count) then
          RowLabel := AnsiQuotedStr(RowLabels[RowHdr^.RowIndex], '"')
        else
          RowLabel := AnsiQuotedStr('Fila ' + IntToStr(RowHdr^.RowIndex), '"');

        WriteLn(F, Format(
          '    row_hdr_%d [label=%s, fillcolor="#2196f3", fontcolor="#FFFFFF"];',
          [RowHdr^.RowIndex, RowLabel]));
        RowHdr := RowHdr^.NextRow;
      end;
      WriteLn(F, '');

      ColHdr := M.ColHeaders;
      while ColHdr <> nil do
      begin
        if (ColLabels <> nil) and (ColHdr^.ColIndex < ColLabels.Count) then
          ColLabel := AnsiQuotedStr(ColLabels[ColHdr^.ColIndex], '"')
        else
          ColLabel := AnsiQuotedStr('Col ' + IntToStr(ColHdr^.ColIndex), '"');

        WriteLn(F, Format(
          '    col_hdr_%d [label=%s, fillcolor="#4caf50", fontcolor="#FFFFFF"];',
          [ColHdr^.ColIndex, ColLabel]));
        ColHdr := ColHdr^.NextCol;
      end;
      WriteLn(F, '');

      RowHdr := M.RowHeaders;
      while RowHdr <> nil do
      begin
        Node := RowHdr^.FirstNode;
        while Node <> nil do
        begin
          WriteLn(F, Format(
            '    node_%d_%d [label="[%d,%d]\n%s", fillcolor="#667eea:#764ba2", gradientangle=45, fontcolor="#FFFFFF", fontsize=12];',
            [Node^.Row, Node^.Col, Node^.Row, Node^.Col, DataToString(Node^.Data)]));
          Node := Node^.NextRow;
        end;
        RowHdr := RowHdr^.NextRow;
      end;
      WriteLn(F, '');

      Write(F, '    { rank=same; M_root; ');
      ColHdr := M.ColHeaders;
      while ColHdr <> nil do
      begin
        Write(F, Format('col_hdr_%d; ', [ColHdr^.ColIndex]));
        ColHdr := ColHdr^.NextCol;
      end;
      WriteLn(F, '}');
      RowHdr := M.RowHeaders;
      while RowHdr <> nil do
      begin
        Write(F, Format('    { rank=same; row_hdr_%d; ', [RowHdr^.RowIndex]));
        Node := RowHdr^.FirstNode;
        while Node <> nil do
        begin
          Write(F, Format('node_%d_%d; ', [Node^.Row, Node^.Col]));
          Node := Node^.NextRow;
        end;
        WriteLn(F, '}');
        RowHdr := RowHdr^.NextRow;
      end;
      WriteLn(F, '');
      PrevColHdr := nil;
      ColHdr := M.ColHeaders;
      if ColHdr <> nil then
      begin
        WriteLn(F, Format('    M_root -> col_hdr_%d [style=invis];',
          [ColHdr^.ColIndex]));
        PrevColHdr := ColHdr;
        ColHdr := ColHdr^.NextCol;
      end;
      while ColHdr <> nil do
      begin
        WriteLn(F, Format('    col_hdr_%d -> col_hdr_%d [style=invis];',
          [PrevColHdr^.ColIndex, ColHdr^.ColIndex]));
        PrevColHdr := ColHdr;
        ColHdr := ColHdr^.NextCol;
      end;
      WriteLn(F, '');
      PrevRowHdr := nil;
      RowHdr := M.RowHeaders;
      if RowHdr <> nil then
      begin
        WriteLn(F, Format('    M_root -> row_hdr_%d [style=invis];',
          [RowHdr^.RowIndex]));
        PrevRowHdr := RowHdr;
        RowHdr := RowHdr^.NextRow;
      end;
      while RowHdr <> nil do
      begin
        WriteLn(F, Format('    row_hdr_%d -> row_hdr_%d [style=invis];',
          [PrevRowHdr^.RowIndex, RowHdr^.RowIndex]));
        PrevRowHdr := RowHdr;
        RowHdr := RowHdr^.NextRow;
      end;
      WriteLn(F, '');
      RowHdr := M.RowHeaders;
      while RowHdr <> nil do
      begin
        if RowHdr^.FirstNode <> nil then
        begin
          WriteLn(F, Format(
            '    row_hdr_%d -> node_%d_%d [color="#2196f3", constraint=false];',
            [RowHdr^.RowIndex, RowHdr^.FirstNode^.Row, RowHdr^.FirstNode^.Col]));
          Node := RowHdr^.FirstNode;
          while Node^.NextRow <> nil do
          begin
            WriteLn(F, Format(
              '    node_%d_%d -> node_%d_%d [color="#ff9800", constraint=false];',
              [Node^.Row, Node^.Col, Node^.NextRow^.Row, Node^.NextRow^.Col]));
            Node := Node^.NextRow;
          end;
        end;
        RowHdr := RowHdr^.NextRow;
      end;
      WriteLn(F, '');
      ColHdr := M.ColHeaders;
      while ColHdr <> nil do
      begin
        if ColHdr^.FirstNode <> nil then
        begin
          WriteLn(F, Format('    col_hdr_%d -> node_%d_%d [color="#4caf50"];',
            [ColHdr^.ColIndex, ColHdr^.FirstNode^.Row, ColHdr^.FirstNode^.Col]));
          Node := ColHdr^.FirstNode;
          while Node^.NextCol <> nil do
          begin
            WriteLn(F, Format('    node_%d_%d -> node_%d_%d [color="#9c27b0"];',
              [Node^.Row, Node^.Col, Node^.NextCol^.Row, Node^.NextCol^.Col]));
            Node := Node^.NextCol;
          end;
        end;
        ColHdr := ColHdr^.NextCol;
      end;
      WriteLn(F, '');
      WriteLn(F, Format(
        '    info [label="Matriz Dispersa\nDimensiones: %dx%d\nElementos: %d", shape=note, style="filled", fillcolor="#ffeaa7", fontcolor="#2d3436"];', [M.MaxRows, M.MaxCols, M.NodeCount]));
    end;

    WriteLn(F, '}');
  finally
    CloseFile(F);
  end;
end;

end.
