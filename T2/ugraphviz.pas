unit uGraphviz;

{$mode objfpc}{$H+}
{$codepage utf8}

interface

uses
  SysUtils, Process, uBST;

procedure WriteDot(const Root: PNode; const DotFile: string);
function RenderDotToPNG(const DotFile, PngFile: string): integer;
procedure BuildGraphvizPNG(const Root: PNode; const DotFile, PngFile: string);

implementation

function EscapeLabel(const S: unicodestring): unicodestring;
var
  i: SizeInt;
  c: widechar;
begin
  Result := '';
  for i := 1 to Length(S) do
  begin
    c := S[i];
    case c of
      '"': Result += '\"';
      '\': Result += '\\';
      #10, #13: Result += '\n';
      '|', '{', '}', '<', '>': Result += '\' + c;
      else
        Result += c;
    end;
  end;
end;

procedure WriteNodeAndEdges(const N: PNode; var F: Text);
begin
  if N = nil then Exit;
  WriteLn(F, Format('  n%d [label="%d\n%s %s\n%s"];',
    [N^.id, N^.id, EscapeLabel(N^.first_name), EscapeLabel(N^.last_name),
    EscapeLabel(N^.email)]));

  if N^.left <> nil then
  begin
    WriteLn(F, Format('  n%d -> n%d [label="L"];', [N^.id, N^.left^.id]));
    WriteNodeAndEdges(N^.left, F);
  end;
  if N^.right <> nil then
  begin
    WriteLn(F, Format('  n%d -> n%d [label="R"];', [N^.id, N^.right^.id]));
    WriteNodeAndEdges(N^.right, F);
  end;
end;

procedure WriteDot(const Root: PNode; const DotFile: string);
var
  F: Text;
begin
  Assign(F, DotFile);
  Rewrite(F);
  try
    WriteLn(F, 'digraph BST {');
    WriteLn(F, '  graph [rankdir=TB, splines=true, nodesep="0.35", ranksep="0.6"];');
    WriteLn(F,
      '  node  [shape=box, style="rounded,filled", fillcolor="#eef5ff", color="#5b7bb2", fontname="Helvetica"];');
    WriteLn(F, '  edge  [color="#7c8fb3", arrowsize=0.8, fontname="Helvetica"];');

    if Root <> nil then
      WriteNodeAndEdges(Root, F)
    else
      WriteLn(F, '  // Árbol vacío');

    WriteLn(F, '}');
  finally
    Close(F);
  end;
end;

function RenderDotToPNG(const DotFile, PngFile: string): integer;
var
  Proc: TProcess;
begin
  Result := -1;
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := 'dot';
    Proc.Parameters.Clear;
    Proc.Parameters.Add('-Tpng');
    Proc.Parameters.Add(DotFile);
    Proc.Parameters.Add('-o');
    Proc.Parameters.Add(PngFile);
    Proc.Options := [poUsePipes, poWaitOnExit];
    try
      Proc.Execute;
      Result := Proc.ExitStatus;
    except
      on E: Exception do
      begin
        Result := -1;
      end;
    end;
  finally
    Proc.Free;
  end;
end;

procedure BuildGraphvizPNG(const Root: PNode; const DotFile, PngFile: string);
var
  Code: integer;
begin
  WriteDot(Root, DotFile);
  Code := RenderDotToPNG(DotFile, PngFile);
  if Code <> 0 then
    WriteLn(Format(
      'Aviso: no se pudo renderizar PNG (exit code %d). Verifique Graphviz en PATH. Se generó %s',
      [Code, DotFile]))
  else
    WriteLn(Format('OK: imagen generada -> %s', [PngFile]));
end;

end.
