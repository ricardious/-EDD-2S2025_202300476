unit DotUtils;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function DotEscape(const S: string): string;
function DotTrunc(const S: string; MaxLen: integer): string;
function BoolToYesNo(B: boolean): string;
function EmailStateToText(S: integer): string;

implementation

function DotEscape(const S: string): string;
var
  R: string;
begin
  R := StringReplace(S, '\', '\\', [rfReplaceAll]);
  R := StringReplace(R, '"', '\"', [rfReplaceAll]);
  R := StringReplace(R, #13#10, '\n', [rfReplaceAll]);
  R := StringReplace(R, #10, '\n', [rfReplaceAll]);
  R := StringReplace(R, #13, '\n', [rfReplaceAll]);
  Result := R;
end;

function DotTrunc(const S: string; MaxLen: integer): string;
begin
  if (MaxLen > 0) and (Length(S) > MaxLen) then
    Result := Copy(S, 1, MaxLen - 1) + '…'   // ASCII: '...'
  else
    Result := S;
end;

function BoolToYesNo(B: boolean): string;
begin
  if B then Result := 'Yes'
  else
    Result := 'No';
end;

function EmailStateToText(S: integer): string;
begin
  // TEmailState = (esUnread, esRead)
  case S of
    0: Result := 'Unread';
    1: Result := 'Read';
    else
      Result := 'Unknown';
  end;
end;

end.
