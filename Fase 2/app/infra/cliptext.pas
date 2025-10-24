unit ClipText;

{$mode ObjFPC}{$H+}

interface

uses
  SysUtils;

function Clip(const S: string; MaxLen: integer; const Suffix: string = '...'): string;

function ClipAtWord(const S: string; MaxLen: integer;
  const Suffix: string = '...'): string;

function ClipMiddle(const S: string; MaxLen: integer;
  const Suffix: string = '...'): string;

implementation

function Clip(const S: string; MaxLen: integer; const Suffix: string): string;
begin
  if MaxLen <= 0 then Exit('');
  if Length(S) <= MaxLen then Exit(S);
  if Length(Suffix) >= MaxLen then Exit(Copy(Suffix, 1, MaxLen));
  Result := Copy(S, 1, MaxLen - Length(Suffix)) + Suffix;
end;

function ClipAtWord(const S: string; MaxLen: integer; const Suffix: string): string;
var
  cutPos, i: integer;
begin
  if MaxLen <= 0 then Exit('');
  if Length(S) <= MaxLen then Exit(S);
  if Length(Suffix) >= MaxLen then Exit(Copy(Suffix, 1, MaxLen));

  cutPos := MaxLen - Length(Suffix);
  if cutPos < 1 then Exit(Copy(Suffix, 1, MaxLen));

  i := cutPos;
  while (i > 1) and (S[i] <> ' ') do Dec(i);

  if i > 1 then
    Result := TrimRight(Copy(S, 1, i - 1)) + Suffix
  else
    Result := Copy(S, 1, cutPos) + Suffix;
end;

function ClipMiddle(const S: string; MaxLen: integer; const Suffix: string): string;
var
  keepLeft, keepRight: integer;
begin
  if MaxLen <= 0 then Exit('');
  if Length(S) <= MaxLen then Exit(S);
  if Length(Suffix) >= MaxLen then Exit(Copy(Suffix, 1, MaxLen));

  keepLeft := (MaxLen - Length(Suffix)) div 2;
  keepRight := (MaxLen - Length(Suffix)) - keepLeft;

  Result := Copy(S, 1, keepLeft) + Suffix + Copy(S, Length(S) -
    keepRight + 1, keepRight);
end;

end.
