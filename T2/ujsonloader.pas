unit uJsonLoader;

{$mode objpc}{$H+}
{$codepage utf8}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, uBST;

procedure LoadUsersFromJSON(const FileName: string; var Root: PNode);

implementation

procedure LoadUsersFromJSON(const FileName: string; var Root: PNode);
var
  FS: TFileStream;
  Parser: TJSONParser;
  Data: TJSONData;
  Arr: TJSONArray;
  I: integer;
  Obj: TJSONObject;
  IdVal: integer;
  FName, LName, Email: unicodestring;
  Node: PNode;
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt('No se encontró el archivo JSON: %s', [FileName]);

  FS := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    Parser := TJSONParser.Create(FS);
    try
      Data := Parser.Parse;
      try
        if (Data = nil) or (Data.JSONType <> jtArray) then
          raise Exception.Create('El JSON raíz debe ser un arreglo de objetos.');
        Arr := TJSONArray(Data);
        for I := 0 to Arr.Count - 1 do
        begin
          if Arr.Items[I].JSONType <> jtObject then
            Continue;
          Obj := TJSONObject(Arr.Items[I]);

          IdVal := Obj.Get('id', -1);

          FName := Obj.Get('first_name', '');
          LName := Obj.Get('last_name', '');
          Email := Obj.Get('email', '');

          if IdVal < 0 then
            Continue;

          Node := NewNode(IdVal, FName, LName, Email);
          Insert(Root, Node);
        end;
      finally
        Data.Free;
      end;
    finally
      Parser.Free;
    end;
  finally
    FS.Free;
  end;
end;

end.
