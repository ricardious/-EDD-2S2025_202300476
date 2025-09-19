program T2Main;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  SysUtils,
  Classes,
  uBST,
  uJsonLoader,
  uGraphviz;

const
  OUTPUT_DIR = 'T2';
  JSON_FILE = OUTPUT_DIR + DirectorySeparator + 'data.json';
  DOT_FILE = OUTPUT_DIR + DirectorySeparator + 'bst.dot';
  PNG_FILE = OUTPUT_DIR + DirectorySeparator + 'bst.png';

  SAMPLE_JSON =
    '[{"id":35,"first_name":"Chicky","last_name":"Egle of Germany","email":"cegleofgermany0@paginegialle.it"},'
    +
    ' {"id":60,"first_name":"Norine","last_name":"Sindle","email":"nsindle1@wp.com"},' +
    ' {"id":22,"first_name":"Elane","last_name":"Antonik","email":"eantonik2@army.mil"},'
    +
    ' {"id":16,"first_name":"Thomasa","last_name":"Olphert","email":"tolphert3@senate.gov"},'
    + ' {"id":50,"first_name":"Linzy","last_name":"Kerne","email":"lkerne4@globo.com"},'
    +
    ' {"id":13,"first_name":"Tamas","last_name":"Hedney","email":"thedney5@deliciousdays.com"},'
    + ' {"id":49,"first_name":"Carrissa","last_name":"Elwyn","email":"celwyn6@uol.com.br"},'
    + ' {"id":43,"first_name":"Lexine","last_name":"Dempsey","email":"ldempsey7@pcworld.com"},'
    + ' {"id":23,"first_name":"Kennedy","last_name":"Beedham","email":"kbeedham8@jimdo.com"},'
    + ' {"id":19,"first_name":"Saxe","last_name":"Spencelayh","email":"sspencelayh9@xrea.com"},'
    + ' {"id":1,"first_name":"Stanton","last_name":"Whiston","email":"swhistona@amazon.de"},'
    + ' {"id":82,"first_name":"Lyndell","last_name":"Jeune","email":"ljeuneb@meetup.com"},'
    + ' {"id":28,"first_name":"Enoch","last_name":"Flanders","email":"eflandersc@alexa.com"},'
    + ' {"id":30,"first_name":"Ramsay","last_name":"Dallin","email":"rdallind@imageshack.us"},'
    + ' {"id":14,"first_name":"Elwood","last_name":"Corwood","email":"ecorwoode@dagondesign.com"}]';

  procedure EnsureOutputDir;
  begin
    if not DirectoryExists(OUTPUT_DIR) then
      if not ForceDirectories(OUTPUT_DIR) then
        raise Exception.CreateFmt('No se pudo crear el directorio: %s', [OUTPUT_DIR]);
  end;

  procedure EnsureSampleJSONFile;
  var
    FS: TFileStream;
    S: rawbytestring;
  begin
    if FileExists(JSON_FILE) then Exit;
    FS := TFileStream.Create(JSON_FILE, fmCreate);
    try
      S := UTF8Encode(SAMPLE_JSON);
      FS.WriteBuffer(Pointer(S)^, Length(S));
    finally
      FS.Free;
    end;
    WriteLn('Archivo de ejemplo creado: ', JSON_FILE);
  end;

var
  Root: PNode = nil;

  procedure PrintNode(const N: PNode);
  begin
    WriteLn(Format('(%d) %s %s < %s >', [N^.id, N^.first_name, N^.last_name, N^.email]));
  end;

begin
  try
    EnsureOutputDir;
    EnsureSampleJSONFile;

    LoadUsersFromJSON(JSON_FILE, Root);
    WriteLn('Total de nodos en el BST: ', CountNodes(Root));

    InOrder(Root, @PrintNode);

    BuildGraphvizPNG(Root, DOT_FILE, PNG_FILE);

    WriteLn('Listo. Archivos generados en la carpeta "', OUTPUT_DIR, '".');
    WriteLn('  - ', DOT_FILE);
    WriteLn('  - ', PNG_FILE);

  except
    on E: Exception do
    begin
      WriteLn('Error: ', E.Message);
    end;
  end;

  FreeTree(Root);

  WriteLn('Presiona ENTER para salir...');
  ReadLn;
end.
