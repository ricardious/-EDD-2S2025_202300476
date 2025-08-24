unit JsonUsersLoader;
{$mode ObjFPC}{$H+}

interface
uses
  Classes, SysUtils, fpjson, jsonparser, User, SinglyLinkedList, UserService;

function LoadUsersFromJson(const Path: AnsiString; var Users: TSinglyLinkedList): Integer;
function LoadUsersFromJsonContent(const Content: AnsiString; var Users: TSinglyLinkedList): Integer;

implementation

function ReadFileContent(const FileName: string): string;
var
  StringList: TStringList;
begin
  StringList := TStringList.Create;
  try
    StringList.LoadFromFile(FileName);
    Result := StringList.Text;
  finally
    StringList.Free;
  end;
end;

function LoadUsersFromJson(const Path: AnsiString; var Users: TSinglyLinkedList): Integer;
var
  JSON: TJSONData;
  Obj, U: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Rec: TUser;
  Content: string;
  LoadedCount: Integer;
begin
  Result := 0;
  LoadedCount := 0;

  if not FileExists(Path) then Exit;

  try
    Content := ReadFileContent(Path);
    JSON := GetJSON(Content);
    try
      if JSON is TJSONObject then
      begin
        Obj := TJSONObject(JSON);
        if Obj.Find('usuarios') <> nil then
        begin
          Arr := Obj.Arrays['usuarios'];
          for I := 0 to Arr.Count - 1 do
          begin
            if Arr.Items[I] is TJSONObject then
            begin
              U := Arr.Objects[I];
              Rec.Id       := U.Integers['id'];
              Rec.Name     := U.Strings['nombre'];
              Rec.Username := U.Strings['usuario'];
              Rec.Password := U.Strings['password'];
              Rec.Email    := U.Strings['email'];
              Rec.Phone    := U.Strings['telefono'];

              if (FindUserById(Users, Rec.Id) <> nil) then
                raise Exception.CreateFmt('Duplicate ID found: %d', [Rec.Id]);

              if (FindUserByEmail(Users, Rec.Email) <> nil) then
                raise Exception.CreateFmt('Duplicate email found: %s', [Rec.Email]);

              AddUser(Users, Rec);
              Inc(LoadedCount);
            end;
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
    Result := LoadedCount;
  except
    on E: Exception do
    begin
      Result := LoadedCount;
      raise;
    end;
  end;
end;

function LoadUsersFromJsonContent(const Content: AnsiString; var Users: TSinglyLinkedList): Integer;
var
  JSON: TJSONData;
  Obj, U: TJSONObject;
  Arr: TJSONArray;
  I: Integer;
  Rec: TUser;
  LoadedCount: Integer;
begin
  Result := 0;
  LoadedCount := 0;

  try
    JSON := GetJSON(Content);
    try
      if JSON is TJSONObject then
      begin
        Obj := TJSONObject(JSON);
        if Obj.Find('usuarios') <> nil then
        begin
          Arr := Obj.Arrays['usuarios'];
          for I := 0 to Arr.Count - 1 do
          begin
            if Arr.Items[I] is TJSONObject then
            begin
              U := Arr.Objects[I];
              Rec.Id       := U.Integers['id'];
              Rec.Name     := U.Strings['nombre'];
              Rec.Username := U.Strings['usuario'];
              Rec.Password := U.Strings['password'];
              Rec.Email    := U.Strings['email'];
              Rec.Phone    := U.Strings['telefono'];

              if (FindUserById(Users, Rec.Id) <> nil) then
                raise Exception.CreateFmt('Duplicate ID found: %d', [Rec.Id]);
              if (FindUserByEmail(Users, Rec.Email) <> nil) then
                raise Exception.CreateFmt('Duplicate email found: %s', [Rec.Email]);

              AddUser(Users, Rec);
              Inc(LoadedCount);
            end;
          end;
        end;
      end;
    finally
      JSON.Free;
    end;
    Result := LoadedCount;
  except
    on E: Exception do
    begin
      Result := LoadedCount;
      raise;
    end;
  end;
end;

end.
