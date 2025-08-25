unit JsonUsersLoader;
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpjson, jsonparser, User, SinglyLinkedList,
  UserService, DoublyLinkedList, Stack, Queue, CircularLinkedList;

function LoadUsersFromJson(const Path: ansistring;
  var Users: TSinglyLinkedList): integer;
function LoadUsersFromJsonContent(const Content: ansistring;
  var Users: TSinglyLinkedList): integer;

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

function LoadUsersFromJson(const Path: ansistring;
  var Users: TSinglyLinkedList): integer;
var
  JSON: TJSONData;
  Obj, U: TJSONObject;
  Arr: TJSONArray;
  I: integer;
  Rec: TUser;
  Content: string;
  LoadedCount: integer;
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
              Rec.Id := U.Integers['id'];
              Rec.Name := U.Strings['nombre'];
              Rec.Username := U.Strings['usuario'];
              Rec.Password := U.Strings['password'];
              Rec.Email := U.Strings['email'];
              Rec.Phone := U.Strings['telefono'];

              DoublyLinkedList.Init(Rec.Inbox);
              Stack.Init(Rec.Trash);
              Queue.Init(Rec.ScheduledMail);
              CircularLinkedList.Init(Rec.Contacts);

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

function LoadUsersFromJsonContent(const Content: ansistring;
  var Users: TSinglyLinkedList): integer;
var
  JSON: TJSONData;
  Obj, U: TJSONObject;
  Arr: TJSONArray;
  I: integer;
  Rec: TUser;
  LoadedCount: integer;
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
              Rec.Id := U.Integers['id'];
              Rec.Name := U.Strings['nombre'];
              Rec.Username := U.Strings['usuario'];
              Rec.Password := U.Strings['password'];
              Rec.Email := U.Strings['email'];
              Rec.Phone := U.Strings['telefono'];

              DoublyLinkedList.Init(Rec.Inbox);
              Stack.Init(Rec.Trash);
              Queue.Init(Rec.ScheduledMail);
              CircularLinkedList.Init(Rec.Contacts);

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
