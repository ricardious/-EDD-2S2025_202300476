unit AuthService;

{$mode ObjFPC}{$H+}

interface

uses
  User, SinglyLinkedList, UserService;

function SignIn(const Users: TSinglyLinkedList;
  const Email, Password: ansistring): PUser;

implementation

function SignIn(const Users: TSinglyLinkedList;
  const Email, Password: ansistring): PUser;
var
  U: PUser;
begin
  U := FindUserByEmail(Users, Email);
  if (U <> nil) and (U^.Password = Password) then
    Result := U
  else
    Result := nil;
end;

end.
