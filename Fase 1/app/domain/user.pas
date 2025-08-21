unit User;

{$mode ObjFPC}{$H+}

interface

type
  PUser = ^TUser;
  TUser = record
    Id: LongInt;
    Name: AnsiString;
    Username: AnsiString;
    Password: AnsiString;
    Email: AnsiString;
    Phone: AnsiString;
  end;

implementation

end.

