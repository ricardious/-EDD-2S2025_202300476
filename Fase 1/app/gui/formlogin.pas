unit FormLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TLogin }

  TLogin = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    procedure Edit2Change(Sender: TObject);
  private

  public

  end;

var
  Login: TLogin;

implementation

{$R *.lfm}

{ TLogin }

procedure TLogin.Edit2Change(Sender: TObject);
begin
     Edit2.PasswordChar := '*';
end;

end.

