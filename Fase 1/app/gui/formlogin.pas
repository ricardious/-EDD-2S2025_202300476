unit FormLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, BGRACustomDrawn;

type

  { TLogin }

  TLogin = class(TForm)
    BCButton1: TBCButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    LblSignUp: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    procedure Edit2Change(Sender: TObject);
    procedure LblSignUpMouseEnter(Sender: TObject);
    procedure LblSignUpMouseLeave(Sender: TObject);
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

procedure TLogin.LblSignUpMouseEnter(Sender: TObject);
begin
     LblSignUp.Font.Color := TColor($00FF3C00);
     LblSignUp.Font.Style := [fsUnderline];
     LblSignUp.Cursor := crHandPoint;
end;

procedure TLogin.LblSignUpMouseLeave(Sender: TObject);
begin
     LblSignUp.Font.Color := TColor($00FF3C00);
     LblSignUp.Font.Style := [];
     LblSignUp.Cursor := crDefault;
end;

end.

