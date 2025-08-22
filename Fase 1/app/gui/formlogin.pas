unit FormLogin;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, BGRACustomDrawn;

type

  { TSignIn }

  TSignIn = class(TForm)
    BtnSignIn: TBCButton;
    EditEmail: TEdit;
    EditPsw: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    LblSignUp: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    procedure BtnSignInClick(Sender: TObject);
    procedure EditPswChange(Sender: TObject);
    procedure LblSignUpClick(Sender: TObject);
    procedure LblSignUpMouseEnter(Sender: TObject);
    procedure LblSignUpMouseLeave(Sender: TObject);
  private

  public

  end;

var
  SignIn: TSignIn;

implementation

uses
  AppState, UserService, User, FormRegister;

{$R *.lfm}

{ TSignIn }

procedure TSignIn.EditPswChange(Sender: TObject);
begin
     EditPsw.PasswordChar := '*';
end;

procedure TSignIn.LblSignUpClick(Sender: TObject);
begin
  Self.Hide;
  if Assigned(FormRegister.SignUp) then
    FormRegister.SignUp.Show
  else
  begin
    Application.CreateForm(TSignUp, SignUp);
    SignUp.Show;
  end;
end;

procedure TSignIn.BtnSignInClick(Sender: TObject);
var
  email, password: AnsiString;
  foundUser: PUser;
begin
  email := EditEmail.Text;
  password := EditPsw.Text;

  if (Trim(email) = '') or (Trim(password) = '') then
  begin
    ShowMessage('Please enter your email and password.');
    Exit;
  end;

  foundUser := FindUserByEmail(Users, email);

  if foundUser <> nil then
  begin
    if foundUser^.Password = password then
    begin
      ShowMessage('Welcome, ' + foundUser^.Name + '!');
    end
    else
    begin
      ShowMessage('The password is incorrect. Please try again.');
    end;
  end
  else
  begin
    ShowMessage('The user with email "' + email + '" was not found.');
  end;
end;

procedure TSignIn.LblSignUpMouseEnter(Sender: TObject);
begin
     LblSignUp.Font.Color := TColor($00FF3C00);
     LblSignUp.Font.Style := [fsUnderline];
     LblSignUp.Cursor := crHandPoint;
end;

procedure TSignIn.LblSignUpMouseLeave(Sender: TObject);
begin
     LblSignUp.Font.Color := TColor($00FF3C00);
     LblSignUp.Font.Style := [];
     LblSignUp.Cursor := crDefault;
end;

end.

