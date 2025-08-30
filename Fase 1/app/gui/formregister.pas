unit FormRegister;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCButton, BGRACustomDrawn;

type

  { TSignUp }

  TSignUp = class(TForm)
    BtnSignUp: TBCButton;
    EditName: TEdit;
    EditUsername: TEdit;
    EditEmail: TEdit;
    EditPhone: TEdit;
    EditPsw: TEdit;
    EditConfirmPsw: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Label1: TLabel;
    LblSignIn: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    procedure BtnSignUpClick(Sender: TObject);
    procedure EditConfirmPswChange(Sender: TObject);
    procedure EditPswChange(Sender: TObject);
    procedure LblSignInClick(Sender: TObject);
    procedure LblSignInMouseEnter(Sender: TObject);
    procedure LblSignInMouseLeave(Sender: TObject);
  private
    function IsValidEmail(const S: ansistring): boolean;
    procedure ClearFields;
  public

  end;

var
  SignUp: TSignUp;

implementation

uses
  AppState, UserService, User, FormLogin;

  {$R *.lfm}

  { TSignUp }

function TSignUp.IsValidEmail(const S: ansistring): boolean;
begin
  Result := (Pos('@', S) > 1) and (Pos('.', S) > 1) and (Length(S) >= 5);
end;

procedure TSignUp.ClearFields;
begin
  EditName.Clear;
  EditUsername.Clear;
  EditEmail.Clear;
  EditPhone.Clear;
  EditPsw.Clear;
  EditConfirmPsw.Clear;
end;

procedure TSignUp.BtnSignUpClick(Sender: TObject);
var
  newUser: TUser;
  confirmPsw: ansistring;
begin
  newUser.Name := Trim(EditName.Text);
  newUser.Username := Trim(EditUsername.Text);
  newUser.Email := Trim(EditEmail.Text);
  newUser.Phone := Trim(EditPhone.Text);
  newUser.Password := EditPsw.Text;
  confirmPsw := EditConfirmPsw.Text;

  if (newUser.Name = '') or (newUser.Username = '') or (newUser.Email = '') or
    (newUser.Password = '') then
  begin
    ShowMessage('All fields are required.');
    Exit;
  end;

  if newUser.Password <> confirmPsw then
  begin
    ShowMessage('Passwords do not match.');
    Exit;
  end;

  if FindUserByEmail(Users, newUser.Email) <> nil then
  begin
    ShowMessage('The email "' + newUser.Email + '" is already registered.');
    Exit;
  end;

  newUser.Id := -1;
  AddUser(Users, newUser);

  ShowMessage('Registration successful! You can now sign in.');
  Self.Close;
  if Assigned(FormLogin.SignIn) then
  begin
    FormLogin.SignIn.EditEmail.Text := newUser.Email;
    FormLogin.SignIn.Show;
  end
  else
    ShowMessage('⚠️ Login form is not available.');
end;


procedure TSignUp.EditConfirmPswChange(Sender: TObject);
begin
  EditConfirmPsw.PasswordChar := '*';
end;

procedure TSignUp.EditPswChange(Sender: TObject);
begin
  EditPsw.PasswordChar := '*';
end;

procedure TSignUp.LblSignInClick(Sender: TObject);
begin
  Self.Hide;
  if Assigned(SignIn) then
    SignIn.Show;
end;

procedure TSignUp.LblSignInMouseEnter(Sender: TObject);
begin
  LblSignIn.Font.Color := TColor($00FF3C00);
  LblSignIn.Font.Style := [fsUnderline];
  LblSignIn.Cursor := crHandPoint;
end;

procedure TSignUp.LblSignInMouseLeave(Sender: TObject);
begin
  LblSignIn.Font.Color := TColor($00FF3C00);
  LblSignIn.Font.Style := [];
  LblSignIn.Cursor := crDefault;
end;


end.
