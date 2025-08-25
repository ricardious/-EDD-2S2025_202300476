program EDDMail;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, datetimectrls, FormLogin, User, Email, UserService, SinglyLinkedList,
  DoublyLinkedList, CircularLinkedList, Queue, Stack, AppState,
  AuthService, FormDashboardUser, JsonUsersLoader;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  {$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar:=True;
  {$POP}
  Application.Initialize;
  InitAppState;
  BootstrapRoot(Users);
  Application.CreateForm(TDashboardUser, DashboardUser);
  Application.Run;
end.

