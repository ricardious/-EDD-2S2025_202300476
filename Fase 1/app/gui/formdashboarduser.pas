unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, MaskEdit, Grids, DateTimePicker, BCMDButton, ATShapeLineBGRA,
  BCLabel, BCRoundedImage, BGRACustomDrawn, BCButton, BGRAThemeButton,
  BCMDButtonFocus, JsonUsersLoader, fpjson, jsonparser, AppState,
  FormLogin, ContactService, CircularLinkedList, User;

type

  { TDashboardUser }

  TDashboardUser = class(TForm)
    BtnAddContact: TBCMDButtonFocus;
    BtnDelete1: TBCMDButton;
    BtnLogout: TBCMDButton;
    BtnNext: TBCMDButton;
    BtnPrev: TBCMDButton;
    BtnSelectFile4: TBCMDButton;
    BtnSend: TBCButton;
    BtnSend1: TBCButton;
    BtnSendAllClick: TBCMDButton;
    DateEditSend: TDateTimePicker;
    EditEmail: TEdit;
    EditRecipient1: TEdit;
    EditSearh: TEdit;
    EditSubject1: TEdit;
    GroupBox1: TGroupBox;
    GroupViewContacts: TGroupBox;
    Image2: TImage;
    LblEditName: TLabeledEdit;
    LblEditUsername: TLabeledEdit;
    LblEditEmail: TLabeledEdit;
    LblEditPhone: TLabeledEdit;
    LblEditName4: TLabeledEdit;
    LblEditName5: TLabeledEdit;
    LblEditName6: TLabeledEdit;
    LblEditName7: TLabeledEdit;
    LblEmail: TLabel;
    LblInfo2: TLabel;
    LblInfo3: TLabel;
    LblInfo4: TLabel;
    LblInfo5: TLabel;
    LblMessage: TBCLabel;
    EditSubject: TEdit;
    LblMessage1: TBCLabel;
    LblSendDate: TBCLabel;
    LblRecipient1: TBCLabel;
    LblSubject: TBCLabel;
    EditRecipient: TEdit;
    LblRecipient: TBCLabel;
    BtnContacts: TBCMDButton;
    BtnDelete: TBCMDButton;
    BtnUpdateProfile: TBCMDButton;
    BtnScheduledEmails: TBCMDButton;
    BtnScheduleEmail: TBCMDButton;
    BtnGenerateReports: TBCMDButton;
    LblFileSelected: TBCLabel;
    LblInfo1: TLabel;
    LblSubject1: TBCLabel;
    ListView2: TListView;
    ListView3: TListView;
    MemoMessage1: TMemo;
    MemoPreview1: TMemo;
    Panel1: TPanel;
    PanelUpdateProfile: TPanel;
    PanelControls1: TPanel;
    PanelScheduledEmails: TPanel;
    PanelInboxBody1: TPanel;
    PanelInboxBody2: TPanel;
    PanelContacts: TPanel;
    PanelScheduleEmail: TPanel;
    PanelTitle: TLabel;
    BtnInbox: TBCMDButton;
    BtnSendEmail: TBCMDButton;
    BtnTrash: TBCMDButton;
    Image1: TImage;
    LblInfo: TLabel;
    BtnSortAZ: TBCMDButton;
    LblSection: TLabel;
    Label2: TLabel;
    ListView1: TListView;
    MemoMessage: TMemo;
    MemoPreview: TMemo;
    PanelInboxBody: TPanel;
    PanelControls2: TPanel;
    PanelTrash: TPanel;
    PanelSendEmail: TPanel;
    PanelControls: TPanel;
    PanelInbox: TPanel;
    PanelTop: TPanel;
    PanelBody: TPanel;
    PanelSidebar: TPanel;
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    procedure BtnAddContactClick(Sender: TObject);
    procedure BtnContactsClick(Sender: TObject);
    procedure BtnInboxClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnScheduledEmailsClick(Sender: TObject);
    procedure BtnScheduleEmailClick(Sender: TObject);
    procedure BtnSendEmailClick(Sender: TObject);
    procedure BtnTrashClick(Sender: TObject);
    procedure BtnUpdateProfileClick(Sender: TObject);
  private
    procedure ShowPanel(APanel: TPanel);
  private
    FContactCursor: PCircularNode;
    procedure ShowCurrentContact;
    procedure UpdateContactCursor;
  public

  end;

var
  DashboardUser: TDashboardUser;

implementation

{$R *.lfm}

{ TDashboardUser }

procedure TDashboardUser.ShowPanel(APanel: TPanel);
begin
  PanelInbox.Visible := False;
  PanelSendEmail.Visible := False;
  PanelTrash.Visible := False;
  PanelScheduleEmail.Visible := False;
  PanelScheduledEmails.Visible := False;
  PanelContacts.Visible := False;
  PanelUpdateProfile.Visible := False;

  if Assigned(APanel) then
  begin
    APanel.Visible := True;
  end;
end;

procedure TDashboardUser.ShowCurrentContact;
var
  C: PUser;
begin
  if FContactCursor = nil then
  begin
    LblEditName.Text := '';
    LblEditUsername.Text := '';
    LblEditEmail.Text := '';
    LblEditPhone.Text := '';
    Exit;
  end;

  C := PUser(FContactCursor^.Data);   // cast
  LblEditName.Text := C^.Name;
  LblEditUsername.Text := C^.Username;
  LblEditEmail.Text := C^.Email;
  LblEditPhone.Text := C^.Phone;
end;

procedure TDashboardUser.UpdateContactCursor;
begin
  FContactCursor := CurrentUser^.Contacts.Head;
  ShowCurrentContact;
end;

procedure TDashboardUser.BtnInboxClick(Sender: TObject);
begin
  ShowPanel(PanelInbox);
end;

procedure TDashboardUser.BtnLogoutClick(Sender: TObject);
begin
  CurrentUser := nil;
  Self.Close;
  SignIn.Show;
end;

procedure TDashboardUser.BtnNextClick(Sender: TObject);
begin
  if FContactCursor = nil then Exit;
  FContactCursor := FContactCursor^.Next;
  ShowCurrentContact;
end;

procedure TDashboardUser.BtnPrevClick(Sender: TObject);
begin
  if FContactCursor = nil then Exit;
  FContactCursor := FContactCursor^.Prev;
  ShowCurrentContact;
end;

procedure TDashboardUser.BtnContactsClick(Sender: TObject);
begin
  ShowPanel(PanelContacts);
end;

procedure TDashboardUser.BtnAddContactClick(Sender: TObject);
var
  Err: integer;
begin
  Err := AddContactToUser(CurrentUser^, Users, EditEmail.Text);
  case Err of
    0: begin
      ShowMessage('Contact added');
      UpdateContactCursor;
    end;
    -1: ShowMessage('Please enter an email');
    -2: ShowMessage('This user is not registered');
    -3: ShowMessage('The contact already exists');
  end;

end;

procedure TDashboardUser.BtnScheduledEmailsClick(Sender: TObject);
begin
  ShowPanel(PanelScheduledEmails);
end;

procedure TDashboardUser.BtnScheduleEmailClick(Sender: TObject);
begin
  ShowPanel(PanelScheduleEmail);
end;

procedure TDashboardUser.BtnSendEmailClick(Sender: TObject);
begin
  ShowPanel(PanelSendEmail);
end;

procedure TDashboardUser.BtnTrashClick(Sender: TObject);
begin
  ShowPanel(PanelTrash);
end;

procedure TDashboardUser.BtnUpdateProfileClick(Sender: TObject);
begin
  ShowPanel(PanelUpdateProfile);
end;

end.
