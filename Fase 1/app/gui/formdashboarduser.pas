unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, MaskEdit, Grids, DateTimePicker, BCMDButton, ATShapeLineBGRA,
  BCLabel, BCRoundedImage, BGRACustomDrawn, BCButton, BGRAThemeButton,
  BCMDButtonFocus, JsonUsersLoader, fpjson, jsonparser, AppState,
  FormLogin, ContactService, CircularLinkedList, User, Email,
  DoublyLinkedList, UserService, Stack, EmailService;

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
    LvInbox: TListView;
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
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnInboxClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnScheduledEmailsClick(Sender: TObject);
    procedure BtnScheduleEmailClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnSendEmailClick(Sender: TObject);
    procedure BtnSortAZClick(Sender: TObject);
    procedure BtnTrashClick(Sender: TObject);
    procedure BtnUpdateProfileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvInboxSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
  private
    procedure ShowPanel(APanel: TPanel);
    procedure ClearUserUI;
  private
    FContactCursor: PCircularNode;
    procedure ShowCurrentContact;
    procedure UpdateContactCursor;
  private
    FInboxCursor: PDoublyNode;
    procedure RefreshInboxList;
    procedure ShowSelectedMail;
    procedure UpdateUnreadCount;
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

procedure TDashboardUser.RefreshInboxList;
var
  Node: PDoublyNode;
  Mail: PEmail;
  Item: TListItem;
begin
  LvInbox.Items.BeginUpdate;
  try
    LvInbox.Items.Clear;
    Node := CurrentUser^.Inbox.Head;
    while Node <> nil do
    begin
      Mail := PEmail(Node^.Data);
      if Mail = nil then Continue;

      Item := LvInbox.Items.Add;
      case Mail^.State of
        esUnread: Item.Caption := 'NL';
        esRead: Item.Caption := 'L';
      end;
      Item.SubItems.Add(Mail^.Subject);
      Item.SubItems.Add(Mail^.Sender);
      Item.Data := Node;
      Node := Node^.Next;
    end;
  finally
    LvInbox.Items.EndUpdate;
  end;
  LvInbox.ItemIndex := -1;
  UpdateUnreadCount;
  // MemoPreview.SetFocus;  -> ERROR
end;

procedure TDashboardUser.UpdateUnreadCount;
var
  UnreadCount: integer;
begin
  UnreadCount := CountUnreadEmails(CurrentUser^.Inbox);
  LblFileSelected.Caption := 'Unread: ' + IntToStr(UnreadCount);
end;


procedure TDashboardUser.ShowSelectedMail;
var
  Mail: PEmail;
begin
  if FInboxCursor = nil then Exit;

  Mail := PEmail(FInboxCursor^.Data);
  if Mail = nil then Exit;

  MarkEmailAsRead(Mail);
  LvInbox.Selected.Caption := 'L';

  MemoPreview.Lines.Text :=
    'Status: ' + LvInbox.Selected.Caption + #13#10 + 'Subject: ' +
    Mail^.Subject + #13#10 + 'From: ' + Mail^.Sender + #13#10 +
    'Date: ' + Mail^.Date + #13#10#13#10 + Mail^.MessageBody;

  UpdateUnreadCount;
end;


procedure TDashboardUser.ClearUserUI;
begin
  EditEmail.Text := '';
end;

procedure TDashboardUser.BtnSortAZClick(Sender: TObject);
begin
  if CurrentUser = nil then Exit;
  DoublyLinkedList.Sort(@CompareEmailsBySubject, CurrentUser^.Inbox);
  RefreshInboxList;
  ShowMessage('Emails sorted by subject (A-Z)');
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
  ClearUserUI;
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

procedure TDashboardUser.BtnDeleteClick(Sender: TObject);
begin
  if FInboxCursor = nil then Exit;

  if MessageDlg('Delete this email?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    DeleteEmailFromInbox(CurrentUser^.Inbox, CurrentUser^.Trash, FInboxCursor);
    FInboxCursor := nil;
    RefreshInboxList;
    MemoPreview.Clear;
    BtnDelete.Enabled := False;
  end;
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
    -4: ShowMessage('You cannot add yourself as a contact');
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

procedure TDashboardUser.BtnSendClick(Sender: TObject);
var
  SendResult: TEmailSendResult;
begin
  SendResult := SendEmailToContact(CurrentUser, Trim(EditRecipient.Text),
    Trim(EditSubject.Text), MemoMessage.Text);

  case SendResult of
    esrSuccess: begin
      ShowMessage('Email sent');
      EditRecipient.Text := '';
      EditSubject.Text := '';
      MemoMessage.Lines.Clear;
    end;
    esrEmptyRecipient: ShowMessage('Please enter the recipient');
    esrNotInContacts: ShowMessage('Error: the recipient is not in your contact list');
  end;
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

procedure TDashboardUser.FormShow(Sender: TObject);
begin
  RefreshInboxList;
  UpdateContactCursor;
end;

procedure TDashboardUser.LvInboxSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  if Selected and (Item <> nil) then
  begin
    FInboxCursor := PDoublyNode(Item.Data);
    ShowSelectedMail;
    BtnDelete.Enabled := True;
  end
  else
  begin
    FInboxCursor := nil;
    MemoPreview.Clear;
    BtnDelete.Enabled := False;
  end;
end;

end.
