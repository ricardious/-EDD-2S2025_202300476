unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, MaskEdit, Grids, DateTimePicker, SynEdit, BCMDButton,
  ATShapeLineBGRA, BCLabel, BCRoundedImage, BGRACustomDrawn, BCButton,
  BGRAThemeButton, BCMDButtonFocus, DTAnalogClock, JsonUsersLoader, fpjson,
  jsonparser, AppState, FormLogin, ContactService, CircularLinkedList, User,
  Email, DoublyLinkedList, UserService, Stack, EmailService, Queue;

type

  { TDashboardUser }

  TDashboardUser = class(TForm)
    BtnAddContact: TBCMDButtonFocus;
    BtnEmptyTrash: TBCMDButton;
    BtnLogout: TBCMDButton;
    BtnNext: TBCMDButton;
    BtnPrev: TBCMDButton;
    BtnUpdProfile: TBCMDButton;
    BtnSend: TBCButton;
    BtnScheduleSend: TBCButton;
    BtnSendAll: TBCMDButton;
    DateEditSend: TDateTimePicker;
    EditEmail: TEdit;
    EditScheduleRecipient: TEdit;
    EditSearch: TEdit;
    EditScheduleSubject: TEdit;
    GroupBox1: TGroupBox;
    GroupViewContacts: TGroupBox;
    Image2: TImage;
    LblEditName: TLabeledEdit;
    LblEditUsername: TLabeledEdit;
    LblEditEmail: TLabeledEdit;
    LblEditPhone: TLabeledEdit;
    LblEditUpdName: TLabeledEdit;
    LblEditUpdUsername: TLabeledEdit;
    LblEditUpdEmail: TLabeledEdit;
    LblEditUpdPhone: TLabeledEdit;
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
    LvTrashEmails: TListView;
    LvScheduledEmails: TListView;
    MemoScheduleMessage: TMemo;
    MemoTrashPreview: TMemo;
    Panel1: TPanel;
    PanelUpdateProfile: TPanel;
    PanelControls1: TPanel;
    PanelScheduledEmails: TPanel;
    PanelTrashBody: TPanel;
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
    procedure BtnEmptyTrashClick(Sender: TObject);
    procedure BtnInboxClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnScheduledEmailsClick(Sender: TObject);
    procedure BtnScheduleEmailClick(Sender: TObject);
    procedure BtnScheduleSendClick(Sender: TObject);
    procedure BtnSendAllClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnSendEmailClick(Sender: TObject);
    procedure BtnSortAZClick(Sender: TObject);
    procedure BtnTrashClick(Sender: TObject);
    procedure BtnUpdateProfileClick(Sender: TObject);
    procedure BtnUpdProfileClick(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvInboxSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure LvTrashEmailsSelectItem(Sender: TObject; Item: TListItem;
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
  private
    procedure RefreshTrashList;
    procedure UpdateTrashSearch(const KeyWord: string);
  private
    procedure RefreshScheduledEmailsList;
  private
    procedure LoadCurrentUserData;
    function ValidateProfileData: boolean;
    procedure UpdateUserProfile;
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
    'Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', Mail^.Date) +
    #13#10#13#10 + Mail^.MessageBody;

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

function TruncateText(const S: string; MaxLen: integer): string;
begin
  if Length(S) > MaxLen then
    Result := Copy(S, 1, MaxLen) + '...'
  else
    Result := S;
end;

procedure TDashboardUser.RefreshTrashList;
var
  Item: TListItem;
  Mail: PEmail;
  i: integer;
begin
  LvTrashEmails.Items.BeginUpdate;
  try
    LvTrashEmails.Items.Clear;
    for i := 0 to Stack.Count(CurrentUser^.Trash) - 1 do
    begin
      Mail := PEmail(Stack.GetItem(CurrentUser^.Trash, i));

      if Mail <> nil then
      begin
        Item := LvTrashEmails.Items.Add;
        Item.Caption := Mail^.Subject;
        Item.SubItems.Add(Mail^.Sender);
        Item.SubItems.Add(TruncateText(Mail^.MessageBody, 15));
        Item.Data := Mail;
      end;
    end;
  finally
    LvTrashEmails.Items.EndUpdate;
  end;
end;

procedure TDashboardUser.UpdateTrashSearch(const KeyWord: string);

  procedure FillFiltered(const Filter: string);
  var
    i: integer;
    Item: TListItem;
    M: PEmail;
  begin
    LvTrashEmails.Items.BeginUpdate;
    try
      LvTrashEmails.Items.Clear;
      for i := 0 to Stack.Count(CurrentUser^.Trash) - 1 do
      begin
        M := PEmail(Stack.GetItem(CurrentUser^.Trash, i));
        if (Filter = '') or (Pos(LowerCase(Filter), LowerCase(M^.Subject)) >
          0) or (Pos(LowerCase(Filter), LowerCase(M^.Sender)) > 0) or
          (Pos(LowerCase(Filter), LowerCase(M^.MessageBody)) > 0) then
        begin
          Item := LvTrashEmails.Items.Add;
          Item.Caption := M^.Subject;
          Item.SubItems.Add(M^.Sender);
          Item.SubItems.Add(M^.MessageBody);
          Item.Data := M;
        end;
      end;
    finally
      LvTrashEmails.Items.EndUpdate;
    end;
  end;

begin
  FillFiltered(Trim(KeyWord));
end;

procedure TDashboardUser.RefreshScheduledEmailsList;
var
  TempQueue: TQueue;
  Mail: PEmail;
  ListItem: TListItem;
begin
  LvScheduledEmails.Items.BeginUpdate;
  LvScheduledEmails.Items.Clear;

  TempQueue := Default(TQueue);
  Init(TempQueue);

  try
    while not IsEmpty(CurrentUser^.ScheduledMail) do
    begin
      Mail := PEmail(Dequeue(CurrentUser^.ScheduledMail));

      if Mail <> nil then
      begin
        ListItem := LvScheduledEmails.Items.Add;
        ListItem.Caption := Mail^.Subject;
        ListItem.SubItems.Add(Mail^.Recipient);

        try
          ListItem.SubItems.Add(FormatDateTime('yyyy-mm-dd', Mail^.Date));
          ListItem.SubItems.Add(FormatDateTime('hh:nn am/pm', Mail^.Date));
        except
          ListItem.SubItems.Add('N/A');
          ListItem.SubItems.Add('N/A');
        end;
        Enqueue(TempQueue, Mail);
      end;
    end;
  finally
    CurrentUser^.ScheduledMail := TempQueue;
    LvScheduledEmails.Items.EndUpdate;
  end;
end;


procedure TDashboardUser.LoadCurrentUserData;
begin
  if CurrentUser = nil then Exit;

  LblEditUpdName.Text := CurrentUser^.Name;
  LblEditUpdUsername.Text := CurrentUser^.Username;
  LblEditUpdEmail.Text := CurrentUser^.Email;
  LblEditUpdPhone.Text := CurrentUser^.Phone;
end;

function TDashboardUser.ValidateProfileData: boolean;
var
  NewUsername, NewPhone: string;
  ExistingUser: PUser;
  i: integer;
begin
  Result := False;

  NewUsername := Trim(LblEditUpdUsername.Text);
  NewPhone := Trim(LblEditUpdPhone.Text);

  if NewUsername = '' then
  begin
    ShowMessage('Username cannot be empty.');
    LblEditUpdUsername.SetFocus;
    Exit;
  end;

  if Length(NewUsername) < 3 then
  begin
    ShowMessage('Username must be at least 3 characters long.');
    LblEditUpdUsername.SetFocus;
    Exit;
  end;

  if (NewUsername <> CurrentUser^.Username) then
  begin
    ExistingUser := FindUserByUsername(Users, NewUsername);
    if ExistingUser <> nil then
    begin
      ShowMessage('Username "' + NewUsername + '" is already taken.');
      LblEditUpdUsername.SetFocus;
      Exit;
    end;
  end;

  if (NewPhone <> '') and (Length(NewPhone) < 8) then
  begin
    ShowMessage('Phone must have at least 8 digits.');
    LblEditUpdPhone.SetFocus;
    Exit;
  end;

  if NewPhone <> '' then
  begin
    for i := 1 to Length(NewPhone) do
    begin
      if not (NewPhone[i] in ['0'..'9', ' ', '-', '(', ')']) then
      begin
        ShowMessage('Phone contains invalid characters.');
        LblEditUpdPhone.SetFocus;
        Exit;
      end;
    end;
  end;

  Result := True;
end;

procedure TDashboardUser.UpdateUserProfile;
var
  NewUsername, NewPhone: string;
  ChangesCount: integer;
begin
  if not ValidateProfileData then Exit;

  NewUsername := Trim(LblEditUpdUsername.Text);
  NewPhone := Trim(LblEditUpdPhone.Text);

  ChangesCount := 0;

  if NewUsername <> CurrentUser^.Username then
  begin
    CurrentUser^.Username := NewUsername;
    Inc(ChangesCount);
  end;

  if NewPhone <> CurrentUser^.Phone then
  begin
    CurrentUser^.Phone := NewPhone;
    Inc(ChangesCount);
  end;

  if ChangesCount > 0 then
  begin
    ShowMessage(Format('Profile updated successfully. %d field(s) modified.',
      [ChangesCount]));

    LoadCurrentUserData;
  end
  else
  begin
    ShowMessage('No changes detected in the profile.');
  end;
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

procedure TDashboardUser.BtnEmptyTrashClick(Sender: TObject);
var
  Mail: PEmail;
begin
  if MessageDlg('Empty Trash permanently?', mtConfirmation, [mbYes, mbNo], 0) =
    mrYes then
  begin
    while not Stack.IsEmpty(CurrentUser^.Trash) do
    begin
      Mail := PEmail(Stack.Pop(CurrentUser^.Trash));
      Dispose(Mail);
    end;
    RefreshTrashList;
    MemoTrashPreview.Clear;
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
  RefreshScheduledEmailsList;
  ShowPanel(PanelScheduledEmails);
end;

procedure TDashboardUser.BtnScheduleEmailClick(Sender: TObject);
begin
  DateEditSend.DateTime := Now;
  ShowPanel(PanelScheduleEmail);
end;

procedure TDashboardUser.BtnScheduleSendClick(Sender: TObject);
var
  Recipient, Subject, Body: string;
  ScheduledDate: TDateTime;
  NewMail: PEmail;
  ValidationResult: TEmailSendResult;
begin
  Recipient := Trim(EditScheduleRecipient.Text);
  Subject := Trim(EditScheduleSubject.Text);
  Body := MemoScheduleMessage.Text;
  ScheduledDate := DateEditSend.DateTime;

  ValidationResult := ValidateEmailRecipient(Recipient, CurrentUser^.Contacts);
  case ValidationResult of
    esrEmptyRecipient:
    begin
      ShowMessage('Please enter a recipient.');
      EditScheduleRecipient.SetFocus;
      Exit;
    end;
    esrNotInContacts:
    begin
      ShowMessage('Recipient is not in your contact list.');
      EditScheduleRecipient.SetFocus;
      Exit;
    end;
  end;

  if ScheduledDate <= Now then
  begin
    ShowMessage('Please select a future date and time.');
    Exit;
  end;

  NewMail := CreateNewEmail(CurrentUser^.Email, Recipient, Subject, Body, ScheduledDate);
  Enqueue(CurrentUser^.ScheduledMail, NewMail);
  ShowMessage('Email scheduled successfully for: ' + DateTimeToStr(ScheduledDate));

  EditScheduleRecipient.Clear;
  EditScheduleSubject.Clear;
  MemoScheduleMessage.Clear;
  DateEditSend.DateTime := Now;
end;

procedure TDashboardUser.BtnSendAllClick(Sender: TObject);
var
  TempQueue: TQueue;
  Mail: PEmail;
  EmailsSentCount: integer;
  // CurrentTime: TDateTime;
begin
  if IsEmpty(CurrentUser^.ScheduledMail) then
  begin
    ShowMessage('No scheduled emails to send.');
    Exit;
  end;

  TempQueue := Default(TQueue);
  Init(TempQueue);
  EmailsSentCount := 0;
  // CurrentTime := Now;

  while not IsEmpty(CurrentUser^.ScheduledMail) do
  begin
    Mail := PEmail(Dequeue(CurrentUser^.ScheduledMail));

    if Mail <> nil then
    begin
      try
        // if Mail^.Date <= CurrentTime then
        // begin
        if DeliverEmailToUser(Mail^.Recipient, Mail) = edrSuccess then
        begin
          Inc(EmailsSentCount);
        end
        else
        begin
          ShowMessage('Error sending email to: ' + Mail^.Recipient);
          Dispose(Mail);
        end;
        // end
        // else
        // begin
        //  Enqueue(TempQueue, Mail);
        // end;
      except
        on E: Exception do
        begin
          ShowMessage('Error processing email: ' + E.Message);
          Dispose(Mail);
        end;
      end;
    end;
  end;
  CurrentUser^.ScheduledMail := TempQueue;
  if EmailsSentCount > 0 then
    ShowMessage(Format('%d email(s) sent successfully.', [EmailsSentCount]))
  else
    ShowMessage('No emails were ready to be sent at this time.');
  RefreshScheduledEmailsList;
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
  RefreshTrashList;
  ShowPanel(PanelTrash);
end;

procedure TDashboardUser.BtnUpdateProfileClick(Sender: TObject);
begin
  LoadCurrentUserData;
  ShowPanel(PanelUpdateProfile);
end;

procedure TDashboardUser.BtnUpdProfileClick(Sender: TObject);
begin
  UpdateUserProfile;
end;

procedure TDashboardUser.EditSearchChange(Sender: TObject);
begin
  UpdateTrashSearch(Trim(EditSearch.Text));
end;

procedure TDashboardUser.FormShow(Sender: TObject);
begin
  BoundsRect := Screen.WorkAreaRect;
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

procedure TDashboardUser.LvTrashEmailsSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
var
  M: PEmail;
begin
  if Selected and (Item <> nil) then
  begin
    M := PEmail(Item.Data);
    MemoTrashPreview.Lines.Text :=
      'Subject: ' + M^.Subject + #13#10 + 'From: ' + M^.Sender +
      #13#10 + 'Date: ' + FormatDateTime('yyyy-mm-dd hh:nn', M^.Date) +
      #13#10#13#10 + M^.MessageBody;
  end
  else
    MemoTrashPreview.Clear;
end;


end.
