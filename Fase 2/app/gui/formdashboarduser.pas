unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, MaskEdit, Grids, DateTimePicker, SynEdit, BCMDButton,
  ATShapeLineBGRA, BCLabel, BCRoundedImage, BGRACustomDrawn, BCButton,
  BGRAThemeButton, BCMDButtonFocus, DTAnalogClock, JsonUsersLoader, fpjson,
  jsonparser, AppState, FormLogin, ContactService, CircularLinkedList, User,
  Email, DoublyLinkedList, UserService, Stack, EmailService, Queue,
  Process, LCLIntf, BTree, AVLTree, Community, CommunityService;

type
  TDraftOrder = (doPre, doIn, doPost);

  { TDashboardUser }

  TDashboardUser = class(TForm)
    BtnAddContact: TBCMDButtonFocus;
    BtnDeleteContact: TBCMDButtonFocus;
    BtnDeleteFavorite: TBCMDButton;
    BtnFavorite: TBCMDButton;
    BtnDrafts: TBCMDButton;
    BtnCommunity: TBCMDButton;
    BtnEmptyTrash: TBCMDButton;
    BtnLogout: TBCMDButton;
    BtnNext: TBCMDButton;
    BtnPrev: TBCMDButton;
    BtnFavorites: TBCMDButton;
    BtnPost: TBCButton;
    BtnSendDraft: TBCMDButton;
    BtnEditDraft: TBCMDButton;
    BtnDeleteDraft: TBCMDButton;
    BtnPreOrder: TBCMDButton;
    BtnInOrder: TBCMDButton;
    BtnPostOrder: TBCMDButton;
    BtnUpdProfile: TBCMDButton;
    BtnSend: TBCButton;
    BtnScheduleSend: TBCButton;
    BtnSendAll: TBCMDButton;
    DateEditSend: TDateTimePicker;
    EditEmail: TEdit;
    EditDeleteEmail: TEdit;
    EditCommunity: TEdit;
    EditScheduleRecipient: TEdit;
    EditSearch: TEdit;
    EditScheduleSubject: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupViewContacts: TGroupBox;
    Image2: TImage;
    Label1: TLabel;
    Panel4: TPanel;
    LabeledEditSubjectDraft: TLabeledEdit;
    LabeledEditToDraft: TLabeledEdit;
    LabeledEditMessageDraft: TLabeledEdit;
    LblEditName: TLabeledEdit;
    LblEditUsername: TLabeledEdit;
    LblEditEmail: TLabeledEdit;
    LblEditPhone: TLabeledEdit;
    LblEditUpdName: TLabeledEdit;
    LblEditUpdUsername: TLabeledEdit;
    LblEditUpdEmail: TLabeledEdit;
    LblEditUpdPhone: TLabeledEdit;
    LblEmail: TLabel;
    LblEmail1: TLabel;
    LblFavoritesCount: TBCLabel;
    LblInfo2: TLabel;
    LblInfo3: TLabel;
    LblInfo4: TLabel;
    LblInfo5: TLabel;
    LblInfo6: TLabel;
    LblInfo7: TLabel;
    LblInfo8: TLabel;
    LblMessage: TBCLabel;
    EditSubject: TEdit;
    LblMessage1: TBCLabel;
    LblMessage2: TBCLabel;
    LblCommunity: TBCLabel;
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
    LvFavorites: TListView;
    LvDrafts: TListView;
    LvTrashEmails: TListView;
    LvScheduledEmails: TListView;
    MemoMessageCommunity: TMemo;
    MemoPreviewFavorite: TMemo;
    MemoScheduleMessage: TMemo;
    MemoTrashPreview: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    PanelControls3: TPanel;
    PanelControls4: TPanel;
    PanelFavorites: TPanel;
    PanelFavoritesBody: TPanel;
    PanelCommunityPost: TPanel;
    PanelDrafts: TPanel;
    PanelDraftBody: TPanel;
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
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    procedure BtnAddContactClick(Sender: TObject);
    procedure BtnCommunityClick(Sender: TObject);
    procedure BtnContactsClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnDeleteContactClick(Sender: TObject);
    procedure BtnDeleteDraftClick(Sender: TObject);
    procedure BtnDeleteFavoriteClick(Sender: TObject);
    procedure BtnDraftsClick(Sender: TObject);
    procedure BtnEditDraftClick(Sender: TObject);
    procedure BtnEmptyTrashClick(Sender: TObject);
    procedure BtnFavoriteClick(Sender: TObject);
    procedure BtnFavoritesClick(Sender: TObject);
    procedure BtnGenerateReportsClick(Sender: TObject);
    procedure BtnInboxClick(Sender: TObject);
    procedure BtnInOrderClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
    procedure BtnPostClick(Sender: TObject);
    procedure BtnPostOrderClick(Sender: TObject);
    procedure BtnPreOrderClick(Sender: TObject);
    procedure BtnPrevClick(Sender: TObject);
    procedure BtnScheduledEmailsClick(Sender: TObject);
    procedure BtnScheduleEmailClick(Sender: TObject);
    procedure BtnScheduleSendClick(Sender: TObject);
    procedure BtnSendAllClick(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure BtnSendDraftClick(Sender: TObject);
    procedure BtnSendEmailClick(Sender: TObject);
    procedure BtnSortAZClick(Sender: TObject);
    procedure BtnTrashClick(Sender: TObject);
    procedure BtnUpdateProfileClick(Sender: TObject);
    procedure BtnUpdProfileClick(Sender: TObject);
    procedure EditSearchChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LvDraftsSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure LvInboxSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure LvTrashEmailsSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    procedure LvFavoritesSelectItem(Sender: TObject; Item: TListItem;
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
  private
    procedure RefreshFavoritesList;
  private
    FDefaultMessage: string;
    FEmailJustSent: boolean;
    procedure MaybeAutoSaveDraft;
  private
    FDraftOrder: TDraftOrder;
    procedure RefreshDraftsList;
    procedure FillDrafts_PreOrder;
    procedure FillDrafts_InOrder;
    procedure FillDrafts_PostOrder;
    procedure LoadSelectedDraftToEditors;
  public

  end;

var
  DashboardUser: TDashboardUser;
  GDraftsListView: TListView = nil;
  GDraftsCount: integer = 0;

implementation

{$R *.lfm}

{ TDashboardUser }

procedure DraftsCollector(Data: Pointer);
var
  M: PEmail;
  It: TListItem;
begin
  if GDraftsListView = nil then Exit;
  M := PEmail(Data);
  if M = nil then Exit;

  Inc(GDraftsCount);
  It := GDraftsListView.Items.Add;
  It.Caption := IntToStr(M^.Id);           // ID
  It.SubItems.Add(M^.Subject);             // Subject
  It.SubItems.Add(M^.Recipient);           // Recipient
  It.SubItems.Add('Draft');                // Status
  It.SubItems.Add('');                     // Actions
  It.Data := M;
end;

function RunDotToPng(const DotFile, PngFile: string): boolean;
const
  GraphvizCmd = 'dot';
var
  Proc: TProcess;
begin
  Result := False;
  Proc := TProcess.Create(nil);
  try
    Proc.Executable := GraphvizCmd;
    Proc.Parameters.Add('-Tpng');
    Proc.Parameters.Add('-o' + PngFile);
    Proc.Parameters.Add(DotFile);
    Proc.Options := [poWaitOnExit];
    Proc.Execute;
    Result := (Proc.ExitStatus = 0) and FileExists(PngFile);
  finally
    Proc.Free;
  end;
end;

procedure TDashboardUser.MaybeAutoSaveDraft;
var
  Recipient, Subject_, Body: string;
begin
  if FEmailJustSent then
  begin
    FEmailJustSent := False;
    Exit;
  end;

  if CurrentUser = nil then Exit;

  Recipient := Trim(EditRecipient.Text);
  Subject_ := Trim(EditSubject.Text);
  Body := Trim(MemoMessage.Text);

  if (Recipient <> '') and (Subject_ <> '') and (Body <> '') and
    (Body <> Trim(FDefaultMessage)) then
  begin
    SaveDraft(CurrentUser, Subject_, Body, Recipient);
    ShowMessage('Message saved as draft.');

    EditRecipient.Clear;
    EditSubject.Clear;
    MemoMessage.Text := FDefaultMessage;
  end;
end;

procedure TDashboardUser.ShowPanel(APanel: TPanel);
begin
  if PanelSendEmail.Visible and (APanel <> PanelSendEmail) then
    MaybeAutoSaveDraft;

  PanelInbox.Visible := False;
  PanelSendEmail.Visible := False;
  PanelTrash.Visible := False;
  PanelScheduleEmail.Visible := False;
  PanelScheduledEmails.Visible := False;
  PanelContacts.Visible := False;
  PanelCommunityPost.Visible := False;
  PanelDrafts.Visible := False;
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

procedure TDashboardUser.BtnInOrderClick(Sender: TObject);
begin
  FDraftOrder := doIn;
  RefreshDraftsList;
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

procedure TDashboardUser.BtnPostClick(Sender: TObject);
var
  CommName, Body: string;
  R: TPostMessageResult;
begin
  if CurrentUser = nil then Exit;

  CommName := Trim(EditCommunity.Text);
  Body := MemoMessageCommunity.Text;

  R := PostMessageToCommunity(CommName, CurrentUser^.Email, Body);

  case R of
    pmOK:
    begin
      ShowMessage('Message posted to community.');
      MemoMessageCommunity.Clear;
    end;
    pmEmpty:
      ShowMessage('Enter a community name and a non-empty message.');
    pmCommunityNotFound:
      ShowMessage('Community not found.');
    pmNotMember:
      ShowMessage('You must be a member of this community to post.');
  end;
end;

procedure TDashboardUser.BtnPostOrderClick(Sender: TObject);
begin
  FDraftOrder := doPost;
  RefreshDraftsList;
end;

procedure TDashboardUser.BtnPreOrderClick(Sender: TObject);
begin
  FDraftOrder := doPre;
  RefreshDraftsList;
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

procedure TDashboardUser.BtnDeleteContactClick(Sender: TObject);
var
  Err: integer;
  Email: string;
begin
  if CurrentUser = nil then Exit;

  Email := Trim(EditDeleteEmail.Text);
  Err := RemoveContactFromUser(CurrentUser^, Email);

  case Err of
    0:
    begin
      ShowMessage('Contact removed');
      UpdateContactCursor;
      EditDeleteEmail.Clear;
    end;
    -1: ShowMessage('Please enter an email');
    -2: ShowMessage('This email is not in your contact list');
    -3: ShowMessage('You cannot remove yourself');
  end;
end;

procedure TDashboardUser.BtnDeleteDraftClick(Sender: TObject);
var
  M: PEmail;
  Id: integer;
begin
  if (LvDrafts.Selected = nil) or (CurrentUser = nil) then Exit;

  M := PEmail(LvDrafts.Selected.Data);
  if M = nil then Exit;
  Id := M^.Id;

  if MessageDlg('Delete this draft?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if RemoveDraft(CurrentUser, Id) then
    begin
      ShowMessage('Draft deleted.');
      RefreshDraftsList;
      LabeledEditSubjectDraft.Clear;
      LabeledEditToDraft.Clear;
      LabeledEditMessageDraft.Clear;
    end
    else
      ShowMessage('Could not delete the draft.');
  end;
end;

procedure TDashboardUser.LvFavoritesSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
var
  M: PEmail;
begin
  if Selected and (Item <> nil) then
  begin
    M := PEmail(Item.Data);
    BtnDeleteFavorite.Enabled := (M <> nil);

    if M <> nil then
      MemoPreviewFavorite.Lines.Text :=
        'ID: ' + IntToStr(M^.Id) + LineEnding + 'From: ' + M^.Sender +
        LineEnding + 'To: ' + M^.Recipient + LineEnding + 'Subject: ' +
        M^.Subject + LineEnding + 'Date: ' + FormatDateTime(
        'yyyy-mm-dd hh:nn', M^.Date) + LineEnding + LineEnding + M^.MessageBody
    else
      MemoPreviewFavorite.Clear;
  end
  else
  begin
    BtnDeleteFavorite.Enabled := False;
    MemoPreviewFavorite.Clear;
  end;
end;

procedure TDashboardUser.BtnDeleteFavoriteClick(Sender: TObject);
var
  M: PEmail;
  FavId: integer;
begin
  if (LvFavorites.Selected = nil) or (CurrentUser = nil) then Exit;

  M := PEmail(LvFavorites.Selected.Data);
  if M = nil then Exit;

  FavId := M^.Id;

  if MessageDlg('Remove from favorites?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    if RemoveFavorite(CurrentUser, FavId) then
    begin
      ShowMessage('Favorite removed.');
      RefreshFavoritesList;
      BtnDeleteFavorite.Enabled := False;
    end
    else
      ShowMessage('Could not remove favorite (not found or error).');
  end;
end;

procedure TDashboardUser.BtnDraftsClick(Sender: TObject);
begin
  FDraftOrder := doIn;
  RefreshDraftsList;
  ShowPanel(PanelDrafts);
end;

procedure TDashboardUser.BtnEditDraftClick(Sender: TObject);
var
  M: PEmail;
  Id: integer;
  Ok: boolean;
  NewTo, NewSubj, NewBody: string;
begin
  if (LvDrafts.Selected = nil) or (CurrentUser = nil) then Exit;

  M := PEmail(LvDrafts.Selected.Data);
  if M = nil then Exit;
  Id := M^.Id;

  NewTo := Trim(LabeledEditToDraft.Text);
  NewSubj := Trim(LabeledEditSubjectDraft.Text);
  NewBody := Trim(LabeledEditMessageDraft.Text);

  if (NewTo = '') or (NewSubj = '') or (NewBody = '') then
  begin
    ShowMessage('All fields are required to update the draft.');
    Exit;
  end;

  Ok := UpdateDraft(CurrentUser, Id, NewTo, NewSubj, NewBody);
  if Ok then
  begin
    ShowMessage('Draft updated.');
    RefreshDraftsList;
  end
  else
    ShowMessage('Could not update the draft.');
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

procedure TDashboardUser.RefreshFavoritesList;

  procedure InOrder(Node: PBTreeNode; var Count: integer);
  var
    i: integer;
    M: PEmail;
    Item: TListItem;
  begin
    if Node = nil then Exit;

    for i := 1 to Node^.n do
    begin
      InOrder(Node^.Children[i], Count);

      M := PEmail(Node^.Keys[i]);
      if M <> nil then
      begin
        Inc(Count);
        Item := LvFavorites.Items.Add;
        Item.Caption := IntToStr(M^.Id);
        Item.SubItems.Add(M^.Sender);
        Item.SubItems.Add(M^.Recipient);
        Item.SubItems.Add(M^.Subject);
        Item.SubItems.Add(TruncateText(M^.MessageBody, 30));
        Item.Data := M;
      end;
    end;

    InOrder(Node^.Children[Node^.n + 1], Count);
  end;

var
  Root: PBTreeNode;
  Count: integer;
begin
  if (CurrentUser = nil) or (CurrentUser^.Favorites = nil) then Exit;

  LvFavorites.Items.BeginUpdate;
  try
    LvFavorites.Items.Clear;
    Root := CurrentUser^.Favorites.GetRoot;
    Count := 0;
    InOrder(Root, Count);
  finally
    LvFavorites.Items.EndUpdate;
  end;
  LblFavoritesCount.Caption := 'Favorites: ' + IntToStr(Count);
  BtnDeleteFavorite.Enabled := False;
end;

procedure TDashboardUser.BtnFavoriteClick(Sender: TObject);
var
  Mail: PEmail;
begin
  if (FInboxCursor = nil) or (CurrentUser = nil) then Exit;

  Mail := PEmail(FInboxCursor^.Data);
  if Mail = nil then Exit;

  AddFavorite(CurrentUser, Mail);
  ShowMessage('Email added to Favorites.');
  RefreshFavoritesList;
end;

procedure TDashboardUser.BtnFavoritesClick(Sender: TObject);
begin
  RefreshFavoritesList;
  ShowPanel(PanelFavorites);
end;

procedure TDashboardUser.BtnGenerateReportsClick(Sender: TObject);
const
  DOT = '.dot';
  PNG = '.png';
var
  BaseDir, DotPath, PngPath: string;
begin
  if CurrentUser = nil then Exit;

  BaseDir := ExpandFileName(ExtractFilePath(Application.ExeName) +
    '..' + DirectorySeparator + 'data' + DirectorySeparator + 'output' +
    DirectorySeparator + Format('%s-Reportes', [CurrentUser^.Username]));
  ForceDirectories(BaseDir);

  Screen.Cursor := crHourGlass;
  try
    // INBOX
    DotPath := BaseDir + DirectorySeparator + 'ReporteInbox' + DOT;
    PngPath := BaseDir + DirectorySeparator + 'ReporteInbox' + PNG;
    DoublyLinkedList.GenerateDotFile(CurrentUser^.Inbox, DotPath, @EmailToStr);
    if not RunDotToPng(DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Inbox PNG.');

    // TRASH
    DotPath := BaseDir + DirectorySeparator + 'ReportePapelera' + DOT;
    PngPath := BaseDir + DirectorySeparator + 'ReportePapelera' + PNG;
    Stack.GenerateDotFile(CurrentUser^.Trash, DotPath, @EmailToStr);
    if not RunDotToPng(DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Trash PNG.');

    // SCHEDULED
    DotPath := BaseDir + DirectorySeparator + 'ReporteProgramados' + DOT;
    PngPath := BaseDir + DirectorySeparator + 'ReporteProgramados' + PNG;
    Queue.GenerateDotFile(CurrentUser^.ScheduledMail, DotPath, @EmailToStr);
    if not RunDotToPng(DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Scheduled PNG.');

    // CONTACTS
    DotPath := BaseDir + DirectorySeparator + 'ReporteContactos' + DOT;
    PngPath := BaseDir + DirectorySeparator + 'ReporteContactos' + PNG;
    CircularLinkedList.GenerateDotFile(CurrentUser^.Contacts, DotPath, @UserToStr);
    if not RunDotToPng(DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Contacts PNG.');

    // DRAFTS
    if not GenerateDraftsAVLReport(CurrentUser, BaseDir, DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Drafts PNG.');

    // FAVORITES
    if not GenerateFavoritesBTreeReport(CurrentUser, BaseDir, DotPath, PngPath) then
      raise Exception.Create('Graphviz failed generating Favorites PNG.');

    ShowMessage('Reports generated at: ' + BaseDir);
    OpenDocument(BaseDir);
  except
    on E: Exception do
      MessageDlg('Graphviz Error', E.Message, mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
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

procedure TDashboardUser.BtnCommunityClick(Sender: TObject);
begin
  ShowPanel(PanelCommunityPost);
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

  NewMail := CreateNewEmail(CurrentUser^.Email, Recipient, Subject,
    Body, ScheduledDate);
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
      FEmailJustSent := True;
      ShowMessage('Email sent');
      EditRecipient.Text := '';
      EditSubject.Text := '';
      MemoMessage.Lines.Clear;
      MemoMessage.Text := FDefaultMessage;
    end;
    esrEmptyRecipient: ShowMessage('Please enter the recipient');
    esrNotInContacts: ShowMessage('Error: the recipient is not in your contact list');
  end;
end;

procedure TDashboardUser.BtnSendDraftClick(Sender: TObject);
var
  M: PEmail;
  Id: integer;
  R: TEmailSendResult;
begin
  if (LvDrafts.Selected = nil) or (CurrentUser = nil) then Exit;

  M := PEmail(LvDrafts.Selected.Data);
  if M = nil then Exit;
  Id := M^.Id;

  R := SendDraftMail(CurrentUser, Id);
  case R of
    esrSuccess:
    begin
      ShowMessage('Draft sent.');
      RefreshDraftsList;
      LabeledEditSubjectDraft.Clear;
      LabeledEditToDraft.Clear;
      LabeledEditMessageDraft.Clear;
    end;
    esrEmptyRecipient: ShowMessage('Recipient is empty.');
    esrNotInContacts: ShowMessage('Recipient is not in your contact list.');
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
  FDefaultMessage := MemoMessage.Text;
  FEmailJustSent := False;
  LvFavorites.OnSelectItem := @LvFavoritesSelectItem;
end;

procedure TDashboardUser.RefreshDraftsList;
begin
  LvDrafts.Items.BeginUpdate;
  try
    LvDrafts.Items.Clear;
    case FDraftOrder of
      doPre: FillDrafts_PreOrder;
      doIn: FillDrafts_InOrder;
      doPost: FillDrafts_PostOrder;
    end;
  finally
    LvDrafts.Items.EndUpdate;
  end;

  BtnSendDraft.Enabled := False;
  BtnEditDraft.Enabled := False;
  BtnDeleteDraft.Enabled := False;
end;

procedure TDashboardUser.FillDrafts_InOrder;
begin
  if (CurrentUser = nil) or (CurrentUser^.Drafts = nil) then Exit;

  GDraftsListView := LvDrafts;
  GDraftsCount := 0;
  CurrentUser^.Drafts.TraverseInOrder(@DraftsCollector);
end;

procedure TDashboardUser.FillDrafts_PreOrder;
begin
  if (CurrentUser = nil) or (CurrentUser^.Drafts = nil) then Exit;

  GDraftsListView := LvDrafts;
  GDraftsCount := 0;
  CurrentUser^.Drafts.TraversePreOrder(@DraftsCollector);
end;

procedure TDashboardUser.FillDrafts_PostOrder;
begin
  if (CurrentUser = nil) or (CurrentUser^.Drafts = nil) then Exit;

  GDraftsListView := LvDrafts;
  GDraftsCount := 0;
  CurrentUser^.Drafts.TraversePostOrder(@DraftsCollector);
end;

procedure TDashboardUser.LoadSelectedDraftToEditors;
var
  M: PEmail;
begin
  if (LvDrafts.Selected = nil) then Exit;
  M := PEmail(LvDrafts.Selected.Data);
  if M = nil then Exit;

  LabeledEditSubjectDraft.Text := M^.Subject;
  LabeledEditToDraft.Text := M^.Recipient;
  LabeledEditMessageDraft.Text := M^.MessageBody;

  BtnSendDraft.Enabled := True;
  BtnEditDraft.Enabled := True;
  BtnDeleteDraft.Enabled := True;
end;

procedure TDashboardUser.LvDraftsSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  if Selected then
    LoadSelectedDraftToEditors
  else
  begin
    BtnSendDraft.Enabled := False;
    BtnEditDraft.Enabled := False;
    BtnDeleteDraft.Enabled := False;
  end;
end;

procedure TDashboardUser.LvInboxSelectItem(Sender: TObject; Item: TListItem;
  Selected: boolean);
begin
  if Selected and (Item <> nil) then
  begin
    FInboxCursor := PDoublyNode(Item.Data);
    ShowSelectedMail;
    BtnDelete.Enabled := True;
    BtnFavorite.Enabled := True;
  end
  else
  begin
    FInboxCursor := nil;
    MemoPreview.Clear;
    BtnDelete.Enabled := False;
    BtnFavorite.Enabled := False;
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
