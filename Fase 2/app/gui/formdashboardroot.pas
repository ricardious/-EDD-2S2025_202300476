unit FormDashboardRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  ComboEx, BCMDButton, ATShapeLineBGRA, BCLabel, BCRoundedImage,
  BCMDButtonFocus, JsonUsersLoader, fpjson, jsonparser, AppState, FormLogin,
  Process, FileUtil, SinglyLinkedList, User, RelationsService,
  JsonEmailsLoader, Community, CommunityService, BST, UserService;

type

  { TDashboardRoot }

  TDashboardRoot = class(TForm)
    BtnAddCommunity: TBCMDButtonFocus;
    BtnAddContact1: TBCMDButtonFocus;
    BtnAddUserToCommunity: TBCMDButtonFocus;
    BtnCommunities: TBCMDButton;
    BtnCommunitiesReport: TBCMDButton;
    BtnCommunityMessages: TBCMDButton;
    BtnGenerateCommunityReport: TBCMDButton;
    BtnSearchCommunity: TBCMDButton;
    EditSearchCommunity: TEdit;
    GroupBoxCreateCommunity: TGroupBox;
    GroupAddUsersToCommunity: TGroupBox;
    ImgCommunityReportPreview: TImage;
    LblEditCommunity: TLabeledEdit;
    LblEditEmail: TLabeledEdit;
    LblEditName: TLabeledEdit;
    LblEditName1: TLabeledEdit;
    LblFileSelected1: TBCLabel;
    LblInfo1: TLabel;
    LblInfo3: TLabel;
    LblInfo4: TLabel;
    BtnLogout: TBCMDButton;
    LblFileSelected: TBCLabel;
    BtnSelectFile: TBCMDButton;
    LblLoadData: TBCMDButton;
    BtnBulkLoad: TBCMDButton;
    Image1: TImage;
    LblInfo: TLabel;
    LblSection: TLabel;
    Label2: TLabel;
    LblStatusCommunityReport: TBCLabel;
    LvCommunityMessages: TListView;
    MemoJson: TMemo;
    MemoPreview: TMemo;
    PanelCommunityControl: TPanel;
    PanelCommunityReport: TPanel;
    PanelControls: TPanel;
    PanelBottom: TPanel;
    PanelBulk: TPanel;
    PanelControls1: TPanel;
    PanelControls3: TPanel;
    PanelCommunityMessages: TPanel;
    PanelCommunityMessagesBody: TPanel;
    PanelTop: TPanel;
    PanelBody: TPanel;
    PanelSidebar: TPanel;
    ScrollBoxCommunity: TScrollBox;
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    Splitter1: TSplitter;
    procedure BtnAddCommunityClick(Sender: TObject);
    procedure BtnAddUserToCommunityClick(Sender: TObject);
    procedure BtnBulkLoadClick(Sender: TObject);
    procedure BtnCommunitiesClick(Sender: TObject);
    procedure BtnCommunitiesReportClick(Sender: TObject);
    procedure BtnCommunityMessagesClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnSearchCommunityClick(Sender: TObject);
    procedure BtnSelectFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure LblLoadDataClick(Sender: TObject);
  private
    procedure ShowPanel(APanel: TPanel);
    procedure SetupPreview(AScroll: TScrollBox; AImg: TImage);
    procedure CenterPreview(AScroll: TScrollBox; AImg: TImage);
    procedure ImgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift{%H-}: TShiftState; X, Y: integer);
    procedure ImgPreviewMouseMove(Sender: TObject; Shift{%H-}: TShiftState;
      X, Y: integer);
    procedure ImgPreviewMouseUp(Sender: TObject; Button{%H-}: TMouseButton;
      Shift{%H-}: TShiftState; X{%H-}, Y{%H-}: integer);
  private
    FSelectedFile: string;
    FDragging: boolean;
    FLast: TPoint;
  private
    procedure RefreshCommunityMessages(const CommunityName: string);
    procedure LvCommunityMessagesSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);
    function TruncateText(const S: string; MaxLen: integer): string;
  public

  end;

var
  DashboardRoot: TDashboardRoot;

implementation

{$R *.lfm}

{ TDashboardRoot }

procedure TDashboardRoot.ShowPanel(APanel: TPanel);
begin
  PanelBulk.Visible := False;
  PanelCommunityControl.Visible := False;
  PanelCommunityReport.Visible := False;
  PanelCommunityMessages.Visible := False;

  if Assigned(APanel) then
  begin
    APanel.Visible := True;
  end;
end;

procedure TDashboardRoot.RefreshCommunityMessages(const CommunityName: string);
var
  C: PCommunity;
  Node: PSinglyNode;
  M: PCommunityMessage;
  It: TListItem;
begin
  LvCommunityMessages.Items.BeginUpdate;
  try
    LvCommunityMessages.Items.Clear;
    MemoPreview.Clear;

    C := FindCommunity(Trim(CommunityName));
    if C = nil then
    begin
      ShowMessage('Community not found.');
      Exit;
    end;

    Node := C^.Messages.Head;
    while Node <> nil do
    begin
      M := PCommunityMessage(Node^.Data);
      if M <> nil then
      begin
        It := LvCommunityMessages.Items.Add;
        It.Caption := M^.AuthorEmail;                                   // Email
        It.SubItems.Add(TruncateText(M^.Content, 60));                  // Message
        It.SubItems.Add(FormatDateTime('yyyy-mm-dd hh:nn', M^.PostedAt));
        // Published Date
        It.Data := M;
      end;
      Node := Node^.Next;
    end;

    LvCommunityMessages.SortType := stText;
  finally
    LvCommunityMessages.Items.EndUpdate;
  end;
end;

procedure TDashboardRoot.LvCommunityMessagesSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
var
  M: PCommunityMessage;
begin
  if Selected and (Item <> nil) then
  begin
    M := PCommunityMessage(Item.Data);
    if M <> nil then
    begin
      MemoPreview.Lines.Text :=
        'Author: ' + M^.AuthorEmail + LineEnding + 'Published: ' +
        FormatDateTime('yyyy-mm-dd hh:nn', M^.PostedAt) + LineEnding +
        LineEnding + M^.Content;
    end;
  end
  else
    MemoPreview.Clear;
end;

function TDashboardRoot.TruncateText(const S: string; MaxLen: integer): string;
begin
  if Length(S) > MaxLen then
    Result := Copy(S, 1, MaxLen) + '...'
  else
    Result := S;
end;

procedure TDashboardRoot.SetupPreview(AScroll: TScrollBox; AImg: TImage);
begin
  AImg.Parent := AScroll;
  AScroll.DoubleBuffered := True;
  AImg.Cursor := crHandPoint;

  AImg.OnMouseDown := @ImgPreviewMouseDown;
  AImg.OnMouseMove := @ImgPreviewMouseMove;
  AImg.OnMouseUp := @ImgPreviewMouseUp;
end;

procedure TDashboardRoot.CenterPreview(AScroll: TScrollBox; AImg: TImage);
var
  needH, needV: boolean;
begin
  needH := AImg.Width > AScroll.ClientWidth;
  needV := AImg.Height > AScroll.ClientHeight;

  if (not needH) and (not needV) then
  begin
    AImg.Left := (AScroll.ClientWidth - AImg.Width) div 2;
    AImg.Top := (AScroll.ClientHeight - AImg.Height) div 2;
    AScroll.HorzScrollBar.Position := 0;
    AScroll.VertScrollBar.Position := 0;
  end
  else
  begin
    if needH then
    begin
      AImg.Left := 0;
      AScroll.HorzScrollBar.Position := 0;
    end
    else
      AImg.Left := (AScroll.ClientWidth - AImg.Width) div 2;

    if needV then
    begin
      AImg.Top := 0;
      AScroll.VertScrollBar.Position := 0;
    end
    else
      AImg.Top := (AScroll.ClientHeight - AImg.Height) div 2;
  end;
end;

procedure TDashboardRoot.ImgPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if Button = mbLeft then
  begin
    FDragging := True;
    FLast := Point(X, Y);
    TImage(Sender).Cursor := crSizeAll;
  end;
end;

procedure TDashboardRoot.ImgPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
var
  dx, dy: integer;
  S: TScrollBox;
begin
  if not FDragging then Exit;
  dx := X - FLast.X;
  dy := Y - FLast.Y;
  S := TScrollBox(TImage(Sender).Parent);
  S.ScrollBy(dx, dy);
end;

procedure TDashboardRoot.ImgPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  FDragging := False;
  TImage(Sender).Cursor := crHandPoint;
end;

function UserToString(Data: Pointer): string;
var
  U: PUser;
begin
  U := PUser(Data);
  Result :=
    'ID: ' + IntToStr(U^.Id) + LineEnding + 'Nombre: ' + U^.Name +
    LineEnding + 'Usuario: ' + U^.Username + LineEnding + 'Email: ' +
    U^.Email + LineEnding + 'Teléfono: ' + U^.Phone;
end;

procedure TDashboardRoot.BtnBulkLoadClick(Sender: TObject);
begin
  ShowPanel(PanelBulk);
end;

procedure TDashboardRoot.BtnAddCommunityClick(Sender: TObject);
var
  R: TAddCommunityResult;
  CommunityName: string;
begin
  CommunityName := Trim(LblEditCommunity.Text);
  R := AddCommunity(CommunityName, Now);

  case R of
    acrOK:
    begin
      ShowMessage('Community created.');
      LblEditCommunity.Clear;
    end;
    acrEmptyName:
      ShowMessage('Please enter a community name.');
    acrAlreadyExists:
      ShowMessage('A community with that name already exists.');
  end;
end;

procedure TDashboardRoot.BtnAddUserToCommunityClick(Sender: TObject);
var
  CommunityName, UserEmail: string;
  R: TAddMemberResult;
begin
  CommunityName := Trim(LblEditName.Text);
  UserEmail := Trim(LblEditEmail.Text);

  R := AddUserToCommunity(CommunityName, UserEmail);

  case R of
    amrOK:
    begin
      ShowMessage('User added to community.');
      LblEditEmail.Clear;
    end;
    amrEmpty:
      ShowMessage('Please enter community name and user email.');
    amrCommunityNotFound:
      ShowMessage('Community not found.');
    amrUserNotFound:
      ShowMessage('User email not found in registered users.');
    amrAlreadyMember:
      ShowMessage('User is already a member of this community.');
  end;
end;

procedure TDashboardRoot.BtnCommunitiesClick(Sender: TObject);
begin
  ShowPanel(PanelCommunityControl);
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

procedure TDashboardRoot.BtnCommunitiesReportClick(Sender: TObject);
const
  DOT_EXT = '.dot';
  PNG_EXT = '.png';
var
  BaseDir, DotPath, PngPath: string;
begin
  EnsureCommunitiesTree;

  BaseDir := ExpandFileName(ExtractFilePath(Application.ExeName) +
    'Root-Reportes' + DirectorySeparator);
  ForceDirectories(BaseDir);

  // Output paths
  DotPath := BaseDir + 'Comunidades' + DOT_EXT;
  PngPath := BaseDir + 'Comunidades' + PNG_EXT;

  try
    GenerateCommunitiesDot(DotPath);
  except
    on E: Exception do
    begin
      MessageDlg('Communities Report',
        'Error generating DOT: ' + E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if not RunDotToPng(DotPath, PngPath) then
  begin
    MessageDlg('Communities Report',
      'Could not generate PNG. Is Graphviz ("dot" command) installed?',
      mtError, [mbOK], 0);
    Exit;
  end;

  try
    ImgCommunityReportPreview.Picture.LoadFromFile(PngPath);
    CenterPreview(ScrollBoxCommunity, ImgCommunityReportPreview);
  except
    on E: Exception do
    begin
      MessageDlg('Communities Report',
        'PNG generated, but preview could not be loaded: ' + E.Message,
        mtWarning, [mbOK], 0);
    end;
  end;

  LblStatusCommunityReport.Caption := 'Report generated at: ' + BaseDir;
  ShowMessage('Communities Report generated at: ' + BaseDir);

  ShowPanel(PanelCommunityReport);
end;

procedure TDashboardRoot.BtnCommunityMessagesClick(Sender: TObject);
begin
  LvCommunityMessages.Items.Clear;
  MemoPreview.Clear;
  ShowPanel(PanelCommunityMessages);
end;

procedure TDashboardRoot.BtnLogoutClick(Sender: TObject);
begin
  CurrentUser := nil;
  Self.Close;
  SignIn.Show;
end;

procedure TDashboardRoot.BtnSearchCommunityClick(Sender: TObject);
var
  Q: string;
begin
  Q := Trim(EditSearchCommunity.Text);
  if Q = '' then
  begin
    ShowMessage('Enter a community name to search.');
    Exit;
  end;

  RefreshCommunityMessages(Q);

  // ShowPanel(PanelCommunityMessages);
end;

procedure TDashboardRoot.BtnSelectFileClick(Sender: TObject);
var
  OD: TOpenDialog;
begin
  OD := TOpenDialog.Create(Self);
  try
    OD.Filter := 'JSON files|*.json';
    if DirectoryExists('data') then
      OD.InitialDir := 'data';

    if OD.Execute then
    begin
      FSelectedFile := OD.FileName;
      LblFileSelected.Caption := ExtractFileName(FSelectedFile);

      try
        MemoJson.Lines.LoadFromFile(FSelectedFile);
        MemoJson.ReadOnly := False;
      except
        on E: Exception do
        begin
          LblFileSelected.Caption := 'Error loading file';
          MemoJson.Lines.Text :=
            '{' + LineEnding + '  "error": "' +
            StringReplace(E.Message, '"', '\"', [rfReplaceAll]) +
            '"' + LineEnding + '}';
        end;
      end;
    end;
  finally
    OD.Free;
  end;
end;

procedure TDashboardRoot.FormShow(Sender: TObject);
begin
  BoundsRect := Screen.WorkAreaRect;
  EnsureCommunitiesTree;
  SetupPreview(ScrollBoxCommunity, ImgCommunityReportPreview);
end;

procedure TDashboardRoot.LblLoadDataClick(Sender: TObject);
var
  JsonContent, SourceLabel, ReportMessage: string;
  Data: TJSONData;
  LoadedUsers, LoadedEmails, SkippedEmails: integer;
  HasUsers, HasEmails: boolean;
begin
  if Trim(MemoJson.Text) <> '' then
  begin
    JsonContent := MemoJson.Text;
    SourceLabel := 'Memo JSON';
  end
  else if FSelectedFile <> '' then
  begin
    try
      MemoJson.Lines.LoadFromFile(FSelectedFile);
      JsonContent := MemoJson.Text;
      SourceLabel := 'File: ' + ExtractFileName(FSelectedFile);
    except
      on E: Exception do
      begin
        MessageDlg('File read error', E.Message, mtError, [mbOK], 0);
        Exit;
      end;
    end;
  end
  else
  begin
    MessageDlg('Bulk Load', 'Paste JSON in the memo or select a JSON file first.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  LoadedUsers := 0;
  LoadedEmails := 0;
  SkippedEmails := 0;
  HasUsers := False;
  HasEmails := False;

  try
    Data := GetJSON(JsonContent);
    try
      if Data is TJSONObject then
      begin
        if (TJSONObject(Data).Find('usuarios') <> nil) and
          (TJSONObject(Data).Find('usuarios').JSONType = jtArray) then
          HasUsers := True;

        if (TJSONObject(Data).Find('correos') <> nil) and
          (TJSONObject(Data).Find('correos').JSONType = jtArray) then
          HasEmails := True;
      end;
    finally
      Data.Free;
    end;
  except
    on E: Exception do
    begin
      MessageDlg('Invalid JSON', E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  if not HasUsers and not HasEmails then
  begin
    MessageDlg('Bulk Load',
      'The JSON root must be an object and contain a "users" or "emails" array.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  if HasUsers then
  begin
    try
      LoadedUsers := LoadUsersFromJsonContent(JsonContent, Users);
    except
      on E: Exception do
        MessageDlg('Bulk Load (Users) error', E.Message, mtError, [mbOK], 0);
    end;
  end;

  if HasEmails then
  begin
    try
      LoadedEmails := LoadEmailsFromJsonContent(JsonContent, Users, SkippedEmails);
    except
      on E: Exception do
        MessageDlg('Bulk Load (Emails) error', E.Message, mtError, [mbOK], 0);
    end;
  end;

  ReportMessage := 'Bulk Load complete:' + LineEnding;
  if HasUsers then
    ReportMessage := ReportMessage + Format('Users loaded: %d', [LoadedUsers]) +
      LineEnding;
  if HasEmails then
  begin
    ReportMessage := ReportMessage + Format('Emails loaded: %d', [LoadedEmails]) +
      LineEnding;
    ReportMessage := ReportMessage +
      Format('Emails skipped (recipient not found): %d', [SkippedEmails]);
  end;

  MessageDlg('Bulk Load', Trim(ReportMessage), mtInformation, [mbOK], 0);
end;

end.
