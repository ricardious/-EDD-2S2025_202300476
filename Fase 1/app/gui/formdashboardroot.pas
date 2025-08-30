unit FormDashboardRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComboEx, BCMDButton, ATShapeLineBGRA, BCLabel, BCRoundedImage,
  BCMDButtonFocus, JsonUsersLoader, fpjson, jsonparser, AppState, FormLogin,
  Process, FileUtil, SinglyLinkedList, User, RelationsService;

type

  { TDashboardRoot }

  TDashboardRoot = class(TForm)
    BtnAddCommunity: TBCMDButtonFocus;
    BtnAddContact1: TBCMDButtonFocus;
    BtnCommunities: TBCMDButton;
    GroupBoxCreateCommunity: TGroupBox;
    GroupAddUsersToCommunity: TGroupBox;
    ImgRelationsReportPreview: TImage;
    LblEditEmail: TLabeledEdit;
    LblEditName: TLabeledEdit;
    LblEditName1: TLabeledEdit;
    LblInfo4: TLabel;
    LblStatusUserReport: TBCLabel;
    BtnLogout: TBCMDButton;
    BtnGenerateUserReport: TBCMDButton;
    BtnGenerateRelationsReport: TBCMDButton;
    ImgUserReportPreview: TImage;
    LblFileSelected: TBCLabel;
    BtnSelectFile: TBCMDButton;
    LblFileSelected1: TBCLabel;
    LblInfo1: TLabel;
    LblInfo2: TLabel;
    LblLoadData: TBCMDButton;
    BtnBulkLoad: TBCMDButton;
    BtnReportUsers: TBCMDButton;
    BtnReportRelations: TBCMDButton;
    Image1: TImage;
    LblInfo: TLabel;
    LblSection: TLabel;
    Label2: TLabel;
    MemoJson: TMemo;
    PanelCommunityControl: TPanel;
    PanelControls2: TPanel;
    PanelRelationsReport: TPanel;
    PanelUsersReport: TPanel;
    PanelControls: TPanel;
    PanelBottom: TPanel;
    PanelBulk: TPanel;
    PanelControls1: TPanel;
    PanelTop: TPanel;
    PanelBody: TPanel;
    PanelSidebar: TPanel;
    ScrollBoxUsers: TScrollBox;
    ScrollBoxRelations: TScrollBox;
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    procedure BtnBulkLoadClick(Sender: TObject);
    procedure BtnCommunitiesClick(Sender: TObject);
    procedure BtnGenerateRelationsReportClick(Sender: TObject);
    procedure BtnGenerateUserReportClick(Sender: TObject);
    procedure BtnLogoutClick(Sender: TObject);
    procedure BtnReportRelationsClick(Sender: TObject);
    procedure BtnReportUsersClick(Sender: TObject);
    procedure BtnSelectFileClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ImgUserReportPreviewMouseDown(Sender: TObject;
      Button: TMouseButton; Shift{%H-}: TShiftState; X, Y: integer);
    procedure ImgUserReportPreviewMouseMove(Sender: TObject;
      Shift{%H-}: TShiftState; X, Y: integer);
    procedure ImgUserReportPreviewMouseUp(Sender: TObject;
      Button{%H-}: TMouseButton; Shift{%H-}: TShiftState; X{%H-}, Y{%H-}: integer);
    procedure LblLoadDataClick(Sender: TObject);
    procedure ScrollBoxRelationsResize(Sender: TObject);
    procedure ScrollBoxUsersResize(Sender: TObject);
  private
    procedure ShowPanel(APanel: TPanel);
    procedure SetupPreview(AScroll: TScrollBox; AImg: TImage);
    procedure CenterPreview(AScroll: TScrollBox; AImg: TImage);
    procedure ImgPreviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure ImgPreviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure ImgPreviewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
  private
    FSelectedFile: string;
    FDragging: boolean;
    FLast: TPoint;
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
  PanelUsersReport.Visible := False;
  PanelRelationsReport.Visible := False;

  if Assigned(APanel) then
  begin
    APanel.Visible := True;
  end;
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

procedure TDashboardRoot.BtnCommunitiesClick(Sender: TObject);
begin
  ShowPanel(PanelCommunityControl);
end;

procedure TDashboardRoot.BtnGenerateRelationsReportClick(Sender: TObject);
const
  GraphvizCmd = 'dot';
var
  ReportDir, DotFile, PngFile, ExeDir, ParentDir: string;
  Proc: TProcess;
  Visualizer: TEmailMatrixVisualizer;
begin
  ExeDir := ExtractFilePath(Application.ExeName);
  ParentDir := ExpandFileName(ExeDir + '..' + DirectorySeparator);
  ReportDir := ParentDir + 'data' + DirectorySeparator + 'output' +
    DirectorySeparator + 'Root-Reportes';
  ForceDirectories(ReportDir);

  DotFile := ReportDir + DirectorySeparator + 'RelationsReport.dot';
  PngFile := ReportDir + DirectorySeparator + 'RelationsReport.png';

  if Count(Users) = 0 then
  begin
    MessageDlg('Relations report', 'No users loaded.', mtInformation, [mbOK], 0);
    Exit;
  end;

  Screen.Cursor := crHourGlass;
  try
    Visualizer := TEmailMatrixVisualizer.Create(1000);
    try
      Visualizer.BuildMatrixFromUsers(Users);
      Visualizer.GenerateVisualization(DotFile);
    finally
      Visualizer.Free;
    end;

    Proc := TProcess.Create(nil);
    try
      Proc.Executable := GraphvizCmd;
      Proc.Parameters.Add('-Tpng');
      Proc.Parameters.Add('-o' + PngFile);
      Proc.Parameters.Add(DotFile);
      Proc.Options := [poWaitOnExit];
      Proc.Execute;
      if Proc.ExitStatus <> 0 then
        raise Exception.Create('Graphviz error code ' + IntToStr(Proc.ExitStatus));
    finally
      Proc.Free;
    end;

    if FileExists(PngFile) then
    begin
      SetupPreview(ScrollBoxRelations, ImgRelationsReportPreview);
      ImgRelationsReportPreview.Picture.LoadFromFile(PngFile);
      CenterPreview(ScrollBoxRelations, ImgRelationsReportPreview);
      MessageDlg('Relations report', 'PNG generated:' + LineEnding + PngFile,
        mtInformation, [mbOK], 0);
    end
    else
      raise Exception.Create('PNG file not created');

  except
    on E: Exception do
      MessageDlg('Relations report error', E.Message, mtError, [mbOK], 0);
  end;
  Screen.Cursor := crDefault;
end;

procedure TDashboardRoot.BtnGenerateUserReportClick(Sender: TObject);
const
  GraphvizCmd = 'dot';
var
  ReportDir, DotFilePath, PngFilePath, ExeDir, ParentDir: string;
  AProcess: TProcess;
  PublicUsers: TSinglyLinkedList;
  N: PSinglyNode;
  U: PUser;
begin
  LblStatusUserReport.Caption := 'Status: Preparing directories...';
  Application.ProcessMessages;

  ExeDir := ExtractFilePath(Application.ExeName);
  ParentDir := ExpandFileName(ExeDir + '..' + DirectorySeparator);
  ReportDir := ParentDir + 'data' + DirectorySeparator + 'output' +
    DirectorySeparator + 'Root-Reportes';
  try
    ForceDirectories(ReportDir);
  except
    on E: Exception do
    begin
      LblStatusUserReport.Font.Color := clRed;
      LblStatusUserReport.Caption := 'Status: Directory error';
      MessageDlg('Directory Error',
        'Could not create the reports directory: ' + E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  DotFilePath := ReportDir + DirectorySeparator + 'UsersReport.dot';
  PngFilePath := ReportDir + DirectorySeparator + 'UsersReport.png';

  if Count(Users) = 0 then
  begin
    LblStatusUserReport.Font.Color := clGray;
    LblStatusUserReport.Caption := 'Status: No users to report';
    MessageDlg('User Report',
      'There are no users loaded in the system to generate a report.',
      mtInformation, [mbOK], 0);
    Exit;
  end;

  LblStatusUserReport.Font.Color := clBlue;
  LblStatusUserReport.Caption := 'Status: Building DOT...';
  Application.ProcessMessages;

  PublicUsers := Default(TSinglyLinkedList);
  Init(PublicUsers);
  N := Users.Head;
  if N <> nil then
    N := N^.Next;

  while N <> nil do
  begin
    U := PUser(N^.Data);
    if (U = nil) or not SameText(U^.Username, 'root') then
      InsertLast(PublicUsers, N^.Data);
    N := N^.Next;
  end;

  // if Count(PublicUsers) = 0 then
  // begin
  //   MessageDlg('User Report',
  //     'No public users to include (root excluded).',
  //     mtInformation, [mbOK], 0);
  //   Exit;
  // end;

  try
    GenerateDotFile(PublicUsers, DotFilePath, @UserToString);
  except
    on E: Exception do
    begin
      LblStatusUserReport.Font.Color := clRed;
      LblStatusUserReport.Caption := 'Status: DOT generation failed';
      MessageDlg('File Error', 'Failed to generate the DOT file: ' +
        E.Message, mtError, [mbOK], 0);
      Clear(PublicUsers);
      Exit;
    end;
  end;

  Clear(PublicUsers);

  LblStatusUserReport.Font.Color := clBlue;
  LblStatusUserReport.Caption := 'Status: Running Graphviz...';
  Application.ProcessMessages;

  AProcess := TProcess.Create(nil);
  try
    AProcess.Executable := GraphvizCmd;
    AProcess.Parameters.Add('-Tpng');
    AProcess.Parameters.Add('-o' + PngFilePath);
    AProcess.Parameters.Add(DotFilePath);
    AProcess.Options := [poWaitOnExit];

    AProcess.Execute;

    if AProcess.ExitStatus <> 0 then
    begin
      LblStatusUserReport.Font.Color := clRed;
      LblStatusUserReport.Caption := 'Status: Graphviz failed';
      MessageDlg('Graphviz Error',
        'The "dot" command failed. Make sure Graphviz is installed ' +
        'and its path is in the system''s PATH environment variable.',
        mtError, [mbOK], 0);
      Exit;
    end;
  finally
    AProcess.Free;
  end;

  LblStatusUserReport.Font.Color := clBlue;
  LblStatusUserReport.Caption := 'Status: Loading preview...';
  Application.ProcessMessages;

  if FileExists(PngFilePath) then
  begin
    try
      SetupPreview(ScrollBoxUsers, ImgUserReportPreview);
      ImgUserReportPreview.Picture.LoadFromFile(PngFilePath);
      CenterPreview(ScrollBoxUsers, ImgUserReportPreview);
      LblStatusUserReport.Font.Color := clGreen;
      LblStatusUserReport.Caption := 'Status: Ready';
      MessageDlg('Success',
        'Report generated successfully at: ' + PngFilePath, mtInformation, [mbOK], 0);
    except
      on E: Exception do
      begin
        LblStatusUserReport.Font.Color := clRed;
        LblStatusUserReport.Caption := 'Status: Load error';
        MessageDlg('Load Error', 'Could not load the report image: ' +
          E.Message, mtError, [mbOK], 0);
      end;
    end;
  end
  else
  begin
    LblStatusUserReport.Font.Color := clRed;
    LblStatusUserReport.Caption := 'Status: PNG not found';
    MessageDlg('Error', 'The image file was not found after running Graphviz.',
      mtError, [mbOK], 0);
  end;
end;

procedure TDashboardRoot.BtnLogoutClick(Sender: TObject);
begin
  CurrentUser := nil;
  Self.Close;
  SignIn.Show;
end;

procedure TDashboardRoot.BtnReportRelationsClick(Sender: TObject);
begin
  ShowPanel(PanelRelationsReport);
end;

procedure TDashboardRoot.BtnReportUsersClick(Sender: TObject);
begin
  ShowPanel(PanelUsersReport);
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
end;

procedure TDashboardRoot.ImgUserReportPreviewMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin

end;

procedure TDashboardRoot.ImgUserReportPreviewMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: integer);
begin

end;

procedure TDashboardRoot.ImgUserReportPreviewMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin

end;

procedure TDashboardRoot.LblLoadDataClick(Sender: TObject);
var
  Data: TJSONData;
  LoadedCount: integer;
  JsonContent, SourceLabel: string;
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
    MessageDlg('Bulk Load',
      'Paste JSON in the memo or select a JSON file first.',
      mtWarning, [mbOK], 0);
    Exit;
  end;

  try
    Data := GetJSON(JsonContent);
    Data.Free;
  except
    on E: Exception do
    begin
      MessageDlg('Invalid JSON', E.Message, mtError, [mbOK], 0);
      Exit;
    end;
  end;

  try
    LoadedCount := LoadUsersFromJsonContent(JsonContent, Users);
    MessageDlg('Bulk Load',
      Format('Successfully loaded %d users from %s', [LoadedCount, SourceLabel]),
      mtInformation, [mbOK], 0);
  except
    on E: Exception do
      MessageDlg('Bulk Load error', E.Message, mtError, [mbOK], 0);
  end;
end;

procedure TDashboardRoot.ScrollBoxRelationsResize(Sender: TObject);
begin
  CenterPreview(ScrollBoxRelations, ImgRelationsReportPreview);
end;

procedure TDashboardRoot.ScrollBoxUsersResize(Sender: TObject);
begin
  CenterPreview(ScrollBoxUsers, ImgUserReportPreview);
end;

end.
