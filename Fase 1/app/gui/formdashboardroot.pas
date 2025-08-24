unit FormDashboardRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCMDButton, ATShapeLineBGRA, BCLabel, BCRoundedImage, JsonUsersLoader, fpjson,
  jsonparser, AppState;

type

  { TDashboardRoot }

  TDashboardRoot = class(TForm)
    BCLabel1: TBCLabel;
    BCRoundedImage1: TBCRoundedImage;
    BCRoundedImage2: TBCRoundedImage;
    BtnSelectFile1: TBCMDButton;
    BtnSelectFile2: TBCMDButton;
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
    Panel1: TPanel;
    Panel2: TPanel;
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
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    procedure BtnBulkLoadClick(Sender: TObject);
    procedure BtnReportRelationsClick(Sender: TObject);
    procedure BtnReportUsersClick(Sender: TObject);
    procedure BtnSelectFileClick(Sender: TObject);
    procedure LblLoadDataClick(Sender: TObject);
    procedure PanelControlsClick(Sender: TObject);
  private
    procedure ShowPanel(APanel: TPanel);
  private
     FSelectedFile: string;
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

procedure TDashboardRoot.PanelControlsClick(Sender: TObject);
begin

end;

procedure TDashboardRoot.BtnBulkLoadClick(Sender: TObject);
begin
  ShowPanel(PanelBulk);
end;

procedure TDashboardRoot.BtnReportRelationsClick(Sender: TObject);
begin
  ShowPanel(PanelUsersReport);
end;

procedure TDashboardRoot.BtnReportUsersClick(Sender: TObject);
begin
  ShowPanel(PanelRelationsReport);
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
            '{' + LineEnding +
            '  "error": "' + StringReplace(E.Message, '"', '\"', [rfReplaceAll]) + '"' + LineEnding +
            '}';
        end;
      end;
    end;
  finally
    OD.Free;
  end;
end;

procedure TDashboardRoot.LblLoadDataClick(Sender: TObject);
var
  Data: TJSONData;
  LoadedCount: Integer;
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
               Format('Successfully loaded %d users from %s',
                      [LoadedCount, SourceLabel]),
               mtInformation, [mbOK], 0);

    MemoJson.Lines.Text := UsersDump;
  except
    on E: Exception do
      MessageDlg('Bulk Load error', E.Message, mtError, [mbOK], 0);
  end;
end;



end.

