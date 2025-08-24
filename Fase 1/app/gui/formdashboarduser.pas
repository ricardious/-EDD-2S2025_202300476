unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, BCMDButton, ATShapeLineBGRA, BCLabel, BCRoundedImage,
  BGRACustomDrawn, BCButton, JsonUsersLoader, fpjson, jsonparser, AppState;

type

  { TDashboardRoot }

  TDashboardRoot = class(TForm)
    BtnSend: TBCButton;
    LblMessage: TBCLabel;
    EditSubject: TEdit;
    LblSubject: TBCLabel;
    EditRecipient: TEdit;
    LblRecipient: TBCLabel;
    BCRoundedImage2: TBCRoundedImage;
    BtnContacts: TBCMDButton;
    BtnDelete: TBCMDButton;
    BtnUpdateProfile: TBCMDButton;
    BtnSelectFile2: TBCMDButton;
    BtnAddContact: TBCMDButton;
    BtnScheduledEmails: TBCMDButton;
    BtnScheduleEmail: TBCMDButton;
    BtnGenerateReports: TBCMDButton;
    LblFileSelected: TBCLabel;
    LblFileSelected1: TBCLabel;
    LblInfo1: TLabel;
    LblInfo2: TLabel;
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
    Panel2: TPanel;
    PanelInboxBody: TPanel;
    PanelControls2: TPanel;
    PanelRelationsReport: TPanel;
    PanelSendEmail: TPanel;
    PanelControls: TPanel;
    PanelInbox: TPanel;
    PanelTop: TPanel;
    PanelBody: TPanel;
    PanelSidebar: TPanel;
    Shape1: TShape;
    ShapeLineBGRA1: TShapeLineBGRA;
    Splitter1: TSplitter;
    procedure BtnTrashClick(Sender: TObject);
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
  PanelInbox.Visible := False;
  PanelSendEmail.Visible := False;
  PanelRelationsReport.Visible := False;

  if Assigned(APanel) then
  begin
    APanel.Visible := True;
  end;
end;

procedure TDashboardRoot.PanelControlsClick(Sender: TObject);
begin

end;


procedure TDashboardRoot.BtnTrashClick(Sender: TObject);
begin
  ShowPanel(PanelSendEmail);
end;



end.

