unit FormDashboardUser;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, MaskEdit, Grids, DateTimePicker, BCMDButton, ATShapeLineBGRA,
  BCLabel, BCRoundedImage, BGRACustomDrawn, BCButton, BGRAThemeButton,
  BCMDButtonFocus, JsonUsersLoader, fpjson, jsonparser, AppState;

type

  { TDashboardUser }

  TDashboardUser = class(TForm)
    BCMDButtonFocus1: TBCMDButtonFocus;
    BtnDelete1: TBCMDButton;
    BtnLogout: TBCMDButton;
    BtnSelectFile: TBCMDButton;
    BtnSelectFile1: TBCMDButton;
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
    LblEditName1: TLabeledEdit;
    LblEditName2: TLabeledEdit;
    LblEditName3: TLabeledEdit;
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
    procedure BtnTrashClick(Sender: TObject);
    procedure PanelControlsClick(Sender: TObject);
    procedure PanelInboxClick(Sender: TObject);
  private
    procedure ShowPanel(APanel: TPanel);
  private

  public

  end;

var
  DashboardUser: TDashboardUser;

implementation

{$R *.lfm}

{ TDashboardUser }


procedure TDashboardUser.ShowPanel(APanel: TPanel);
begin

end;

procedure TDashboardUser.PanelControlsClick(Sender: TObject);
begin

end;

procedure TDashboardUser.PanelInboxClick(Sender: TObject);
begin

end;


procedure TDashboardUser.BtnTrashClick(Sender: TObject);
begin
end;



end.

