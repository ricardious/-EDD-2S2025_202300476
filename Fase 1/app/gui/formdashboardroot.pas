unit FormDashboardRoot;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BCMDButton, ATShapeLineBGRA, BCLabel, BCRoundedImage, BGRAImageManipulation,
  BGRAImageList;

type

  { TDashboardRoot }

  TDashboardRoot = class(TForm)
    BCRoundedImage1: TBCRoundedImage;
    BtnSelectFile1: TBCMDButton;
    LblFileSelected: TBCLabel;
    BtnSelectFile: TBCMDButton;
    LblInfo1: TLabel;
    LblLoadData: TBCMDButton;
    BtnBulkLoad: TBCMDButton;
    BtnReportUsers: TBCMDButton;
    BtnReportRelations: TBCMDButton;
    Image1: TImage;
    LblInfo: TLabel;
    LblLoadData1: TBCMDButton;
    LblSection: TLabel;
    Label2: TLabel;
    MemoJson: TMemo;
    PanelBottom1: TPanel;
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
    Splitter1: TSplitter;
    procedure PanelControlsClick(Sender: TObject);
  private

  public

  end;

var
  DashboardRoot: TDashboardRoot;

implementation

{$R *.lfm}

{ TDashboardRoot }

procedure TDashboardRoot.PanelControlsClick(Sender: TObject);
begin

end;

end.

