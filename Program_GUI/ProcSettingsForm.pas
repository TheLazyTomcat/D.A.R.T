unit ProcSettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, DART_ProcessingSettings;

type
{$IFNDEF FPC}
  // combobox in delphi does not have public OnMouseMove event
  TComboBox = class(StdCtrls.TComboBox)
  published
    property OnMouseMove;
  end;
{$ENDIF}

  TfProcSettingsForm = class(TForm)
    lblOptionDescription: TLabel;
    meOptionDecription: TMemo;
    brCommonSettings: TGroupBox;
    gbArchiveSettings: TGroupBox;
    scbArchiveSettings: TScrollBox;
    lblArchiveFileCpt: TLabel;
    lblArchiveTypeCpt: TLabel;
    lblArchiveType: TLabel;
    lblArchiveFile: TLabel;
    cbForceArchiveType: TCheckBox;
    cmbForcedArchiveType: TComboBox;
    bvlHorSplitFile: TBevel;
    rbRebuild: TRadioButton;
    rbExtract: TRadioButton;
    rbConvert: TRadioButton;
    lbleTarget: TLabeledEdit;
    btnBrowseTarget: TButton;
    bvlHorSplitTarget: TBevel;
    cbIgnoreArchiveSignature: TCheckBox;
    cbInMemoryProcessing: TCheckBox;
    cbIgnoreErroneousEntries: TCheckBox;
    cmbConvertTo: TComboBox;
    lblConvertTo: TLabel;
    bvlMainHorSplit: TBevel;
    btnSaveSettings: TButton;
    btnLoadSettings: TButton;
    btnDefaultSettings: TButton;
    btnAccept: TButton;
    btnClose: TButton;
    diaTargetSave: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbForcedArchiveTypeChange(Sender: TObject);
    procedure lbleTargetChange(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnBrowseTargetClick(Sender: TObject);
  private
    fLocalArchiveProcessingSettings:  TDARTArchiveProcessingSettings;
    fLoading:                         Boolean;
    fAccepted:                        Boolean;
  protected
    procedure LoadOptionsDescription;
    procedure SettingsToForm;
    procedure FormToSettings;
  public
    procedure ShowProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
  published
    procedure CheckBoxClick(Sender: TObject);
    procedure RepairMethodClick(Sender: TObject);
    procedure OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;

var
  fProcSettingsForm: TfProcSettingsForm;

implementation

{$R *.dfm}

uses
  {$WARN UNIT_PLATFORM OFF} FileCtrl{$WARN UNIT_PLATFORM ON},
  StrRect,
  DART_Auxiliary, DART_ProcessingManager;

procedure TfProcSettingsForm.LoadOptionsDescription;
begin
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.SettingsToForm;
begin
lblArchiveFile.Caption := MinimizeName(fLocalArchiveProcessingSettings.Common.ArchivePath,lblArchiveFile.Canvas,lblArchiveFile.Constraints.MaxWidth);
lblArchiveFile.ShowHint := not AnsiSameText(lblArchiveFile.Caption,fLocalArchiveProcessingSettings.Common.ArchivePath) or
                           (lblArchiveFile.Canvas.TextWidth(lblArchiveFile.Caption) > lblArchiveFile.Constraints.MaxWidth);
If lblArchiveFile.ShowHint then
  lblArchiveFile.Hint := fLocalArchiveProcessingSettings.Common.ArchivePath;
lblArchiveType.Caption := DART_ArchiveTypeStrings[fLocalArchiveProcessingSettings.Common.SelectedArchiveType];
cbForceArchiveType.Checked := fLocalArchiveProcessingSettings.Common.SelectedArchiveType in [atSCS_frc,atZIP_frc];
If cbForceArchiveType.Checked then
  case fLocalArchiveProcessingSettings.Common.SelectedArchiveType of
    atSCS_frc:  cmbForcedArchiveType.ItemIndex := 0;
    atZIP_frc:  cmbForcedArchiveType.ItemIndex := 1;
  end
else cmbForcedArchiveType.ItemIndex := 0;
case fLocalArchiveProcessingSettings.Common.RepairMethod of
  rmRebuild:  rbRebuild.Checked := True;
  rmExtract:  rbExtract.Checked := True;
  rmConvert:  rbConvert.Checked := True;
end;
lblConvertTo.Enabled := fLocalArchiveProcessingSettings.Common.RepairMethod = rmConvert;
cmbConvertTo.Enabled := fLocalArchiveProcessingSettings.Common.RepairMethod = rmConvert;
case fLocalArchiveProcessingSettings.Common.ConvertTo of
  katSCS: cmbConvertTo.ItemIndex := 0;
  katZIP: cmbConvertTo.ItemIndex := 1;
else
  case fLocalArchiveProcessingSettings.Common.SelectedArchiveType of
    atSCS_sig,atSCS_frc:
      cmbConvertTo.ItemIndex := 1;
    atZIP_sig,atZIP_frc,atZIP_dft:
      cmbConvertTo.ItemIndex := 0;
  end
end;
lbleTarget.Text := fLocalArchiveProcessingSettings.Common.TargetPath;
cbIgnoreArchiveSignature.Checked := fLocalArchiveProcessingSettings.Common.IgnoreArchiveSignature;
cbInMemoryProcessing.Checked := fLocalArchiveProcessingSettings.Common.InMemoryProcessing;
cbInMemoryProcessing.Enabled := fLocalArchiveProcessingSettings.Auxiliary.InMemoryProcessingAllowed;
cbIgnoreErroneousEntries.Checked := fLocalArchiveProcessingSettings.Common.IgnoreErroneousEntries;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.FormToSettings;
begin
// SelectedArchiveType and RepairMethod are selected in-place
case cmbConvertTo.ItemIndex of
  0:  fLocalArchiveProcessingSettings.Common.ConvertTo := katSCS;
  1:  fLocalArchiveProcessingSettings.Common.ConvertTo := katZIP;
else
  fLocalArchiveProcessingSettings.Common.ConvertTo := katUnknown;
end;
case fLocalArchiveProcessingSettings.Common.RepairMethod of
  rmRebuild,
  rmConvert:  fLocalArchiveProcessingSettings.Common.TargetPath := DART_ExpandFileName(lbleTarget.Text);
  rmExtract:  fLocalArchiveProcessingSettings.Common.TargetPath := IncludeTrailingPathDelimiter(DART_ExpandFileName(lbleTarget.Text));
end;
fLocalArchiveProcessingSettings.Common.IgnoreArchiveSignature := cbIgnoreArchiveSignature.Checked;
fLocalArchiveProcessingSettings.Common.InMemoryProcessing := cbInMemoryProcessing.Checked;
fLocalArchiveProcessingSettings.Common.IgnoreErroneousEntries := cbIgnoreErroneousEntries.Checked;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.CheckBoxClick(Sender: TObject);
begin
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    3:  begin   // cbForceArchiveType
          cmbForcedArchiveType.Enabled := TCheckBox(Sender).Checked;
          If TCheckBox(Sender).Checked then
            case cmbForcedArchiveType.ItemIndex of
              0:  fLocalArchiveProcessingSettings.Common.SelectedArchiveType := atSCS_frc;
              1:  fLocalArchiveProcessingSettings.Common.SelectedArchiveType := atZIP_frc;
            end
          else fLocalArchiveProcessingSettings.Common.SelectedArchiveType := fLocalArchiveProcessingSettings.Common.OriginalArchiveType;
          lblArchiveType.Caption := DART_ArchiveTypeStrings[fLocalArchiveProcessingSettings.Common.SelectedArchiveType];
          //ShowProperFrame;
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.RepairMethodClick(Sender: TObject);
begin
If Sender is TRadioButton then
  begin
    btnBrowseTarget.Tag := TRadioButton(Sender).Tag;
    case TRadioButton(Sender).Tag of
      5:  begin   // rbRebuild
            fLocalArchiveProcessingSettings.Common.RepairMethod := rmRebuild;
            lbleTarget.EditLabel.Caption := 'Output file:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fLocalArchiveProcessingSettings.Common);
          end;
      6:  begin   // rbExtract
            fLocalArchiveProcessingSettings.Common.RepairMethod := rmExtract;
            lbleTarget.EditLabel.Caption := 'Extract into:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fLocalArchiveProcessingSettings.Common);
          end;
      7:  begin   // rbConvert
            fLocalArchiveProcessingSettings.Common.RepairMethod := rmConvert;
            lbleTarget.EditLabel.Caption := 'Output file:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fLocalArchiveProcessingSettings.Common);
          end;
    end;
    lbleTarget.Text := fLocalArchiveProcessingSettings.Common.TargetPath;
    lblConvertTo.Enabled := fLocalArchiveProcessingSettings.Common.RepairMethod = rmConvert;
    cmbConvertTo.Enabled := fLocalArchiveProcessingSettings.Common.RepairMethod = rmConvert;
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If Sender is TControl then
  If meOptionDecription.Tag <> TControl(Sender).Tag then
    begin
      meOptionDecription.Tag := TControl(Sender).Tag;
      //If (TControl(Sender).Tag >= Low(fSettingsDescriptions)) and
      //   (TControl(Sender).Tag <= High(fSettingsDescriptions)) then
      //  meSettingDescription.Text := fSettingsDescriptions[TControl(Sender).Tag]
      //else
        meOptionDecription.Text := 'unknown #' + IntToStr(TControl(Sender).Tag);
    end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Control:  TControl;
begin
// this is for disabled controls
If Sender is TGroupBox then
  begin
    Control := TGroupBox(Sender).ControlAtPos(Point(X,Y),True,True);
    If Assigned(Control) and ((Control is TCheckBox) or (Control is TComboBox)) then
      OptionMouseMove(Control,Shift,X,Y);
  end;
end;

//==============================================================================

procedure TfProcSettingsForm.ShowProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
fLocalArchiveProcessingSettings := ArchiveProcessingSettings;
fLoading := True;
try
  SettingsToForm;
finally
  fLoading := False;
end;
cbForceArchiveType.OnClick(cbForceArchiveType);
//ShowProperFrame
fAccepted := False;
ShowModal;
If fAccepted then
  begin
    FormToSettings;
    ArchiveProcessingSettings := fLocalArchiveProcessingSettings;
  end;
end;

//==============================================================================

procedure TfProcSettingsForm.FormCreate(Sender: TObject);
var
  i:  TDARTKnownArchiveTypes;
begin
For i := Low(TDARTKnownArchiveTypes) to High(TDARTKnownArchiveTypes) do
  If i <> katUnknown then
    begin
      cmbForcedArchiveType.Items.Add(DART_KnownArchiveTypeStrings[i]);
      cmbConvertTo.Items.Add(DART_KnownArchiveTypeStrings[i]);
    end;
cmbForcedArchiveType.OnMouseMove := OptionMouseMove;
cmbConvertTo.OnMouseMove := OptionMouseMove;
gbArchiveSettings.DoubleBuffered := True;
scbArchiveSettings.DoubleBuffered := True;    
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.FormShow(Sender: TObject);
begin
meOptionDecription.Text := 'Move cursor over specific setting to see its description.';
btnClose.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.cmbForcedArchiveTypeChange(Sender: TObject);
begin
If cbForceArchiveType.Checked then
  begin
    case cmbForcedArchiveType.ItemIndex of
      0:  fLocalArchiveProcessingSettings.Common.SelectedArchiveType := atSCS_frc;
      1:  fLocalArchiveProcessingSettings.Common.SelectedArchiveType := atZIP_frc;
    end;
    lblArchiveType.Caption := DART_ArchiveTypeStrings[fLocalArchiveProcessingSettings.Common.SelectedArchiveType];
    //ShowProperFrame;
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.lbleTargetChange(Sender: TObject);
begin
lbleTarget.ShowHint := Canvas.TextWidth(lbleTarget.Text) > (lbleTarget.Width - (2 * GetSystemMetrics(SM_CXEDGE)));
If lbleTarget.ShowHint then
  lbleTarget.Hint := lbleTarget.Text
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnAcceptClick(Sender: TObject);
var
  MsgStr: String;
begin
If lbleTarget.Text <> '' then
  begin
    fAccepted := True;
    Close;
  end
else
  begin
    If rbRebuild.Checked or rbConvert.Checked then MsgStr := 'No output file selected.'
      else If rbExtract.Checked then MsgStr := 'No folder for extraction selected.'
        else MsgStr := 'No output selected.';
    MessageDlg(MsgStr,mtInformation,[mbOK],0);
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnCloseClick(Sender: TObject);
begin
Close;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnBrowseTargetClick(Sender: TObject);
var
  TempStr:  String;
begin
case fLocalArchiveProcessingSettings.Common.RepairMethod of
  rmRebuild,
  rmConvert:  // rebuild or convert -> browse for file
    begin
      TempStr := DART_ExpandFileName(lbleTarget.Text);
      If DART_DirectoryExists(ExtractFileDir(TempStr)) then
        diaTargetSave.InitialDir := ExtractFileDir(TempStr);
      diaTargetSave.FileName := TempStr;
      If diaTargetSave.Execute then
        lbleTarget.Text := diaTargetSave.FileName;
    end;
  rmExtract:  // extract -> browse for directory
    begin
      TempStr := DART_ExpandFileName(IncludeTrailingPathDelimiter(lbleTarget.Text));
      If DART_DirectoryExists(ExtractFileDir(TempStr)) then
        TempStr := ExtractFileDir(TempStr)
      else If DART_DirectoryExists(ExtractFileDir(DART_ExpandFileName(TempStr + '..\'))) then
        TempStr := ExtractFileDir(DART_ExpandFileName(TempStr + '..\'))
      else
        TempStr := ExtractFileDir(RTLToStr(ParamStr(0)));
      {$IFDEF FPC}
        with TSelectDirectoryDialog.Create(Self) do
          begin
            Title := 'Select folder for archive extraction.';
            InitialDir := TempStr;
            If Execute then
              lbleTarget.Text := IncludeTrailingPathDelimiter(FileName);
          end;
      {$ELSE}
        If SelectDirectory('Select folder for archive extraction.','',TempStr) then
          lbleTarget.Text := IncludeTrailingPathDelimiter(TempStr);
      {$ENDIF}
    end;
end;
end;

end.
