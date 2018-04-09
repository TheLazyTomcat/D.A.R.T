{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ProcSettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  DART_ProcessingSettings;

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
    lblArchiveFileCpt: TLabel;
    lblArchiveFile: TLabel;
    lblArchiveTypeCpt: TLabel;
    lblArchiveType: TLabel;
    cbForceArchiveType: TCheckBox;
    cmbForcedArchiveType: TComboBox;
    bvlHorSplitFile: TBevel;
    rbRebuild: TRadioButton;
    rbExtract: TRadioButton;
    rbConvert: TRadioButton;
    lblConvertTo: TLabel;
    cmbConvertTo: TComboBox;
    lbleTarget: TLabeledEdit;
    btnBrowseTarget: TButton;
    bvlHorSplitTarget: TBevel;
    cbIgnoreArchiveSignature: TCheckBox;
    cbInMemoryProcessing: TCheckBox;
    cbIgnoreErroneousEntries: TCheckBox;
    gbArchiveSettings: TGroupBox;
    scbArchiveSettings: TScrollBox;    
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
    procedure btnBrowseTargetClick(Sender: TObject);
    procedure btnSaveSettingsClick(Sender: TObject);
    procedure btnLoadSettingsClick(Sender: TObject);
    procedure btnDefaultSettingsClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    fArchiveProcessingSettings:   TDARTArchiveProcessingSettings;
    fActiveArchiveSettingsFrame:  TFrame;
    fLoading:                     Boolean;
    fAccepted:                    Boolean;
    fOptionDescriptions:          array of array of String;
  protected
    procedure LoadOptionsDescription;
    procedure FrameOptionDescriptionHandler(Sender: TObject; DescriptionTag: Integer);
    procedure SettingsToForm;
    procedure FormToSettings;
    procedure ShowArchiveSettingsFrame(ForceChange: Boolean = False);
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
  DART_Auxiliary, DART_ProcessingManager,
  ProcSettingsFrame_ZIP, ProcSettingsFrame_SCS;

procedure TfProcSettingsForm.LoadOptionsDescription;
begin
SetLength(fOptionDescriptions,0,0);
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.FrameOptionDescriptionHandler(Sender: TObject; DescriptionTag: Integer);
var
  GroupIdx: Integer;
  DescrIdx:  Integer;
begin
If meOptionDecription.Tag <> DescriptionTag then
  begin
    meOptionDecription.Tag := DescriptionTag;
    GroupIdx := DescriptionTag div 100;
    If (GroupIdx >= Low(fOptionDescriptions)) and (GroupIdx <= High(fOptionDescriptions)) then
      begin
        DescrIdx := DescriptionTag mod 100;
        If (DescrIdx >= Low(fOptionDescriptions[GroupIdx])) and (DescrIdx <= High(fOptionDescriptions[GroupIdx])) then
          meOptionDecription.Text := fOptionDescriptions[GroupIdx,DescrIdx]
        else
          meOptionDecription.Text := 'unknown #' + IntToStr(DescriptionTag);
      end
    else meOptionDecription.Text := 'unknown #' + IntToStr(DescriptionTag);
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.SettingsToForm;
begin
lblArchiveFile.Caption := MinimizeName(fArchiveProcessingSettings.Common.ArchivePath,lblArchiveFile.Canvas,lblArchiveFile.Constraints.MaxWidth);
lblArchiveFile.ShowHint := not AnsiSameText(lblArchiveFile.Caption,fArchiveProcessingSettings.Common.ArchivePath) or
                           (lblArchiveFile.Canvas.TextWidth(lblArchiveFile.Caption) > lblArchiveFile.Constraints.MaxWidth);
If lblArchiveFile.ShowHint then
  lblArchiveFile.Hint := fArchiveProcessingSettings.Common.ArchivePath;
lblArchiveType.Caption := DART_ArchiveTypeStrings[fArchiveProcessingSettings.Common.SelectedArchiveType];
cbForceArchiveType.Checked := fArchiveProcessingSettings.Common.SelectedArchiveType in [atSCS_frc,atZIP_frc];
If cbForceArchiveType.Checked then
  case fArchiveProcessingSettings.Common.SelectedArchiveType of
    atSCS_frc:  cmbForcedArchiveType.ItemIndex := 0;
    atZIP_frc:  cmbForcedArchiveType.ItemIndex := 1;
  end
else
  begin
    If cmbForcedArchiveType.ItemIndex < 0 then
      cmbForcedArchiveType.ItemIndex := 0;
  end;
case fArchiveProcessingSettings.Common.RepairMethod of
  rmRebuild:  rbRebuild.Checked := True;
  rmExtract:  rbExtract.Checked := True;
  rmConvert:  rbConvert.Checked := True;
end;
lblConvertTo.Enabled := fArchiveProcessingSettings.Common.RepairMethod = rmConvert;
cmbConvertTo.Enabled := fArchiveProcessingSettings.Common.RepairMethod = rmConvert;
case fArchiveProcessingSettings.Common.ConvertTo of
  katSCS: cmbConvertTo.ItemIndex := 0;
  katZIP: cmbConvertTo.ItemIndex := 1;
else
  case fArchiveProcessingSettings.Common.SelectedArchiveType of
    atSCS_sig,atSCS_frc:
      cmbConvertTo.ItemIndex := 1;
    atZIP_sig,atZIP_frc,atZIP_dft:
      cmbConvertTo.ItemIndex := 0;
  end
end;
lbleTarget.Text := fArchiveProcessingSettings.Common.TargetPath;
cbIgnoreArchiveSignature.Checked := fArchiveProcessingSettings.Common.IgnoreArchiveSignature;
cbInMemoryProcessing.Checked := fArchiveProcessingSettings.Common.InMemoryProcessing;
cbInMemoryProcessing.Enabled := fArchiveProcessingSettings.Auxiliary.InMemoryProcessingAllowed;
cbIgnoreErroneousEntries.Checked := fArchiveProcessingSettings.Common.IgnoreErroneousEntries;
If Assigned(fActiveArchiveSettingsFrame) then
  begin
    If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_ZIP then
      TfrmProcSettingsFrame_ZIP(fActiveArchiveSettingsFrame).ShowProcessingSettings(fArchiveProcessingSettings);
    If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_SCS then
      TfrmProcSettingsFrame_SCS(fActiveArchiveSettingsFrame).ShowProcessingSettings(fArchiveProcessingSettings);
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.FormToSettings;
begin
// SelectedArchiveType and RepairMethod are selected in-place
case cmbConvertTo.ItemIndex of
  0:  fArchiveProcessingSettings.Common.ConvertTo := katSCS;
  1:  fArchiveProcessingSettings.Common.ConvertTo := katZIP;
else
  fArchiveProcessingSettings.Common.ConvertTo := katUnknown;
end;
case fArchiveProcessingSettings.Common.RepairMethod of
  rmRebuild,
  rmConvert:  fArchiveProcessingSettings.Common.TargetPath := DART_ExpandFileName(lbleTarget.Text);
  rmExtract:  fArchiveProcessingSettings.Common.TargetPath := IncludeTrailingPathDelimiter(DART_ExpandFileName(lbleTarget.Text));
end;
fArchiveProcessingSettings.Common.IgnoreArchiveSignature := cbIgnoreArchiveSignature.Checked;
fArchiveProcessingSettings.Common.InMemoryProcessing := cbInMemoryProcessing.Checked;
fArchiveProcessingSettings.Common.IgnoreErroneousEntries := cbIgnoreErroneousEntries.Checked;
If Assigned(fActiveArchiveSettingsFrame) then
  begin
    If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_ZIP then
      TfrmProcSettingsFrame_ZIP(fActiveArchiveSettingsFrame).RetrieveProcessingSettings(fArchiveProcessingSettings);
    If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_SCS then
      TfrmProcSettingsFrame_SCS(fActiveArchiveSettingsFrame).RetrieveProcessingSettings(fArchiveProcessingSettings);
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.ShowArchiveSettingsFrame(ForceChange: Boolean = False);
begin
If Assigned(fActiveArchiveSettingsFrame) then
  begin
    If not ForceChange then
      case fArchiveProcessingSettings.Common.SelectedArchiveType of
        atSCS_sig,atSCS_frc:
          If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_SCS then Exit;
        atZIP_sig,atZIP_frc,atZIP_dft:
          If fActiveArchiveSettingsFrame is TfrmProcSettingsFrame_ZIP then Exit;
      end;
    FormToSettings;
    fActiveArchiveSettingsFrame.Free;
  end;
case fArchiveProcessingSettings.Common.SelectedArchiveType of
  atSCS_sig,atSCS_frc:
    begin
      fActiveArchiveSettingsFrame := TfrmProcSettingsFrame_SCS.Create(Self);
      with TfrmProcSettingsFrame_SCS(fActiveArchiveSettingsFrame) do
        begin
          OnOptionDescription := FrameOptionDescriptionHandler;
          Initialize;
          ShowProcessingSettings(fArchiveProcessingSettings);
        end;
    end;
  atZIP_sig,atZIP_frc,atZIP_dft:
    begin
      fActiveArchiveSettingsFrame := TfrmProcSettingsFrame_ZIP.Create(Self);
      with TfrmProcSettingsFrame_ZIP(fActiveArchiveSettingsFrame) do
        begin
          OnOptionDescription := FrameOptionDescriptionHandler;
          Initialize;
          ShowProcessingSettings(fArchiveProcessingSettings);
        end;
    end;
else
  raise Exception.Create('Unknown archive type.');
end;
fActiveArchiveSettingsFrame.Left := 0;
fActiveArchiveSettingsFrame.Top := 0;
fActiveArchiveSettingsFrame.Parent := scbArchiveSettings;
SettingsToForm;
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
              0:  fArchiveProcessingSettings.Common.SelectedArchiveType := atSCS_frc;
              1:  fArchiveProcessingSettings.Common.SelectedArchiveType := atZIP_frc;
            end
          else fArchiveProcessingSettings.Common.SelectedArchiveType := fArchiveProcessingSettings.Common.OriginalArchiveType;
          lblArchiveType.Caption := DART_ArchiveTypeStrings[fArchiveProcessingSettings.Common.SelectedArchiveType];
          ShowArchiveSettingsFrame;
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
            fArchiveProcessingSettings.Common.RepairMethod := rmRebuild;
            lbleTarget.EditLabel.Caption := 'Output file:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fArchiveProcessingSettings.Common);
          end;
      6:  begin   // rbExtract
            fArchiveProcessingSettings.Common.RepairMethod := rmExtract;
            lbleTarget.EditLabel.Caption := 'Extract into:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fArchiveProcessingSettings.Common);
          end;
      7:  begin   // rbConvert
            fArchiveProcessingSettings.Common.RepairMethod := rmConvert;
            lbleTarget.EditLabel.Caption := 'Output file:';
            TDARTProcessingManager.SetTargetPathFromSourcePath(fArchiveProcessingSettings.Common);
          end;
    end;
    lbleTarget.Text := fArchiveProcessingSettings.Common.TargetPath;
    lblConvertTo.Enabled := fArchiveProcessingSettings.Common.RepairMethod = rmConvert;
    cmbConvertTo.Enabled := fArchiveProcessingSettings.Common.RepairMethod = rmConvert;
  end;
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If Sender is TControl then
  FrameOptionDescriptionHandler(Self,TControl(Sender).Tag);
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
fArchiveProcessingSettings := ArchiveProcessingSettings;
fLoading := True;
try
  SettingsToForm;
finally
  fLoading := False;
end;
cbForceArchiveType.OnClick(cbForceArchiveType);
ShowArchiveSettingsFrame(True);
fAccepted := False;
ShowModal;
If fAccepted then
  begin
    FormToSettings;
    ArchiveProcessingSettings := fArchiveProcessingSettings;
  end;
end;

//==============================================================================

procedure TfProcSettingsForm.FormCreate(Sender: TObject);
var
  i:  TDARTKnownArchiveTypes;
begin
fActiveArchiveSettingsFrame := nil;
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
      0:  fArchiveProcessingSettings.Common.SelectedArchiveType := atSCS_frc;
      1:  fArchiveProcessingSettings.Common.SelectedArchiveType := atZIP_frc;
    end;
    lblArchiveType.Caption := DART_ArchiveTypeStrings[fArchiveProcessingSettings.Common.SelectedArchiveType];
    ShowArchiveSettingsFrame;
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

procedure TfProcSettingsForm.btnBrowseTargetClick(Sender: TObject);
var
  TempStr:  String;
begin
case fArchiveProcessingSettings.Common.RepairMethod of
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

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnSaveSettingsClick(Sender: TObject);
begin
//
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnLoadSettingsClick(Sender: TObject);
begin
//
end;

//------------------------------------------------------------------------------

procedure TfProcSettingsForm.btnDefaultSettingsClick(Sender: TObject);
begin
//
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

end.
