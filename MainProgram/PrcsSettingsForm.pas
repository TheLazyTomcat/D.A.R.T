{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit PrcsSettingsForm;

{$INCLUDE 'Source\DART_defs.inc'}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,ExtCtrls,
  ProcSettingsZIPFrame, ProcSettingsSCSFrame, DART_ProcessingSettings;

type
{$IFNDEF FPC}
  // combobox in delphi does not have public OnMouseMove event
  TComboBox = class(StdCtrls.TComboBox)
  published
    property OnMouseMove;
  end;
{$ENDIF}

  { TfPrcsSettingsForm }

  TfPrcsSettingsForm = class(TForm)
    diaSaveDialog: TSaveDialog;
    grbCommonSettings: TGroupBox;
    lblFileCpt: TLabel;
    lblFile: TLabel;
    lblFileTypeCpt: TLabel;
    lblFileType: TLabel;
    cbForceFileType: TCheckBox;
    cmbForcedFileType: TComboBox;
    vblGeneralHorSplit_File: TBevel;
    rbRebuild: TRadioButton;
    rbExtract: TRadioButton;
    lbleTarget: TLabeledEdit;
    btnBrowse: TButton;
    bvlGeneralhorSplit: TBevel;
    cbIgnoreFileSignature: TCheckBox;
    cbInMemoryProcessing: TCheckBox;
    cbIgnoreErroneousEntries: TCheckBox;
    grbArchiveSettings: TGroupBox;
    frmProcSettingsZIP: TfrmProcSettingsZIP;
    frmProcSettingsSCS: TfrmProcSettingsSCS;       
    lblSettingDescription: TLabel;
    meSettingDescription: TMemo;
    btnDefault: TButton;
    btnAccept: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cmbForcedFileTypeChange(Sender: TObject);    
    procedure lbleTargetChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    fFileProcessingSettings:  TFileProcessingSettings;
    fSettingsDescriptions:    array[0..148] of String;
    fLoading:                 Boolean;
    fAccepted:                Boolean;    
  protected
    procedure LoadSettingsDescriptions;
    procedure FrameSettingsHintHandler(Sender: TObject; HintTag: Integer);
    procedure ShowProperFrame;
  public
    procedure SettingsToForm;
    procedure FormToSettings;
    procedure ShowProcessingSettings(var FileProcessingSettings: TFileProcessingSettings);
  published
    procedure RepairMethodClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
    procedure SettingsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  end;

var
  fPrcsSettingsForm: TfPrcsSettingsForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}  

uses
  Windows, StrUtils, //Repairer,
{$WARN UNIT_PLATFORM OFF}
  FileCtrl
{$WARN UNIT_PLATFORM ON}
{$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
  , LazFileUtils, LazUTF8
{$IFEND};


{$R '.\Resources\SettDescr.res'}

//==============================================================================

procedure TfPrcsSettingsForm.LoadSettingsDescriptions;
var
  ResourceStream: TResourceStream;
  TempStrList:    TStringList;
  i:              Integer;
  TagIdx:         Integer;
  NextTagIdx:     Integer;
  Tag:            Integer;
  TempStr:        String;

  Function FindNextTag(Start: Integer): Integer;
  var
    ii: Integer;
  begin
    Result := TempStrList.Count;
    For ii := Start to Pred(TempStrList.Count) do
      If AnsiStartsText('#',TempStrList[ii]) then
        begin
          Result := ii;
          Break;
        end;
  end;

begin
For i := Low(fSettingsDescriptions) to High(fSettingsDescriptions) do
  fSettingsDescriptions[i] := Format('no description (%d)',[i]);
TempStrList := TStringList.Create;
try
  ResourceStream := TResourceStream.Create(hInstance,'SettDescr',RT_RCDATA);
  try
    TempStrList.LoadFromStream(ResourceStream);
  finally
    ResourceStream.Free;
  end;
  // load individual descriptions
  TagIdx := FindNextTag(0);
  while TagIdx < TempStrList.Count do
    begin
      NextTagIdx := FindNextTag(TagIdx + 1);    
      Tag := StrToIntDef(Copy(TempStrList[TagIdx],2,Length(TempStrList[TagIdx])),-1);
      If (Tag >= Low(fSettingsDescriptions)) and (Tag <= High(fSettingsDescriptions)) then
        begin
          TempStr := '';
          For i := Succ(TagIdx) to Pred(NextTagIdx) do
            If i > Succ(TagIdx) then
              TempStr := TempStr + sLineBreak + TempStrList[i]
            else
              TempStr := TempStrList[i];
          fSettingsDescriptions[Tag] := TempStr;  
        end;
      TagIdx := NextTagIdx;  
    end;
finally
  TempStrList.Free;
end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FrameSettingsHintHandler(Sender: TObject; HintTag: Integer);
begin
If meSettingDescription.Tag <> HintTag then
  begin
    meSettingDescription.Tag := HintTag;
    If (HintTag >= Low(fSettingsDescriptions)) and (HintTag <= High(fSettingsDescriptions)) then
      meSettingDescription.Text := fSettingsDescriptions[HintTag]
    else
      meSettingDescription.Text := 'unknown #' + IntToStr(HintTag);
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.ShowProperFrame;
begin
frmProcSettingsZIP.Visible := fFileProcessingSettings.Common.FileType in [atZIP_sig,atZIP_frc,atZIP_dft];
frmProcSettingsSCS.Visible := fFileProcessingSettings.Common.FileType in [atSCS_sig,atSCS_frc];
case fFileProcessingSettings.Common.FileType of
  atZIP_sig,atZIP_frc,atZIP_dft: grbArchiveSettings.Caption := 'ZIP archive settings';
  atSCS_sig,atSCS_frc:           grbArchiveSettings.Caption := 'SCS# archive settings';
else
  grbArchiveSettings.Caption := 'Archive settings';
end;
end;

//==============================================================================

procedure TfPrcsSettingsForm.SettingsToForm;
begin
lblFile.Caption := MinimizeName(fFileProcessingSettings.Common.FilePath,lblFile.Canvas,lblFile.Constraints.MaxWidth);
lblFile.ShowHint := not AnsiSameText(lblFile.Caption,fFileProcessingSettings.Common.FilePath) or
                   (Canvas.TextWidth(lblFile.Caption) > lblFile.Constraints.MaxWidth);
If lblFile.ShowHint then
  lblFile.Hint := fFileProcessingSettings.Common.FilePath;
lblFileType.Caption := FileTypeStrArr[fFileProcessingSettings.Common.FileType];
cbForceFileType.Checked := fFileProcessingSettings.Common.FileType in [atSCS_frc,atZIP_frc];
If cbForceFileType.Checked then
  case fFileProcessingSettings.Common.FileType of
    atZIP_frc:  cmbForcedFileType.ItemIndex := 0;
    atSCS_frc:  cmbForcedFileType.ItemIndex := 1;
  end
else cmbForcedFileType.ItemIndex := 0;
case fFileProcessingSettings.Common.RepairMethod of
  rmRebuild:  rbRebuild.Checked := True;
  rmExtract:  rbExtract.Checked := True;
end;
lbleTarget.Text := fFileProcessingSettings.Common.TargetPath;
cbIgnoreFileSignature.Checked := fFileProcessingSettings.Common.IgnoreFileSignature;
cbInMemoryProcessing.Checked := fFileProcessingSettings.Common.InMemoryProcessing;
cbInMemoryProcessing.Enabled := fFileProcessingSettings.Other.InMemoryProcessingAllowed;
cbIgnoreErroneousEntries.Checked := fFileProcessingSettings.Common.IgnoreErroneousEntries;
frmProcSettingsZIP.ShowProcessingSettings(fFileProcessingSettings.ZIPSettings);
frmProcSettingsSCS.ShowProcessingSettings(fFileProcessingSettings.SCSSettings);
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FormToSettings;
begin
If rbRebuild.Checked then
  begin
    fFileProcessingSettings.Common.RepairMethod := rmRebuild;
  {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
    fFileProcessingSettings.Common.TargetPath := ExpandFileNameUTF8(lbleTarget.Text);
  {$ELSE}
    fFileProcessingSettings.Common.TargetPath := ExpandFileName(lbleTarget.Text);
  {$IFEND}
  end;
If rbExtract.Checked then
  begin
    fFileProcessingSettings.Common.RepairMethod := rmExtract;
  {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
    fFileProcessingSettings.Common.TargetPath := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(lbleTarget.Text));
  {$ELSE}
    fFileProcessingSettings.Common.TargetPath := IncludeTrailingPathDelimiter(ExpandFileName(lbleTarget.Text));
  {$IFEND}
  end;
fFileProcessingSettings.Common.IgnoreFileSignature := cbIgnoreFileSignature.Checked;
fFileProcessingSettings.Common.InMemoryProcessing := cbInMemoryProcessing.Checked;
fFileProcessingSettings.Common.IgnoreErroneousEntries := cbIgnoreErroneousEntries.Checked;
fFileProcessingSettings.ZIPSettings := frmProcSettingsZIP.RetrieveProcessingSettings;
fFileProcessingSettings.SCSSettings := frmProcSettingsSCS.RetrieveProcessingSettings;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.ShowProcessingSettings(var FileProcessingSettings: TFileProcessingSettings);
begin
fFileProcessingSettings := FileProcessingSettings;
fLoading := True;
try
  SettingsToForm;
finally
  fLoading := False;
end;
cbForceFileType.OnClick(cbForceFileType);
fAccepted := False;
ShowProperFrame;
ShowModal;
If fAccepted then
  begin
    FormToSettings;
    FileProcessingSettings := fFileProcessingSettings;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.RepairMethodClick(Sender: TObject);
begin
If Sender is TRadioButton then
  begin
    btnBrowse.Tag := TRadioButton(Sender).Tag;
    case TRadioButton(Sender).Tag of
      5:  begin   // rbRebuild
            lbleTarget.EditLabel.Caption := 'Output file:';
            fFileProcessingSettings.Common.TargetPath := ExtractFilePath(fFileProcessingSettings.Common.FilePath) + 'repaired_' +
                                                         ExtractFileName(fFileProcessingSettings.Common.FilePath);
          end;
      6:  begin   // rbExtract
            lbleTarget.EditLabel.Caption := 'Extract into:';
            fFileProcessingSettings.Common.TargetPath := IncludeTrailingPathDelimiter(ChangeFileExt(fFileProcessingSettings.Common.FilePath,''));
          end;
    end;
    lbleTarget.Text := fFileProcessingSettings.Common.TargetPath;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.CheckBoxClick(Sender: TObject);
begin
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    3:  begin   // cbForceFileType
          cmbForcedFileType.Enabled := TCheckBox(Sender).Checked;
          If TCheckBox(Sender).Checked then
            case cmbForcedFileType.ItemIndex of
              0:  fFileProcessingSettings.Common.FileType := atZIP_frc;
              1:  fFileProcessingSettings.Common.FileType := atSCS_frc;
            end
          else fFileProcessingSettings.Common.FileType := fFileProcessingSettings.Common.OriginalFileType;
          lblFileType.Caption := FileTypeStrArr[fFileProcessingSettings.Common.FileType];
          ShowProperFrame;
        end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.SettingsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If Sender is TControl then
  If meSettingDescription.Tag <> TControl(Sender).Tag then
    begin
      meSettingDescription.Tag := TControl(Sender).Tag;
      If (TControl(Sender).Tag >= Low(fSettingsDescriptions)) and
         (TControl(Sender).Tag <= High(fSettingsDescriptions)) then
        meSettingDescription.Text := fSettingsDescriptions[TControl(Sender).Tag]
      else
        meSettingDescription.Text := 'unknown #' + IntToStr(TControl(Sender).Tag);
    end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Control:  TControl;

{$IFDEF FPC}
  Function Point(X,Y: Integer): TPoint;
  begin
    Result.x := X;
    Result.y := Y;
  end;
{$ENDIF}

begin
If Sender is TGroupBox then
  begin
    Control := TGroupBox(Sender).ControlAtPos(Point(X,Y),True,True);
    If Assigned(Control) and ((Control is TCheckBox) or (Control is TComboBox)) then
      SettingsMouseMove(Control,Shift,X,Y);
  end;
end;

//==============================================================================

procedure TfPrcsSettingsForm.FormCreate(Sender: TObject);
begin
LoadSettingsDescriptions;
frmProcSettingsZIP.OnSettingsHint := FrameSettingsHintHandler;
frmProcSettingsSCS.OnSettingsHint := FrameSettingsHintHandler;
cmbForcedFileType.OnMouseMove := SettingsMouseMove;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FormShow(Sender: TObject);
begin
meSettingDescription.Text := 'Move cursor over specific setting to see its description.';
btnClose.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.cmbForcedFileTypeChange(Sender: TObject);
begin
If cbForceFileType.Checked then
  begin
    case cmbForcedFileType.ItemIndex of
      0:  fFileProcessingSettings.Common.FileType := atZIP_frc;
      1:  fFileProcessingSettings.Common.FileType := atSCS_frc;
    end;
    lblFileType.Caption := FileTypeStrArr[fFileProcessingSettings.Common.FileType];
    ShowProperFrame;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.lbleTargetChange(Sender: TObject);
begin
lbleTarget.ShowHint := Canvas.TextWidth(lbleTarget.Text) > (lbleTarget.Width - (2 * GetSystemMetrics(SM_CXEDGE)));
If lbleTarget.ShowHint then
  lbleTarget.Hint := lbleTarget.Text
end;
  
//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.btnBrowseClick(Sender: TObject);
var
  TempStr:  String;
begin
case btnBrowse.Tag of
  5:  begin   // rebuild -> browse for file
      {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
        TempStr := ExpandFileNameUTF8(lbleTarget.Text);
        If DirectoryExistsUTF8(ExtractFileDir(TempStr)) then
      {$ELSE}
        TempStr := ExpandFileName(lbleTarget.Text);
        If DirectoryExists(ExtractFileDir(TempStr)) then
      {$IFEND}
          diaSaveDialog.InitialDir := ExtractFileDir(TempStr);
        diaSaveDialog.FileName := TempStr;
        If diaSaveDialog.Execute then
          lbleTarget.Text := diaSaveDialog.FileName;
      end;
  6:  begin   // extract -> browse for directory
      {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
        TempStr := ExpandFileNameUTF8(IncludeTrailingPathDelimiter(lbleTarget.Text));
        If DirectoryExistsUTF8(ExtractFileDir(TempStr)) then TempStr := ExtractFileDir(TempStr)
          else If DirectoryExistsUTF8(ExtractFileDir(ExpandFileNameUTF8(TempStr + '..\'))) then
            TempStr := ExtractFileDir(ExpandFileNameUTF8(TempStr + '..\'))
          else
            TempStr := ExtractFileDir(SysToUTF8(ParamStr(0)));
      {$ELSE}
        TempStr := ExpandFileName(IncludeTrailingPathDelimiter(lbleTarget.Text));
        If DirectoryExists(ExtractFileDir(TempStr)) then TempStr := ExtractFileDir(TempStr)
          else If DirectoryExists(ExtractFileDir(ExpandFileName(TempStr + '..\'))) then
            TempStr := ExtractFileDir(ExpandFileName(TempStr + '..\'))
          else
            TempStr := ExtractFileDir(ParamStr(0));
      {$IFEND}
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

procedure TfPrcsSettingsForm.btnDefaultClick(Sender: TObject);
var
  OldProcessingSettings:  TFileProcessingSettings;
begin
If MessageDlg('Are you sure you want to load default settings?',mtWarning,[mbYes,mbNo],0) = mrYes then
  begin
    OldProcessingSettings := fFileProcessingSettings;
    fFileProcessingSettings := DefaultFileProcessingSettings;
    fFileProcessingSettings.Common.FilePath := OldProcessingSettings.Common.FilePath;
    fFileProcessingSettings.Common.OriginalFileType := OldProcessingSettings.Common.OriginalFileType;
    fFileProcessingSettings.Common.FileType := OldProcessingSettings.Common.FileType;
    fFileProcessingSettings.Common.TargetPath := ExtractFilePath(fFileProcessingSettings.Common.FilePath) + 'repaired_' +
                                                 ExtractFileName(fFileProcessingSettings.Common.FilePath);
    fFileProcessingSettings.Other.InMemoryProcessingAllowed := OldProcessingSettings.Other.InMemoryProcessingAllowed;
    fLoading := True;
    try
      SettingsToForm;
    finally
      fLoading := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.btnCloseClick(Sender: TObject);
begin
Close;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.btnAcceptClick(Sender: TObject);
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
    If rbRebuild.Checked then MsgStr := 'No output file selected.'
      else If rbExtract.Checked then MsgStr := 'No folder for extraction selected.'
        else MsgStr := 'No output selected.';
    MessageDlg(MsgStr,mtInformation,[mbOK],0);
  end;
end;

end.
