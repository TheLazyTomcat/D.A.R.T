{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit PrcsSettingsForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  FilesManager;

type

  { TfPrcsSettingsForm }

  TfPrcsSettingsForm = class(TForm)
    diaSaveDialog: TSaveDialog;
    grbGeneral: TGroupBox;
    lblFile: TLabel;
    rbRebuild: TRadioButton;
    rbExtract: TRadioButton;
    vblGeneralHorSplit_File: TBevel;
    lbleData: TLabeledEdit;
    btnBrowse: TButton;
    bvlGeneralhorSplit: TBevel;
    cbIgnoreFileSignature: TCheckBox;
    cbAssumeCompressionMethods: TCheckBox;
    cbInMemoryProcessing: TCheckBox;
    cbIgnoreProcessingErrors: TCheckBox;
    cbLogIgnoredErrors: TCheckBox;
    grdEndOfCentralDirectory: TGroupBox;
    cbIgnoreEndOfCentralDirectory: TCheckBox;
    cbIgnoreDiskSplit: TCheckBox;
    cbIgnoreNumberOfEntries: TCheckBox;
    cbIgnoreCentralDirectoryOffset: TCheckBox;
    cbIgnoreComment: TCheckBox;
    bvlEOCDSplit: TBevel;
    cbLimitSearch: TCheckBox;
    grbCentralDirectoryHeaders: TGroupBox;
    cbCDIgnoreCentralDirectory: TCheckBox;
    cbCDIgnoreSignature: TCheckBox;
    cbCDIgnoreVersions: TCheckBox;
    cbCDClearEncryptionFlags: TCheckBox;
    cbCDIgnoreCompressionMethod: TCheckBox;
    cbCDIgnoreModTime: TCheckBox;
    cbCDIgnoreModDate: TCheckBox;
    cbCDIgnoreCRC32: TCheckBox;
    cbCDIgnoreSizes: TCheckBox;
    cbCDIgnoreInternalFileAttributes: TCheckBox;
    cbCDIgnoreExternalFileAttributes: TCheckBox;
    cbCDIgnoreLocalHeaderOffset: TCheckBox;
    cbCDIgnoreExtraField: TCheckBox;
    cbCDIgnoreFileComment: TCheckBox;
    grbLocalHeaders: TGroupBox;
    cbLHIgnoreLocalHeaders: TCheckBox;
    cbLHIgnoreSignature: TCheckBox;
    cbLHIgnoreVersions: TCheckBox;
    cbLHClearEncryptionFlags: TCheckBox;
    cbLHIgnoreCompressionMethod: TCheckBox;
    cbLHIgnoreModTime: TCheckBox;
    cbLHIgnoreModDate: TCheckBox;
    cbLHIgnoreCRC32: TCheckBox;
    cbLHIgnoreSizes: TCheckBox;
    cbLHIgnoreFileName: TCheckBox;
    cbLHIgnoreExtraField: TCheckBox;
    bvlLHSplit: TBevel;
    cbLHIgnoreDataDescriptor: TCheckBox;
    lblSettingDescription: TLabel;
    meSettingDescription: TMemo;
    btnDefault: TButton;    
    btnAccept: TButton;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbleDataChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    fFileInfo:              TFileListItem;
    fLoading:               Boolean;
    fAccepted:              Boolean;
    fSettingsDescriptions:  array[0..79] of String;
  protected
    procedure LoadSettingsDescriptions;
  public
    procedure SettingsToForm;
    procedure FormToSettings;
    procedure ShowProcessingSettings(var FileInfo: TFileListItem);
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
  Windows, StrUtils, Repairer,
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

//==============================================================================

procedure TfPrcsSettingsForm.SettingsToForm;
begin
case fFileInfo.ProcessingSettings.RepairMethod of
  rmRebuild:  rbRebuild.Checked := True;
  rmExtract:  rbExtract.Checked := True;
end;
lbleData.Text := fFileInfo.ProcessingSettings.RepairData;
cbIgnoreFileSignature.Checked := fFileInfo.ProcessingSettings.IgnoreFileSignature;
cbAssumeCompressionMethods.Checked := fFileInfo.ProcessingSettings.AssumeCompressionMethods;
cbInMemoryProcessing.Checked := fFileInfo.ProcessingSettings.InMemoryProcessing;
cbInMemoryProcessing.Enabled := fFileInfo.ProcessingSettings.OtherSettings.InMemoryProcessingAllowed;
cbIgnoreProcessingErrors.Checked := fFileInfo.ProcessingSettings.IgnoreProcessingErrors;
cbLogIgnoredErrors.Checked := fFileInfo.ProcessingSettings.LogIgnoredErrors;
//eocd
cbIgnoreEndOfCentralDirectory.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory;
cbIgnoreDiskSplit.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit;
cbIgnoreNumberOfEntries.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries;
cbIgnoreCentralDirectoryOffset.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset;
cbIgnoreComment.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreComment;
cbLimitSearch.Checked := fFileInfo.ProcessingSettings.EndOfCentralDirectory.LimitSearch;
//central directory
cbCDIgnoreCentralDirectory.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCentralDirectory;
cbCDIgnoreSignature.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreSignature;
cbCDIgnoreVersions.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreVersions;
cbCDClearEncryptionFlags.Checked := fFileInfo.ProcessingSettings.CentralDirectory.ClearEncryptionFlags;
cbCDIgnoreCompressionMethod.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCompressionMethod;
cbCDIgnoreModTime.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreModTime;
cbCDIgnoreModDate.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreModDate;
cbCDIgnoreCRC32.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCRC32;
cbCDIgnoreSizes.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreSizes;
cbCDIgnoreInternalFileAttributes.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes;
cbCDIgnoreExternalFileAttributes.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes;
cbCDIgnoreLocalHeaderOffset.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset;
cbCDIgnoreExtraField.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreExtraField;
cbCDIgnoreFileComment.Checked := fFileInfo.ProcessingSettings.CentralDirectory.IgnoreFileComment;
//local headers
cbLHIgnoreLocalHeaders.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreLocalHeaders;
cbLHIgnoreSignature.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreSignature;
cbLHIgnoreVersions.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreVersions;
cbLHClearEncryptionFlags.Checked := fFileInfo.ProcessingSettings.LocalHeader.ClearEncryptionFlags;
cbLHIgnoreCompressionMethod.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreCompressionMethod;
cbLHIgnoreModTime.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreModTime;
cbLHIgnoreModDate.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreModDate;
cbLHIgnoreCRC32.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreCRC32;
cbLHIgnoreSizes.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreSizes;
cbLHIgnoreFileName.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreFileName;
cbLHIgnoreExtraField.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreExtraField;
//data descriptor
cbLHIgnoreDataDescriptor.Checked := fFileInfo.ProcessingSettings.LocalHeader.IgnoreDataDescriptor;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FormToSettings;
begin
If rbRebuild.Checked then
  begin
    fFileInfo.ProcessingSettings.RepairMethod := rmRebuild;
  {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
    fFileInfo.ProcessingSettings.RepairData := ExpandFileNameUTF8(lbleData.Text);
  {$ELSE}
    fFileInfo.ProcessingSettings.RepairData := ExpandFileName(lbleData.Text);
  {$IFEND}
  end;
If rbExtract.Checked then
  begin
    fFileInfo.ProcessingSettings.RepairMethod := rmExtract;
  {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
    fFileInfo.ProcessingSettings.RepairData := IncludeTrailingPathDelimiter(ExpandFileNameUTF8(lbleData.Text));
  {$ELSE}
    fFileInfo.ProcessingSettings.RepairData := IncludeTrailingPathDelimiter(ExpandFileName(lbleData.Text));
  {$IFEND}
  end;
fFileInfo.ProcessingSettings.IgnoreFileSignature := cbIgnoreFileSignature.Checked;
fFileInfo.ProcessingSettings.AssumeCompressionMethods := cbAssumeCompressionMethods.Checked;
fFileInfo.ProcessingSettings.InMemoryProcessing := cbInMemoryProcessing.Checked;
fFileInfo.ProcessingSettings.IgnoreProcessingErrors := cbIgnoreProcessingErrors.Checked;
fFileInfo.ProcessingSettings.LogIgnoredErrors := cbLogIgnoredErrors.Checked;
//eocd
fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory := cbIgnoreEndOfCentralDirectory.Checked;
fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit := cbIgnoreDiskSplit.Checked;
fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries := cbIgnoreNumberOfEntries.Checked;
fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset := cbIgnoreCentralDirectoryOffset.Checked;
fFileInfo.ProcessingSettings.EndOfCentralDirectory.IgnoreComment := cbIgnoreComment.Checked;
fFileInfo.ProcessingSettings.EndOfCentralDirectory.LimitSearch := cbLimitSearch.Checked;
//central directory
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCentralDirectory := cbCDIgnoreCentralDirectory.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreSignature := cbCDIgnoreSignature.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreVersions := cbCDIgnoreVersions.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.ClearEncryptionFlags := cbCDClearEncryptionFlags.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCompressionMethod := cbCDIgnoreCompressionMethod.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreModTime := cbCDIgnoreModTime.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreModDate := cbCDIgnoreModDate.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreCRC32 := cbCDIgnoreCRC32.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreSizes := cbCDIgnoreSizes.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes := cbCDIgnoreInternalFileAttributes.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes := cbCDIgnoreExternalFileAttributes.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset := cbCDIgnoreLocalHeaderOffset.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreExtraField := cbCDIgnoreExtraField.Checked;
fFileInfo.ProcessingSettings.CentralDirectory.IgnoreFileComment := cbCDIgnoreFileComment.Checked;
//local headers
fFileInfo.ProcessingSettings.LocalHeader.IgnoreLocalHeaders := cbLHIgnoreLocalHeaders.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreSignature := cbLHIgnoreSignature.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreVersions := cbLHIgnoreVersions.Checked;
fFileInfo.ProcessingSettings.LocalHeader.ClearEncryptionFlags := cbLHClearEncryptionFlags.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreCompressionMethod := cbLHIgnoreCompressionMethod.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreModTime := cbLHIgnoreModTime.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreModDate := cbLHIgnoreModDate.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreCRC32 := cbLHIgnoreCRC32.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreSizes := cbLHIgnoreSizes.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreFileName := cbLHIgnoreFileName.Checked;
fFileInfo.ProcessingSettings.LocalHeader.IgnoreExtraField := cbLHIgnoreExtraField.Checked;
//data descriptor
fFileInfo.ProcessingSettings.LocalHeader.IgnoreDataDescriptor := cbLHIgnoreDataDescriptor.Checked;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.ShowProcessingSettings(var FileInfo: TFileListItem);
begin
fFileInfo := FileInfo;
lblFile.Caption := MinimizeName(fFileInfo.Path,Canvas,lblFile.Constraints.MaxWidth);
lblFile.ShowHint := not AnsiSameText(lblFile.Caption,fFileInfo.Path) or (Canvas.TextWidth(lblFile.Caption) > lblFile.Constraints.MaxWidth);
If lblFile.ShowHint then
  lblFile.Hint := FileInfo.Path;
fLoading := True;
try
  SettingsToForm;
finally
  fLoading := False;
end;
cbIgnoreProcessingErrors.OnClick(cbIgnoreProcessingErrors);
cbIgnoreEndOfCentralDirectory.OnClick(cbIgnoreEndOfCentralDirectory);
cbCDIgnoreCentralDirectory.OnClick(cbCDIgnoreCentralDirectory);
cbLHIgnoreLocalHeaders.OnClick(cbLHIgnoreLocalHeaders);
fAccepted := False;
ShowModal;
FormToSettings;
If fAccepted then
  FileInfo.ProcessingSettings := fFileInfo.ProcessingSettings;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.RepairMethodClick(Sender: TObject);
begin
If Sender is TRadioButton then
  begin
    btnBrowse.Tag := TRadioButton(Sender).Tag;
    case TRadioButton(Sender).Tag of
      2:  begin
            lbleData.EditLabel.Caption := 'Output file:';
            fFileInfo.ProcessingSettings.RepairData := ExtractFilePath(fFileInfo.Path) + 'repaired_' + fFileInfo.Name;
          end;
      3:  begin
            lbleData.EditLabel.Caption := 'Extract into:';
            fFileInfo.ProcessingSettings.RepairData := IncludeTrailingPathDelimiter(ChangeFileExt(fFileInfo.Path,''));
          end;
    end;
    lbleData.Text := fFileInfo.ProcessingSettings.RepairData;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.CheckBoxClick(Sender: TObject);
begin
If (Sender is TCheckBox) and not fLoading then
  begin
    case TCheckBox(Sender).Tag of
        9:  cbLogIgnoredErrors.Enabled := cbIgnoreProcessingErrors.Checked and cbIgnoreProcessingErrors.Enabled;
       20:  begin
              cbIgnoreDiskSplit.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreNumberOfEntries.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreCentralDirectoryOffset.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreComment.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbLimitSearch.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
            end;
       40:  begin
              cbCDIgnoreSignature.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreVersions.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDClearEncryptionFlags.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreCompressionMethod.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreModTime.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreModDate.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreCRC32.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreSizes.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreInternalFileAttributes.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreExternalFileAttributes.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreLocalHeaderOffset.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreExtraField.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreFileComment.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              If TCheckBox(Sender).Checked and cbLHIgnoreCompressionMethod.Checked then
                cbLHIgnoreSizes.Checked := False;
              If TCheckBox(Sender).Checked then
                begin
                  cbLHIgnoreLocalHeaders.Checked := False;
                  cbLHIgnoreFileName.Checked := False;
                end;
            end;
       44:  If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
              cbCDIgnoreSizes.Checked := False;
       48:  If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
              cbCDIgnoreCompressionMethod.Checked := False;
       51:  If TCheckBox(Sender).Checked then
              cbLHIgnoreLocalHeaders.Checked := False;
       60:  begin
              cbLHIgnoreSignature.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreVersions.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHClearEncryptionFlags.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreCompressionMethod.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreModTime.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreModDate.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreCRC32.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreSizes.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreFileName.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreExtraField.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              cbLHIgnoreDataDescriptor.Enabled := not cbLHIgnoreLocalHeaders.Checked;
              If TCheckBox(Sender).Checked then
                begin
                  cbCDIgnoreCentralDirectory.Checked := False;
                  cbCDIgnoreLocalHeaderOffset.Checked := False;
                  If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked then
                    cbCDIgnoreCompressionMethod.Checked := False;
                end;
            end;
       64:  If cbCDIgnoreCentralDirectory.Checked then
              begin
                If TCheckBox(Sender).Checked then
                  cbLHIgnoreSizes.Checked := False;
              end
            else
              If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked and TCheckBox(Sender).Checked then
                cbLHIgnoreSizes.Checked := False;
       68:  If cbCDIgnoreCentralDirectory.Checked then
              begin
                If TCheckBox(Sender).Checked then
                  cbLHIgnoreCompressionMethod.Checked := False;
              end
            else
              If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked and TCheckBox(Sender).Checked then
                cbLHIgnoreCompressionMethod.Checked := False;
       69:  If TCheckBox(Sender).Checked then
              cbCDIgnoreCentralDirectory.Checked := False;
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
    If Assigned(Control) and (Control is TCheckBox) then
      SettingsMouseMove(Control,Shift,X,Y);
  end;
end;

//==============================================================================

procedure TfPrcsSettingsForm.FormCreate(Sender: TObject);
begin
LoadSettingsDescriptions;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FormShow(Sender: TObject);
begin
meSettingDescription.Text := 'Move cursor over specific setting to see its description.';
btnClose.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.lbleDataChange(Sender: TObject);
begin
lbleData.ShowHint := Canvas.TextWidth(lbleData.Text) > (lbleData.Width - 6);
If lbleData.ShowHint then
  lbleData.Hint := lbleData.Text
end;
  
//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.btnBrowseClick(Sender: TObject);
var
  TempStr:  String;
begin
case btnBrowse.Tag of
  2:  begin
      {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
        TempStr := ExpandFileNameUTF8(lbleData.Text);
        If DirectoryExistsUTF8(ExtractFileDir(TempStr)) then
      {$ELSE}
        TempStr := ExpandFileName(lbleData.Text);
        If DirectoryExists(ExtractFileDir(TempStr)) then
      {$IFEND}
          diaSaveDialog.InitialDir := ExtractFileDir(TempStr);
        diaSaveDialog.FileName := TempStr;  
        If diaSaveDialog.Execute then
          lbleData.Text := diaSaveDialog.FileName;
      end;
  3:  begin
      {$IF Defined(FPC) and not Defined(Unicode) and (FPC_FULLVERSION < 20701)}
        TempStr := ExpandFileNameUTF8(IncludeTrailingPathDelimiter(lbleData.Text));
        If DirectoryExistsUTF8(ExtractFileDir(TempStr)) then TempStr := ExtractFileDir(TempStr)
          else If DirectoryExistsUTF8(ExtractFileDir(ExpandFileNameUTF8(TempStr + '..\'))) then
            TempStr := ExtractFileDir(ExpandFileNameUTF8(TempStr + '..\'))
          else
            TempStr := ExtractFileDir(SysToUTF8(ParamStr(0)));
      {$ELSE}
        TempStr := ExpandFileName(IncludeTrailingPathDelimiter(lbleData.Text));
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
              lbleData.Text := IncludeTrailingPathDelimiter(FileName);
          end;
      {$ELSE}
        If SelectDirectory('Select folder for archive extraction.','',TempStr) then
          lbleData.Text := IncludeTrailingPathDelimiter(TempStr);
      {$ENDIF}
      end;
end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.btnDefaultClick(Sender: TObject);
var
  TempMemoryAvailable:  Boolean;
begin
If MessageDlg('Are you sure you want to load default settings?',mtWarning,[mbYes,mbNo],0) = mrYes then
  begin
    TempMemoryAvailable := fFileInfo.ProcessingSettings.OtherSettings.InMemoryProcessingAllowed;
    fFileInfo.ProcessingSettings := DefaultProcessingSettings;
    fFileInfo.ProcessingSettings.RepairData := ExtractFilePath(fFileInfo.Path) + 'repaired_' + fFileInfo.Name;
    fFileInfo.ProcessingSettings.OtherSettings.InMemoryProcessingAllowed := TempMemoryAvailable;
    fLoading := True;
    try
      SettingsToForm;
    finally
      fLoading := False;
    end;
    cbIgnoreEndOfCentralDirectory.OnClick(cbIgnoreEndOfCentralDirectory);
    cbCDIgnoreCentralDirectory.OnClick(cbCDIgnoreCentralDirectory);
    cbLHIgnoreLocalHeaders.OnClick(cbLHIgnoreLocalHeaders);
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
If lbleData.Text <> '' then
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
