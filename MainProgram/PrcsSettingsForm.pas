{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit PrcsSettingsForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, 
  Repairer;

type
  TfPrcsSettingsForm = class(TForm)
    diaSaveDialog: TSaveDialog;
    grbGeneral: TGroupBox;
    lblFile_l: TLabel;
    lblFile: TLabel;
    rbRebuild: TRadioButton;
    rbExtract: TRadioButton;
    vblGeneralHorSplit_File: TBevel;
    lbleData: TLabeledEdit;
    btnBrowse: TButton;
    bvlGeneralhorSplit: TBevel;
    cbIgnoreFileSignature: TCheckBox;
    cbAssumeCompressionMethods: TCheckBox;
    grdEndOfCentralDirectory: TGroupBox;
    cbIgnoreEndOfCentralDirectory: TCheckBox;
    cbIgnoreDiskSplit: TCheckBox;
    cbIgnoreNumberOfEntries: TCheckBox;
    cbIgnoreCentralDirectoryOffset: TCheckBox;
    cbIgnoreComment: TCheckBox;
    grbCentralDirectoryHeaders: TGroupBox;
    cbCDIgnoreCentralDirectory: TCheckBox;
    cbCDIgnoreSignature: TCheckBox;
    cbCDIgnoreVersions: TCheckBox;
    cbCDIgnoreFlags: TCheckBox;
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
    cbLHIgnoreSignature: TCheckBox;
    cbLHIgnoreVersions: TCheckBox;
    cbLHIgnoreFlags: TCheckBox;
    cbLHIgnoreCompressionMethod: TCheckBox;
    cbLHIgnoreModTime: TCheckBox;
    cbLHIgnoreModDate: TCheckBox;
    cbLHIgnoreCRC32: TCheckBox;
    cbLHIgnoreSizes: TCheckBox;
    cbLHIgnoreExtraField: TCheckBox;
    btnAccept: TButton;
    btnClose: TButton;
    procedure FormShow(Sender: TObject);
    procedure lbleDataChange(Sender: TObject);
    procedure btnBrowseClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnAcceptClick(Sender: TObject);
  private
    fFilePath:            String;
    fProcessingSettings:  TProcessingSettings;
    fSelecting:           Boolean;    
    fAccepted:            Boolean;
  public
    procedure SettingsToForm;
    procedure FormToSettings;
    procedure ShowProcessingSettings(const FilePath: String; var ProcessingSettings: TProcessingSettings);
  published  
    procedure RepairMethodClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);
  end;

var
  fPrcsSettingsForm: TfPrcsSettingsForm;

implementation

{$R *.dfm}

uses
{$WARN UNIT_PLATFORM OFF}
  FileCtrl;
{$WARN UNIT_PLATFORM ON}

procedure TfPrcsSettingsForm.SettingsToForm;
begin
case fProcessingSettings.RepairMethod of
  rmRebuild:  rbRebuild.Checked := True;
  rmExtract:  rbExtract.Checked := True;
end;
lbleData.Text := fProcessingSettings.RepairData;
cbIgnoreFileSignature.Checked := fProcessingSettings.IgnoreFileSignature;
cbAssumeCompressionMethods.Checked := fProcessingSettings.AssumeCompressionMethods;
//eocd
cbIgnoreEndOfCentralDirectory.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory;
cbIgnoreDiskSplit.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit;
cbIgnoreNumberOfEntries.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries;
cbIgnoreCentralDirectoryOffset.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset;
cbIgnoreComment.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreComment;
//central directory
cbCDIgnoreCentralDirectory.Checked := fProcessingSettings.CentralDirectory.IgnoreCentralDirectory;
cbCDIgnoreSignature.Checked := fProcessingSettings.CentralDirectory.IgnoreSignature;
cbCDIgnoreVersions.Checked := fProcessingSettings.CentralDirectory.IgnoreVersions;
cbCDIgnoreFlags.Checked := fProcessingSettings.CentralDirectory.IgnoreFlags;
cbCDIgnoreCompressionMethod.Checked := fProcessingSettings.CentralDirectory.IgnoreCompressionMethod;
cbCDIgnoreModTime.Checked := fProcessingSettings.CentralDirectory.IgnoreModTime;
cbCDIgnoreModDate.Checked := fProcessingSettings.CentralDirectory.IgnoreModDate;
cbCDIgnoreCRC32.Checked := fProcessingSettings.CentralDirectory.IgnoreCRC32;
cbCDIgnoreSizes.Checked := fProcessingSettings.CentralDirectory.IgnoreSizes;
cbCDIgnoreInternalFileAttributes.Checked := fProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes;
cbCDIgnoreExternalFileAttributes.Checked := fProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes;
cbCDIgnoreLocalHeaderOffset.Checked := fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset;
cbCDIgnoreExtraField.Checked := fProcessingSettings.CentralDirectory.IgnoreExtraField;
cbCDIgnoreFileComment.Checked := fProcessingSettings.CentralDirectory.IgnoreFileComment;
//local headers
cbLHIgnoreSignature.Checked := fProcessingSettings.LocalHeader.IgnoreSignature;
cbLHIgnoreVersions.Checked := fProcessingSettings.LocalHeader.IgnoreVersions;
cbLHIgnoreFlags.Checked := fProcessingSettings.LocalHeader.IgnoreFlags;
cbLHIgnoreCompressionMethod.Checked := fProcessingSettings.LocalHeader.IgnoreCompressionMethod;
cbLHIgnoreModTime.Checked := fProcessingSettings.LocalHeader.IgnoreModTime;
cbLHIgnoreModDate.Checked := fProcessingSettings.LocalHeader.IgnoreModDate;
cbLHIgnoreCRC32.Checked := fProcessingSettings.LocalHeader.IgnoreCRC32;
cbLHIgnoreSizes.Checked := fProcessingSettings.LocalHeader.IgnoreSizes;
cbLHIgnoreExtraField.Checked := fProcessingSettings.LocalHeader.IgnoreExtraField;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.FormToSettings;
begin
If rbRebuild.Checked then fProcessingSettings.RepairMethod := rmRebuild;
If rbExtract.Checked then fProcessingSettings.RepairMethod := rmExtract;
fProcessingSettings.RepairData := lbleData.Text;
fProcessingSettings.IgnoreFileSignature := cbIgnoreFileSignature.Checked;
fProcessingSettings.AssumeCompressionMethods := cbAssumeCompressionMethods.Checked;
//eocd
fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory := cbIgnoreEndOfCentralDirectory.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit := cbIgnoreDiskSplit.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries := cbIgnoreNumberOfEntries.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset := cbIgnoreCentralDirectoryOffset.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreComment := cbIgnoreComment.Checked;
//central directory
fProcessingSettings.CentralDirectory.IgnoreCentralDirectory := cbCDIgnoreCentralDirectory.Checked;
fProcessingSettings.CentralDirectory.IgnoreSignature := cbCDIgnoreSignature.Checked;
fProcessingSettings.CentralDirectory.IgnoreVersions := cbCDIgnoreVersions.Checked;
fProcessingSettings.CentralDirectory.IgnoreFlags := cbCDIgnoreFlags.Checked;
fProcessingSettings.CentralDirectory.IgnoreCompressionMethod := cbCDIgnoreCompressionMethod.Checked;
fProcessingSettings.CentralDirectory.IgnoreModTime := cbCDIgnoreModTime.Checked;
fProcessingSettings.CentralDirectory.IgnoreModDate := cbCDIgnoreModDate.Checked;
fProcessingSettings.CentralDirectory.IgnoreCRC32 := cbCDIgnoreCRC32.Checked;
fProcessingSettings.CentralDirectory.IgnoreSizes := cbCDIgnoreSizes.Checked;
fProcessingSettings.CentralDirectory.IgnoreInternalFileAttributes := cbCDIgnoreInternalFileAttributes.Checked;
fProcessingSettings.CentralDirectory.IgnoreExternalFileAttributes := cbCDIgnoreExternalFileAttributes.Checked;
fProcessingSettings.CentralDirectory.IgnoreLocalHeaderOffset := cbCDIgnoreLocalHeaderOffset.Checked;
fProcessingSettings.CentralDirectory.IgnoreExtraField := cbCDIgnoreExtraField.Checked;
fProcessingSettings.CentralDirectory.IgnoreFileComment := cbCDIgnoreFileComment.Checked;
//local headers
fProcessingSettings.LocalHeader.IgnoreSignature := cbLHIgnoreSignature.Checked;
fProcessingSettings.LocalHeader.IgnoreVersions := cbLHIgnoreVersions.Checked;
fProcessingSettings.LocalHeader.IgnoreFlags := cbLHIgnoreFlags.Checked;
fProcessingSettings.LocalHeader.IgnoreCompressionMethod := cbLHIgnoreCompressionMethod.Checked;
fProcessingSettings.LocalHeader.IgnoreModTime := cbLHIgnoreModTime.Checked;
fProcessingSettings.LocalHeader.IgnoreModDate := cbLHIgnoreModDate.Checked;
fProcessingSettings.LocalHeader.IgnoreCRC32 := cbLHIgnoreCRC32.Checked;
fProcessingSettings.LocalHeader.IgnoreSizes := cbLHIgnoreSizes.Checked;
fProcessingSettings.LocalHeader.IgnoreExtraField := cbLHIgnoreExtraField.Checked;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.ShowProcessingSettings(const FilePath: String; var ProcessingSettings: TProcessingSettings);
begin
fFilePath := FilePath;
fProcessingSettings := ProcessingSettings;
lblFile.Caption := ExtractFileName(FilePath);
fSelecting := True;
try
  SettingsToForm;
finally
  fSelecting := False;
end;
cbIgnoreEndOfCentralDirectory.OnClick(cbIgnoreEndOfCentralDirectory);
cbCDIgnoreCentralDirectory.OnClick(cbCDIgnoreCentralDirectory);
fAccepted := False;
ShowModal;
FormToSettings;
If fAccepted then
  ProcessingSettings := fProcessingSettings;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.RepairMethodClick(Sender: TObject);
begin
If Sender is TRadioButton then
  begin
    btnBrowse.Tag := TRadioButton(Sender).Tag;
    case TRadioButton(Sender).Tag of
      0:  begin
            lbleData.EditLabel.Caption := 'Output file:';
            fProcessingSettings.RepairData := ExtractFilePath(fFilePath) + 'unlocked_' + ExtractFileName(fFilePath);
          end;
      1:  begin
            lbleData.EditLabel.Caption := 'Extract into:';
            fProcessingSettings.RepairData := ExtractFilePath(fFilePath) + 'extracted\';
          end;
    end;
    lbleData.Text := fProcessingSettings.RepairData;
  end;
end;

//------------------------------------------------------------------------------

procedure TfPrcsSettingsForm.CheckBoxClick(Sender: TObject);
begin
If (Sender is TCheckBox) and not fSelecting then
  begin
    case TCheckBox(Sender).Tag of
      100:  begin
              cbIgnoreDiskSplit.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreNumberOfEntries.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreCentralDirectoryOffset.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
              cbIgnoreComment.Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
            end;
      200:  begin
              cbCDIgnoreSignature.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreVersions.Enabled := not cbCDIgnoreCentralDirectory.Checked;
              cbCDIgnoreFlags.Enabled := not cbCDIgnoreCentralDirectory.Checked;
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
              If TCheckBox(Sender).Checked then
                begin
                  If cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
                    cbLHIgnoreSizes.Checked := False;
                end;
            end;
       204: If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
              cbCDIgnoreSizes.Checked := False;
       208: If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
              cbCDIgnoreCompressionMethod.Checked := False;
       303: If cbCDIgnoreCentralDirectory.Checked then
              cbLHIgnoreSizes.Checked := not TCheckBox(Sender).Checked
            else
              If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked then
                cbLHIgnoreSizes.Checked := not TCheckBox(Sender).Checked;
       307: If cbCDIgnoreCentralDirectory.Checked then
              cbLHIgnoreCompressionMethod.Checked := not TCheckBox(Sender).Checked
            else
              If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked then
                cbLHIgnoreCompressionMethod.Checked := not TCheckBox(Sender).Checked
    else
    end;
  end;
end;

//==============================================================================

procedure TfPrcsSettingsForm.FormShow(Sender: TObject);
begin
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
  0:  begin
        If DirectoryExists(ExtractFileDir(fFilePath)) then
          diaSaveDialog.InitialDir := ExtractFileDir(fFilePath);
        If diaSaveDialog.Execute then
          lbleData.Text := diaSaveDialog.FileName;
      end;
  1:  begin
        If DirectoryExists(ExtractFileDir(fFilePath)) or
           DirectoryExists(ExtractFileDir(ExpandFileName(fFilePath + '..'))) then
          TempStr := ExtractFileDir(fFilePath)
        else
          TempStr := ExtractFileDir(ParamStr(0));
        If SelectDirectory('Select folder for archive extraction.','',TempStr) then
          lbleData.Text := IncludeTrailingPathDelimiter(TempStr);
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
begin
fAccepted := True;
Close;
end;
  
end.
