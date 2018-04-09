{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ProcSettingsFrame_ZIP;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls,
  DART_ProcessingSettings;

type
  TOptionDescriptionEvent = procedure(Sender: TObject; DescriptionTag: Integer) of object;

  TfrmProcSettingsFrame_ZIP = class(TFrame)
    pnlBackground: TPanel;
    gbGeneral: TGroupBox;
    cbAssumeCompressionMethod: TCheckBox;
    gbEndOfCentralDirectory: TGroupBox;
    cbEOCDIgnoreEndOfCentralDirectory: TCheckBox;
    cbEOCDIgnoreDiskSplit: TCheckBox;
    cbEOCDIgnoreNumberOfEntries: TCheckBox;
    cbEOCDIgnoreCentralDirectoryOffset: TCheckBox;
    cbEOCDIgnoreComment: TCheckBox;
    bvlEOCDSplit: TBevel;
    cbEOCDLimitSearch: TCheckBox;
    gbCentralDirectoryHeaders: TGroupBox;
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
    gbLocalHeaders: TGroupBox;
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
  private
    fArchiveProcessingSettings: TDARTArchiveProcessingSettings;
    fLoading:                   Boolean;
    fOnOptionDescription:       TOptionDescriptionEvent;
    fProcessingSettings:        TDART_PS_ZIP;
  public
    procedure Initialize;
    procedure SettingsToFrame;
    procedure FrameToSettings;
    procedure ShowProcessingSettings(const ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
    procedure RetrieveProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
  published
    procedure CheckBoxClick(Sender: TObject);
    procedure OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    property OnOptionDescription: TOptionDescriptionEvent read fOnOptionDescription write fOnOptionDescription;
  end;

implementation

{$R *.dfm}

procedure TfrmProcSettingsFrame_ZIP.Initialize;
begin
// nothing to do here
end;  

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.SettingsToFrame;
begin
// general
cbAssumeCompressionMethod.Checked := fProcessingSettings.AssumeCompressionMethod;
// eocd
cbEOCDIgnoreEndOfCentralDirectory.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory;
cbEOCDIgnoreDiskSplit.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit;
cbEOCDIgnoreNumberOfEntries.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries;
cbEOCDIgnoreCentralDirectoryOffset.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset;
cbEOCDIgnoreComment.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreComment;
cbEOCDLimitSearch.Checked := fProcessingSettings.EndOfCentralDirectory.LimitSearch;
// central directory
cbCDIgnoreCentralDirectory.Checked := fProcessingSettings.CentralDirectory.IgnoreCentralDirectory;
cbCDIgnoreSignature.Checked := fProcessingSettings.CentralDirectory.IgnoreSignature;
cbCDIgnoreVersions.Checked := fProcessingSettings.CentralDirectory.IgnoreVersions;
cbCDClearEncryptionFlags.Checked := fProcessingSettings.CentralDirectory.ClearEncryptionFlags;
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
// local headers
cbLHIgnoreLocalHeaders.Checked := fProcessingSettings.LocalHeader.IgnoreLocalHeaders;
cbLHIgnoreSignature.Checked := fProcessingSettings.LocalHeader.IgnoreSignature;
cbLHIgnoreVersions.Checked := fProcessingSettings.LocalHeader.IgnoreVersions;
cbLHClearEncryptionFlags.Checked := fProcessingSettings.LocalHeader.ClearEncryptionFlags;
cbLHIgnoreCompressionMethod.Checked := fProcessingSettings.LocalHeader.IgnoreCompressionMethod;
cbLHIgnoreModTime.Checked := fProcessingSettings.LocalHeader.IgnoreModTime;
cbLHIgnoreModDate.Checked := fProcessingSettings.LocalHeader.IgnoreModDate;
cbLHIgnoreCRC32.Checked := fProcessingSettings.LocalHeader.IgnoreCRC32;
cbLHIgnoreSizes.Checked := fProcessingSettings.LocalHeader.IgnoreSizes;
cbLHIgnoreFileName.Checked := fProcessingSettings.LocalHeader.IgnoreFileName;
cbLHIgnoreExtraField.Checked := fProcessingSettings.LocalHeader.IgnoreExtraField;
// data descriptor
cbLHIgnoreDataDescriptor.Checked := fProcessingSettings.LocalHeader.IgnoreDataDescriptor;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.FrameToSettings;
begin
// general
fProcessingSettings.AssumeCompressionMethod := cbAssumeCompressionMethod.Checked;
// eocd
fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory := cbEOCDIgnoreEndOfCentralDirectory.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit := cbEOCDIgnoreDiskSplit.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries := cbEOCDIgnoreNumberOfEntries.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset := cbEOCDIgnoreCentralDirectoryOffset.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreComment := cbEOCDIgnoreComment.Checked;
fProcessingSettings.EndOfCentralDirectory.LimitSearch := cbEOCDLimitSearch.Checked;
// central directory
fProcessingSettings.CentralDirectory.IgnoreCentralDirectory := cbCDIgnoreCentralDirectory.Checked;
fProcessingSettings.CentralDirectory.IgnoreSignature := cbCDIgnoreSignature.Checked;
fProcessingSettings.CentralDirectory.IgnoreVersions := cbCDIgnoreVersions.Checked;
fProcessingSettings.CentralDirectory.ClearEncryptionFlags := cbCDClearEncryptionFlags.Checked;
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
// local headers
fProcessingSettings.LocalHeader.IgnoreLocalHeaders := cbLHIgnoreLocalHeaders.Checked;
fProcessingSettings.LocalHeader.IgnoreSignature := cbLHIgnoreSignature.Checked;
fProcessingSettings.LocalHeader.IgnoreVersions := cbLHIgnoreVersions.Checked;
fProcessingSettings.LocalHeader.ClearEncryptionFlags := cbLHClearEncryptionFlags.Checked;
fProcessingSettings.LocalHeader.IgnoreCompressionMethod := cbLHIgnoreCompressionMethod.Checked;
fProcessingSettings.LocalHeader.IgnoreModTime := cbLHIgnoreModTime.Checked;
fProcessingSettings.LocalHeader.IgnoreModDate := cbLHIgnoreModDate.Checked;
fProcessingSettings.LocalHeader.IgnoreCRC32 := cbLHIgnoreCRC32.Checked;
fProcessingSettings.LocalHeader.IgnoreSizes := cbLHIgnoreSizes.Checked;
fProcessingSettings.LocalHeader.IgnoreFileName := cbLHIgnoreFileName.Checked;
fProcessingSettings.LocalHeader.IgnoreExtraField := cbLHIgnoreExtraField.Checked;
// data descriptor
fProcessingSettings.LocalHeader.IgnoreDataDescriptor := cbLHIgnoreDataDescriptor.Checked;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.ShowProcessingSettings(const ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
fProcessingSettings := ArchiveProcessingSettings.ZIP;
fArchiveProcessingSettings := ArchiveProcessingSettings;
fLoading := True;
try
  SettingsToFrame;
finally
  fLoading := False;
end;
// enable/disable
cbEOCDIgnoreEndOfCentralDirectory.OnClick(cbEOCDIgnoreEndOfCentralDirectory);
cbCDIgnoreCentralDirectory.OnClick(cbCDIgnoreCentralDirectory);
cbLHIgnoreLocalHeaders.OnClick(cbLHIgnoreLocalHeaders);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.RetrieveProcessingSettings(var ArchiveProcessingSettings: TDARTArchiveProcessingSettings);
begin
FrameToSettings;
ArchiveProcessingSettings.ZIP := fProcessingSettings;
end;

//==============================================================================

procedure TfrmProcSettingsFrame_ZIP.CheckBoxClick(Sender: TObject);
var
  i:  Integer;
begin
inherited;
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    201:  begin   // cbIgnoreEndOfCentralDirectory
            For i := 0 to Pred(gbEndOfCentralDirectory.ControlCount) do
              If (gbEndOfCentralDirectory.Controls[i] is TCheckBox) and (gbEndOfCentralDirectory.Controls[i] <> cbEOCDIgnoreEndOfCentralDirectory) then
                gbEndOfCentralDirectory.Controls[i].Enabled := not cbEOCDIgnoreEndOfCentralDirectory.Checked;
          end;
    301:  begin   // cbCDIgnoreCentralDirectory
            For i := 0 to Pred(gbCentralDirectoryHeaders.ControlCount) do
              If (gbCentralDirectoryHeaders.Controls[i] is TCheckBox) and (gbCentralDirectoryHeaders.Controls[i] <> cbCDIgnoreCentralDirectory) then
                gbCentralDirectoryHeaders.Controls[i].Enabled := not cbCDIgnoreCentralDirectory.Checked;
            If TCheckBox(Sender).Checked then
              begin
                cbLHIgnoreLocalHeaders.Checked := False;
                If cbLHIgnoreCompressionMethod.Checked then
                  cbLHIgnoreSizes.Checked := False;
                cbLHIgnoreFileName.Checked := False;
              end;
          end;
    305:  begin   // cbCDIgnoreCompressionMethod
            If TCheckBox(Sender).Checked and (cbLHIgnoreLocalHeaders.Checked or
             (cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked)) then
              cbCDIgnoreSizes.Checked := False;
          end;
    309:  begin   // cbCDIgnoreSizes
            If TCheckBox(Sender).Checked and (cbLHIgnoreLocalHeaders.Checked or
              (cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked)) then
              cbCDIgnoreCompressionMethod.Checked := False;
          end;
    312:  begin   // cbCDIgnoreLocalHeaderOffset
            If TCheckBox(Sender).Checked then
              cbLHIgnoreLocalHeaders.Checked := False;
          end;
    401:  begin   // cbLHIgnoreLocalHeaders
            For i := 0 to Pred(gbLocalHeaders.ControlCount) do
              If (gbLocalHeaders.Controls[i] is TCheckBox) and (gbLocalHeaders.Controls[i] <> cbLHIgnoreLocalHeaders) then
                gbLocalHeaders.Controls[i].Enabled := not cbLHIgnoreLocalHeaders.Checked;
            If TCheckBox(Sender).Checked then
              begin
                cbCDIgnoreCentralDirectory.Checked := False;
                cbCDIgnoreLocalHeaderOffset.Checked := False; 
                If cbCDIgnoreCompressionMethod.Checked then
                  cbCDIgnoreSizes.Checked := False;
              end;
          end;
    405:  begin   // cbLHIgnoreCompressionMethod
            If TCheckBox(Sender).Checked and (cbCDIgnoreCentralDirectory.Checked or
             (cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked)) then
              cbLHIgnoreSizes.Checked := False;
          end;
    409:  begin   // cbLHIgnoreSizes
            If TCheckBox(Sender).Checked and (cbCDIgnoreCentralDirectory.Checked or
             (cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked)) then
              cbLHIgnoreCompressionMethod.Checked := False;    
          end;
    410:  begin   // cbLHIgnoreFileName
            If TCheckBox(Sender).Checked then
              cbCDIgnoreCentralDirectory.Checked := False;
          end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.OptionMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If (Sender is TControl) and Assigned(fOnOptionDescription) then
  fOnOptionDescription(Self,TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsFrame_ZIP.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  Control:  TControl;
begin
If Sender is TGroupBox then
  begin
    Control := TGroupBox(Sender).ControlAtPos(Point(X,Y),True,True);
    If Assigned(Control) and (Control is TCheckBox) then
      OptionMouseMove(Control,Shift,X,Y);
  end;
end;

end.
