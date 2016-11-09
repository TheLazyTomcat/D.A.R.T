{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ProcSettingsZIPFrame;

{$INCLUDE 'Source\DART_defs.inc'}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls,
  DART_ProcessingSettings;

type
  TSettingsHintEvent = procedure(Sender: TObject; HintTag: Integer) of object;

  TfrmProcSettingsZIP = class(TFrame)
    pnlBackground: TPanel;
    gbGeneral: TGroupBox;
    cbAssumeCompressionMethods: TCheckBox;    
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
  private
    fFileProcessingSettings:  TFileProcessingSettings;
    fProcessingSettings:      TZIP_Settings;
    fLoading:                 Boolean;
    fOnSettingsHint:          TSettingsHintEvent;
  public
    procedure Initialize;
    procedure SettingsToFrame;
    procedure FrameToSettings;
    procedure ShowProcessingSettings(FileProcessingSettings: TFileProcessingSettings);
    Function RetrieveProcessingSettings: TZIP_Settings;
  published
    procedure CheckBoxClick(Sender: TObject);
    procedure SettingsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    property OnSettingsHint: TSettingsHintEvent read fOnSettingsHint write fOnSettingsHint;
  end;

implementation

{$R *.dfm}

procedure TfrmProcSettingsZIP.Initialize;
begin
// nothing to do here
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsZIP.SettingsToFrame;
begin
// general
cbAssumeCompressionMethods.Checked := fProcessingSettings.AssumeCompressionMethods;
// eocd
cbIgnoreEndOfCentralDirectory.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory;
cbIgnoreDiskSplit.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit;
cbIgnoreNumberOfEntries.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries;
cbIgnoreCentralDirectoryOffset.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset;
cbIgnoreComment.Checked := fProcessingSettings.EndOfCentralDirectory.IgnoreComment;
cbLimitSearch.Checked := fProcessingSettings.EndOfCentralDirectory.LimitSearch;
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

procedure TfrmProcSettingsZIP.FrameToSettings;
begin
// general
fProcessingSettings.AssumeCompressionMethods := cbAssumeCompressionMethods.Checked;
// eocd
fProcessingSettings.EndOfCentralDirectory.IgnoreEndOfCentralDirectory := cbIgnoreEndOfCentralDirectory.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreDiskSplit := cbIgnoreDiskSplit.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreNumberOfEntries := cbIgnoreNumberOfEntries.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreCentralDirectoryOffset := cbIgnoreCentralDirectoryOffset.Checked;
fProcessingSettings.EndOfCentralDirectory.IgnoreComment := cbIgnoreComment.Checked;
fProcessingSettings.EndOfCentralDirectory.LimitSearch := cbLimitSearch.Checked;
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

procedure TfrmProcSettingsZIP.ShowProcessingSettings(FileProcessingSettings: TFileProcessingSettings);
begin
fFileProcessingSettings := FileProcessingSettings;
fProcessingSettings := FileProcessingSettings.ZIPSettings;
fLoading := True;
try
  SettingsToFrame;
finally
  fLoading := False;
end;
// enable/disable
cbIgnoreEndOfCentralDirectory.OnClick(cbIgnoreEndOfCentralDirectory);
cbCDIgnoreCentralDirectory.OnClick(cbCDIgnoreCentralDirectory);
cbLHIgnoreLocalHeaders.OnClick(cbLHIgnoreLocalHeaders);
end;

//------------------------------------------------------------------------------

Function TfrmProcSettingsZIP.RetrieveProcessingSettings: TZIP_Settings;
begin
FrameToSettings;
Result := fProcessingSettings;
end;

//==============================================================================

procedure TfrmProcSettingsZIP.CheckBoxClick(Sender: TObject);
var
  i:  Integer;
begin
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    41: begin   // cbIgnoreEndOfCentralDirectory
          For i := 0 to Pred(grdEndOfCentralDirectory.ControlCount) do
            If (grdEndOfCentralDirectory.Controls[i] is TCheckBox) and (grdEndOfCentralDirectory.Controls[i] <> cbIgnoreEndOfCentralDirectory) then
              grdEndOfCentralDirectory.Controls[i].Enabled := not cbIgnoreEndOfCentralDirectory.Checked;
        end;
    61: begin   // cbCDIgnoreCentralDirectory
          For i := 0 to Pred(grbCentralDirectoryHeaders.ControlCount) do
            If (grbCentralDirectoryHeaders.Controls[i] is TCheckBox) and (grbCentralDirectoryHeaders.Controls[i] <> cbCDIgnoreCentralDirectory) then
              grbCentralDirectoryHeaders.Controls[i].Enabled := not cbCDIgnoreCentralDirectory.Checked;
          If TCheckBox(Sender).Checked then
            begin
              If cbLHIgnoreCompressionMethod.Checked then
                cbLHIgnoreSizes.Checked := False;
              cbLHIgnoreLocalHeaders.Checked := False;
              cbLHIgnoreFileName.Checked := False;
            end;
        end;
    65: begin   // cbCDIgnoreCompressionMethod
          If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
            cbCDIgnoreSizes.Checked := False;
        end;
    69: begin   // cbCDIgnoreSizes
          If TCheckBox(Sender).Checked and cbLHIgnoreSizes.Checked and cbLHIgnoreCompressionMethod.Checked then
            cbCDIgnoreCompressionMethod.Checked := False;
        end;
    72: begin   // cbCDIgnoreLocalHeaderOffset
          If TCheckBox(Sender).Checked then
            cbLHIgnoreLocalHeaders.Checked := False;
        end;
    81: begin   // cbLHIgnoreLocalHeaders
          For i := 0 to Pred(grbLocalHeaders.ControlCount) do
            If (grbLocalHeaders.Controls[i] is TCheckBox) and (grbLocalHeaders.Controls[i] <> cbLHIgnoreLocalHeaders) then
              grbLocalHeaders.Controls[i].Enabled := not cbLHIgnoreLocalHeaders.Checked;
          If TCheckBox(Sender).Checked then
            begin
              cbCDIgnoreCentralDirectory.Checked := False;
              cbCDIgnoreLocalHeaderOffset.Checked := False;
              If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked then
                cbCDIgnoreCompressionMethod.Checked := False;
            end;
        end;
    85: begin   // cbLHIgnoreCompressionMethod
          If cbCDIgnoreCentralDirectory.Checked then
            begin
              If TCheckBox(Sender).Checked then
                cbLHIgnoreSizes.Checked := False;
            end
          else
            If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked and TCheckBox(Sender).Checked then
              cbLHIgnoreSizes.Checked := False;
        end;
    89: begin   // cbLHIgnoreSizes
          If cbCDIgnoreCentralDirectory.Checked then
            begin
              If TCheckBox(Sender).Checked then
                cbLHIgnoreCompressionMethod.Checked := False;
            end
          else
            If cbCDIgnoreSizes.Checked and cbCDIgnoreCompressionMethod.Checked and TCheckBox(Sender).Checked then
              cbLHIgnoreCompressionMethod.Checked := False;
        end;
    90: begin   // cbLHIgnoreFileName
          If TCheckBox(Sender).Checked then
            cbCDIgnoreCentralDirectory.Checked := False;
        end;    
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsZIP.SettingsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If (Sender is TControl) and Assigned(fOnSettingsHint) then
  fOnSettingsHint(Self,TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsZIP.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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

end.
