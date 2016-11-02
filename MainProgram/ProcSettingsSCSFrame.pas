{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ProcSettingsSCSFrame;

{$INCLUDE 'Source\DART_defs.inc'}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ExtCtrls, StdCtrls, Spin,
  DART_ProcessingSettings;

type
  TSettingsHintEvet = procedure(Sender: TObject; HintTag: Integer) of object;  

type
  TfrmProcSettingsSCS = class(TFrame)
    pnlBackground: TPanel;
    gbEntries: TGroupBox;
    cbIgnoreCRC32: TCheckBox;
    cbIgnoreCompressionFlag: TCheckBox;
    gbPathResolve: TGroupBox;
    cbAssumeCityHash: TCheckBox;
    cbUsePredefinedPaths: TCheckBox;
    cbExtractUnresolvedEntries: TCheckBox;
    lblCustomPaths: TLabel;
    meCustomPaths: TMemo;
    lblHelpFiles: TLabel;
    meHelpFiles: TMemo;
    bvlHorSplit: TBevel;
    cbBruteForceResolve: TCheckBox;
    cbLimitedAlphabet: TCheckBox;
    lblLengthLimit: TLabel;
    seLengthLimit: TSpinEdit;
  private
    fProcessingSettings: TSCS_Settings;
    fLoading:            Boolean; 
    fOnSettingsHint:     TSettingsHintEvet;
  public
    procedure SettingsToFrame;
    procedure FrameToSettings;
    procedure ShowProcessingSettings(ProcessingSettings: TSCS_Settings);
    Function RetrieveProcessingSettings: TSCS_Settings;
  published
    procedure CheckBoxClick(Sender: TObject);
    procedure SettingsMouseMove(Sender: TObject; {%H-}Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    property OnSettingsHint: TSettingsHintEvet read fOnSettingsHint write fOnSettingsHint;
  end;

implementation

{$R *.dfm}

procedure TfrmProcSettingsSCS.SettingsToFrame;
var
  i:  Integer;
begin
// entry
cbIgnoreCRC32.Checked := fProcessingSettings.Entry.IgnoreCRC32;
cbIgnoreCompressionFlag.Checked := fProcessingSettings.Entry.IgnoreCompressionFlag;
// path resolve
cbAssumeCityHash.Checked := fProcessingSettings.PathResolve.AssumeCityHash;
cbUsePredefinedPaths.Checked := fProcessingSettings.PathResolve.UsePredefinedPaths;
cbExtractUnresolvedEntries.Checked := fProcessingSettings.PathResolve.ExtractedUnresolvedEntries;
meHelpFiles.Lines.BeginUpdate;
try
  meHelpFiles.Clear;
  For i := Low(fProcessingSettings.PathResolve.HelpFiles) to High(fProcessingSettings.PathResolve.HelpFiles) do
    meHelpFiles.Lines.Add(fProcessingSettings.PathResolve.HelpFiles[i]);
finally
  meHelpFiles.Lines.EndUpdate;
end;
meCustomPaths.Lines.BeginUpdate;
try
  meCustomPaths.Clear;
  For i := Low(fProcessingSettings.PathResolve.CustomPaths) to High(fProcessingSettings.PathResolve.CustomPaths) do
    meCustomPaths.Lines.Add(fProcessingSettings.PathResolve.CustomPaths[i]);
finally
  meCustomPaths.Lines.EndUpdate;
end;
cbBruteForceResolve.Checked := fProcessingSettings.PathResolve.BruteForceResolve;
cbLimitedAlphabet.Checked := fProcessingSettings.PathResolve.BruteForceLimitedAlphabet;
seLengthLimit.Value := fProcessingSettings.PathResolve.BruteForceLengthLimit;
end;
 
//------------------------------------------------------------------------------

procedure TfrmProcSettingsSCS.FrameToSettings;
var
  i:  Integer;
begin
// entry
fProcessingSettings.Entry.IgnoreCRC32 := cbIgnoreCRC32.Checked;
fProcessingSettings.Entry.IgnoreCompressionFlag := cbIgnoreCompressionFlag.Checked;
// path resolve
fProcessingSettings.PathResolve.AssumeCityHash := cbAssumeCityHash.Checked;
fProcessingSettings.PathResolve.UsePredefinedPaths := cbUsePredefinedPaths.Checked;
fProcessingSettings.PathResolve.ExtractedUnresolvedEntries := cbExtractUnresolvedEntries.Checked;
SetLength(fProcessingSettings.PathResolve.HelpFiles,meHelpFiles.Lines.Count);
For i := 0 to Pred(meHelpFiles.Lines.Count) do
   fProcessingSettings.PathResolve.HelpFiles[i] := meHelpFiles.Lines[i];
SetLength(fProcessingSettings.PathResolve.CustomPaths,meCustomPaths.Lines.Count);
For i := 0 to Pred(meCustomPaths.Lines.Count) do
   fProcessingSettings.PathResolve.CustomPaths[i] := meCustomPaths.Lines[i];
fProcessingSettings.PathResolve.BruteForceResolve := cbBruteForceResolve.Checked;
fProcessingSettings.PathResolve.BruteForceLimitedAlphabet := cbLimitedAlphabet.Checked;
fProcessingSettings.PathResolve.BruteForceLengthLimit := seLengthLimit.Value;
end;
 
//------------------------------------------------------------------------------

procedure TfrmProcSettingsSCS.ShowProcessingSettings(ProcessingSettings: TSCS_Settings);
begin
fProcessingSettings := ProcessingSettings;
fLoading := True;
try
  SettingsToFrame;
finally
  fLoading := False;
end;
// enable/disable
cbBruteForceResolve.OnClick(cbBruteForceResolve);
end;
 
//------------------------------------------------------------------------------

Function TfrmProcSettingsSCS.RetrieveProcessingSettings: TSCS_Settings;
begin
FrameToSettings;
Result := fProcessingSettings;
end;

//==============================================================================

procedure TfrmProcSettingsSCS.CheckBoxClick(Sender: TObject);
begin
If (Sender is TCheckBox) and not fLoading then
  case TCheckBox(Sender).Tag of
    146:  begin   // cbBruteForceResolve
            cbLimitedAlphabet.Enabled := TCheckBox(Sender).Checked;
            lblLengthLimit.Enabled := TCheckBox(Sender).Checked;
            seLengthLimit.Enabled := TCheckBox(Sender).Checked;
          end;
  end;
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsSCS.SettingsMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
If (Sender is TControl) and Assigned(fOnSettingsHint) then
  fOnSettingsHint(Self,TControl(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TfrmProcSettingsSCS.GroupBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
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
    If Assigned(Control) and ((Control is TCheckBox) or (Control is TSpinEdit)) then
      SettingsMouseMove(Control,Shift,X,Y);
  end;
end;

end.
