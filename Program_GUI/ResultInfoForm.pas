{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ResultInfoForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,
  DART_ProcessingManager;

type
  TfResultInfoForm = class(TForm)
    lblProcessingResult: TLabel;
    meResultInfo: TMemo;
    lblCopyHint: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure meResultInfoKeyPress(Sender: TObject; var Key: Char);
  private
    VersionStr: String;
  public
    procedure ShowResultInformation(const ArchiveInfo: TDARTArchiveListItem);
  end;

var
  fResultInfoForm: TfResultInfoForm;

implementation

{$R *.dfm}

uses
  ClipBrd,
  WinFileInfo,
  DART_Repairer, DART_ProcessingSettings;

procedure TfResultInfoForm.ShowResultInformation(const ArchiveInfo: TDARTArchiveListItem);
var
  i:  Integer;
begin
meResultInfo.Lines.BeginUpdate;
try
  // basic info and settings
  meResultInfo.Clear;
  meResultInfo.Lines.Add('Archive file: ' + ArchiveInfo.Name);
  meResultInfo.Lines.Add(Format('Archive size: %s (%d bytes)',[SizeToStr(ArchiveInfo.Size),ArchiveInfo.Size]));
  If ArchiveInfo.ArchiveProcessingSettings.Common.SelectedArchiveType <>
     ArchiveInfo.ArchiveProcessingSettings.Common.OriginalArchiveType then
    meResultInfo.Lines.Add(Format('Archive type: %s (%s)',[
      DART_ArchiveTypeStrings[ArchiveInfo.ArchiveProcessingSettings.Common.SelectedArchiveType],
      DART_ArchiveTypeStrings[ArchiveInfo.ArchiveProcessingSettings.Common.OriginalArchiveType]]))
  else
    meResultInfo.Lines.Add(Format('Archive type: %s',[
      DART_ArchiveTypeStrings[ArchiveInfo.ArchiveProcessingSettings.Common.SelectedArchiveType]]));
  meResultInfo.Lines.Add(Format('    Repairer: %s (%s)',[ArchiveInfo.ResultInfo.RepairerInfo,VersionStr]));
  // additional info (warning or error messages)
  case ArchiveInfo.ResultInfo.ResultState of
    rsNormal:   lblProcessingResult.Caption := 'Processing completed successfuly';
    rsWarning:  with ArchiveInfo.ResultInfo.WarningInfo do
                  begin
                    lblProcessingResult.Caption := 'Processing completed with warnings';
                    meResultInfo.Lines.Add(sLineBreak + StringOfChar('-',80) + sLineBreak);
                    meResultInfo.Lines.Add(Format('Warnings(%d): ',[Warnings.Count]));
                    meResultInfo.Lines.Add('');
                    For i := Low(Warnings.Arr) to Pred(Warnings.Count) do
                      meResultInfo.Lines.Add('  ' + Warnings.Arr[i]);
                  end;
    rsError:    with ArchiveInfo.ResultInfo.ErrorInfo do
                  begin
                    lblProcessingResult.Caption := 'An error occurred during the processing';
                    meResultInfo.Lines.Add(sLineBreak + StringOfChar('-',80) + sLineBreak);
                    If (FaultObjectClass <> '') and Assigned(FaultObjectRef) then
                      meResultInfo.Lines.Add(Format('       Function: %s(0x%p).%s [%d]',
                       [FaultObjectClass,Pointer(FaultObjectRef),FaultFunctionName,FaultFunctionIndex]))
                    else
                      meResultInfo.Lines.Add(Format('       Function: %s [%d]',[FaultFunctionName,FaultFunctionIndex]));
                    meResultInfo.Lines.Add('Exception class: ' + ExceptionClass);
                    meResultInfo.Lines.Add(sLineBreak + 'Exception text: ');
                    meResultInfo.Lines.Add(sLineBreak + '  ' + ExceptionText);
                  end;
  else
    lblProcessingResult.Caption := 'Unknown state';
  end;
finally
  meResultInfo.Lines.EndUpdate;
end;
ShowModal;
end;

//==============================================================================

procedure TfResultInfoForm.FormCreate(Sender: TObject);
begin
with TWinFileInfo.Create(WFI_LS_LoadVersionInfo or WFI_LS_LoadFixedFileInfo or WFI_LS_DecodeFixedFileInfo) do
try
  VersionStr :=
    VersionInfoValues[VersionInfoTranslations[0].LanguageStr,'ProductVersion'] +
    {$IFDEF FPC}' L'{$ELSE}' D'{$ENDIF}{$IFDEF x64}+ '64 '{$ELSE}+ '32 '{$ENDIF} +
    '#' + IntToStr(VersionInfoFixedFileInfoDecoded.FileVersionMembers.Build)
    {$IFDEF Debug}+ ' debug'{$ENDIF};
finally
  Free;
end;
end;

//------------------------------------------------------------------------------

procedure TfResultInfoForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
If Key = ^C then
  Clipboard.AsText := lblProcessingResult.Caption + sLineBreak + sLineBreak +
    StringOfChar('-',60) + sLineBreak + sLineBreak + meResultInfo.Text;
end;

//------------------------------------------------------------------------------

procedure TfResultInfoForm.meResultInfoKeyPress(Sender: TObject; var Key: Char);
begin
If Key = ^A then
  begin
    Key := #0;
    meResultInfo.SelectAll;
  end;
end;

end.
