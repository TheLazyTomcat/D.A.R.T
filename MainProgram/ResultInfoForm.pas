{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ResultInfoForm;

{$INCLUDE 'Source\DART_defs.inc'}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  DART_FileManager;

type
  TfResultInfoForm = class(TForm)
    lblResultCaption: TLabel;
    meResultInfoText: TMemo;
    lblHint: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    procedure ShowResultInformation(const FileInfo: TFileListItem);
  end;

var
  fResultInfoForm: TfResultInfoForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}  

uses
  ClipBrd,
  DART_Repairer;

//==============================================================================

procedure TfResultInfoForm.ShowResultInformation(const FileInfo: TFileListItem);
var
  i:  Integer;
begin
meResultInfoText.Clear;
meResultInfoText.Lines.Add('     File: ' + FileInfo.Name);
meResultInfoText.Lines.Add('File size: ' + IntToStr(FileInfo.Size) + ' bytes');
meResultInfoText.Lines.Add(' Repairer: ' + FileInfo.ResultInfo.RepairerInfo);
case FileInfo.ResultInfo.ResultState of
  rsNormal:   lblResultCaption.Caption := 'Processing completed successfuly';
  rsWarning:  with FileInfo.ResultInfo.WarningInfo do
                begin
                  lblResultCaption.Caption := 'Processing completed with warnings';
                  meResultInfoText.Lines.Add(sLineBreak + StringOfChar('-',80) + sLineBreak);
                  meResultInfoText.Lines.Add(Format('Warnings(%d): ',[Length(Warnings)]));
                  meResultInfoText.Lines.Add('');
                  For i := Low(Warnings) to High(Warnings) do
                    meResultInfoText.Lines.Add('  ' + Warnings[i]);
                end;
  rsError:    with FileInfo.ResultInfo.ErrorInfo do
                begin
                  lblResultCaption.Caption := 'An error occured during the processing';
                  meResultInfoText.Lines.Add(sLineBreak + StringOfChar('-',80) + sLineBreak);
                  If (ErrorSourceObjectClass <> '') and Assigned(ErrorSourceObject) then
                    meResultInfoText.Lines.Add(Format('       Function: %s(0x%p).%s [%d]',[ErrorSourceObjectClass,Pointer(ErrorSourceObject),
                                                                                           ErrorSourceFunctionName,ErrorSourceFunctionIndex]))
                  else
                    meResultInfoText.Lines.Add(Format('       Function: %s [%d]',[ErrorSourceFunctionName,ErrorSourceFunctionIndex]));
                  meResultInfoText.Lines.Add('Exception class: ' + ExceptionClass);
                  meResultInfoText.Lines.Add(sLineBreak + 'Exception text: ');
                  meResultInfoText.Lines.Add(sLineBreak + '  ' + ExceptionText);
                end;
else
  lblResultCaption.Caption := 'Unknown state';
end;
ShowModal;
end;

//==============================================================================

procedure TfResultInfoForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
If Key = ^C then
  Clipboard.AsText := lblResultCaption.Caption + sLineBreak + sLineBreak +
                      StringOfChar('-',60)+ sLineBreak + sLineBreak +
                      meResultInfoText.Text;
end;

end.
