{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
unit ErrorForm;

{$IFDEF FPC}{$MODE Delphi}{$ENDIF}

interface

uses
  SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  FilesManager;

type
  TfErrorForm = class(TForm)
    lblFileName: TLabel;
    lblFileSize: TLabel;    
    bvlHorSplit: TBevel;
    lblText: TLabel;
    grbTechnical: TGroupBox;
    lblObject_l: TLabel;
    lblObject: TLabel;
    lblMethod_l: TLabel;
    lblMethod: TLabel;
    lblThread_l: TLabel;
    lblThread: TLabel;
    lblExceptionClass_l: TLabel;
    lblExceptionClass: TLabel;
    lblHint: TLabel;
    procedure FormKeyPress(Sender: TObject; var Key: Char);
  private
    { Private declarations }
  public
    procedure ShowErrorInformation(const FileInfo: TFileListItem);
  end;

var
  fErrorForm: TfErrorForm;

implementation

{$IFDEF FPC}
  {$R *.lfm}
{$ELSE}
  {$R *.dfm}
{$ENDIF}  

uses
  ClipBrd;

procedure TfErrorForm.ShowErrorInformation(const FileInfo: TFileListItem);
begin
lblFileName.Caption := FileInfo.Name;
lblFileSize.Caption := IntToStr(FileInfo.Size) + ' bytes';
lblFileName.ShowHint := Canvas.TextWidth(lblFileName.Caption) > lblFileName.Width;
If lblFileName.ShowHint then
  lblFileName.Hint := lblFileName.Caption;
lblText.Caption := FileInfo.ErrorInfo.Text;
lblObject.Caption := Format('%s (0x%p)',[FileInfo.ErrorInfo.SourceClass,FileInfo.ErrorInfo.Source]);
lblMethod.Caption := Format('[%d] %s',[FileInfo.ErrorInfo.MethodIdx,FileInfo.ErrorInfo.MethodName]);
lblThread.Caption := IntToStr(FileInfo.ErrorInfo.ThreadID);
lblExceptionClass.Caption := FileInfo.ErrorInfo.ExceptionClass;
ShowModal;
end;

//==============================================================================

procedure TfErrorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
If Key = ^C then
  begin
    Clipboard.AsText :=
      lblFileName.Caption + sLineBreak +
      lblFileSize.Caption + sLineBreak +
      lblText.Caption + sLineBreak +
      lblObject.Caption + sLineBreak +
      lblMethod.Caption + sLineBreak +
      lblThread.Caption + sLineBreak +
      lblExceptionClass.Caption;
  end;
end;

end.
