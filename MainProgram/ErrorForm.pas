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
  Repairer;

type
  TfErrorForm = class(TForm)
    lblFileName: TLabel;
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
    procedure ShowErrorInformation(const FileName: String; ErrorInfo: TErrorInfo);
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

procedure TfErrorForm.ShowErrorInformation(const FileName: String; ErrorInfo: TErrorInfo);
begin
lblFileName.Caption := FileName;
lblText.Caption := ErrorInfo.Text;
lblObject.Caption := Format('%s (0x%p)',[ErrorInfo.SourceClass,ErrorInfo.Source]);
lblMethod.Caption := Format('[%d] %s',[ErrorInfo.MethodIdx,ErrorInfo.MethodName]);
lblThread.Caption := IntToStr(ErrorInfo.ThreadID);
lblExceptionClass.Caption := ErrorInfo.ExceptionClass;
ShowModal;
end;

//==============================================================================

procedure TfErrorForm.FormKeyPress(Sender: TObject; var Key: Char);
begin
If Key = ^C then
  begin
    Clipboard.AsText := lblFileName.Caption + sLineBreak + lblText.Caption + sLineBreak +
       lblObject.Caption + sLineBreak + lblMethod.Caption + sLineBreak +
       lblThread.Caption + sLineBreak + lblExceptionClass.Caption;
  end;
end;

end.
