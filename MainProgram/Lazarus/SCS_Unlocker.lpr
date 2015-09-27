program SCS_Unlocker;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  SysUtils,
  Forms,

  Repairer,
  FilesManager,

  MainForm,
  PrcsSettingsForm,
  ErrorForm;

{$R *.res}

begin
{$IFDEF Debug}
  If FileExists(ExtractFilePath(ParamStr(0)) + 'heap.trc') then
    DeleteFile(ExtractFilePath(ParamStr(0)) + 'heap.trc');
  SetHeapTraceOutput(ExtractFilePath(ParamStr(0)) + 'heap.trc');
{$ENDIF}
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfErrorForm, fErrorForm);
  Application.CreateForm(TfPrcsSettingsForm, fPrcsSettingsForm);
  Application.Run;
end.

