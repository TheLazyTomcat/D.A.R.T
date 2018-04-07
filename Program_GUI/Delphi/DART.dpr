program DART;

uses
  Forms,
  MainForm in '..\MainForm.pas' {fMainForm},
  ResultInfoForm in '..\ResultInfoForm.pas' {fResultInfoForm},
  ProcSettingsForm in '..\ProcSettingsForm.pas' {fProcSettingsForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.CreateForm(TfProcSettingsForm, fProcSettingsForm);
  Application.Run;
end.
