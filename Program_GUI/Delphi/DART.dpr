program DART;

uses
  Forms,
  MainForm in '..\MainForm.pas' {fMainForm},
  ResultInfoForm in '..\ResultInfoForm.pas' {fResultInfoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.CreateForm(TfResultInfoForm, fResultInfoForm);
  Application.Run;
end.
