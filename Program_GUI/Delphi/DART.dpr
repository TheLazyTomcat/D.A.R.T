program DART;

uses
  Forms,
  MainForm in '..\MainForm.pas' {fMainForm};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'D.A.R.T';
  Application.CreateForm(TfMainForm, fMainForm);
  Application.Run;
end.
