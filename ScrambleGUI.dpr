program ScrambleGUI;

uses
  System.StartUpCopy,
  FMX.Forms,
  Scramble in 'Scramble.pas' {S};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TS, S);
  Application.Run;
end.
