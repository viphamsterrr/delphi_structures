program mains;

uses
  System.StartUpCopy,
  main in 'main.pas',
  FMX.Forms,
  StraDat in 'StraDat.pas';

//{$R *.res}

begin

  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;

end.
