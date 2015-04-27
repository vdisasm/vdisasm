program BuildStdTypelib;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  uBuider in 'uBuider.pas';

begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF}
  {
    Must be launched in win32/win64 dir.
  }

  try
    ReportMemoryLeaksOnShutdown := True;
    TStdBuilder.CreateLib;
    // TStdBuilder.LoadLib;
  except
    on E: Exception do
      writeln(E.ClassName, ': ', E.Message);
  end;
end.
