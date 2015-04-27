unit uDumpDBText;

interface

uses
  VDAPI;

procedure DumpDatabaseText(C: IVDCore; VA0, VA1: TVA; const FileName: string);

implementation

uses
  System.Diagnostics,
  uCore.Strings,
  vdBufferedStream;

procedure DumpDatabaseText;
var
  f: TBufferedTextFileStream;
  curVA, tmpVA: TVA;
  Layout: IVDVATextLayout;
  inp: IVDInputFile;
  TotalMem: uint64;
  Stage, Done: uint32;
  sw: TStopwatch;
  i: integer;
begin
  if VA0 > VA1 then
    exit;

  sw := TStopwatch.StartNew;
  f := TBufferedTextFileStream.Create(FileName);
  try
    C.Log.WriteLn('Dumping database text');
    C.Log.WriteLn('0................100'); // 20 dots

    TotalMem := C.VM.Sections.CalcContDelta(VA0, VA1 + 1);
    Stage := TotalMem div 20;
    Done := 0;

    inp := C.InputFile;

    f.WriteLn('VDisAsm dumped database text');
    f.WriteLn(SVDisAsmUrl);
    f.WriteLn();
    f.WriteLn('Input file:  ' + inp.FileName);
    f.WriteLn('Working dir: ' + inp.WorkingDir);
    f.WriteLn('Parameters:  ' + inp.Params);
    f.WriteLn('Range: %x - %x', [VA0, VA1]);
    f.WriteLn();

    Layout := CreateVATextLayout(TVDTextFlag.Plain);

    curVA := VA0;
    while curVA <= VA1 do
    begin
      tmpVA := curVA;

      // Decode current VA lines to Layout.
      Layout.Clear;
      if C.Decoder.DecodeToText(tmpVA, Layout) <> 0 then
      begin
        // This linebreak to make last line be "line" too.
        Layout.LineBreak;
        for i := 0 to Layout.GetLineCount - 1 do
          f.WriteLn(Layout.GetLine(i));
      end;

      // Is end?
      if curVA = VA1 then
        break;

      if not C.Decoder.ItemStep(@curVA, 1) then
      begin
        C.Log.WriteLn('');
        // not complete
        C.Log.WriteLn('Not complete dump saved.');
        exit;
      end;

      inc(Done);
      if Done >= Stage then
      begin
        Done := 0;
        C.Log.Write('.');
      end;

    end;

    C.Log.WriteLn('');
    C.Log.WriteLn('Complete dump saved.');

  finally
    f.Free;
    sw.Stop;

    C.Log.WriteLn('Time elapsed: ' + string(sw.Elapsed));
  end;
end;

end.
