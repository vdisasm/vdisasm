program typesproj;

{$apptype console}


uses
  uTypes.Base in 'uTypes.Base.pas',
  uTypes.Lib in 'uTypes.Lib.pas',
  uTypes.Print in 'uTypes.Print.pas';

var
  tl: TVDTypeLibrary;
  s32: TVDType;
  trect, trect2: TRecordType;
  arr1: TArrayType;

procedure main;
var
  Info: TFieldInfo;
  bitofs: TVDBitSize;
begin
  tl := TVDTypeLibrary.Create;
  try
    s32 := tl.CreateSimpleTypeFromString('s32');

    trect := tl.CreateRecord('TRect');
    trect.Comment := 'Rectange';
    trect.AddField('x', s32, 'left');
    trect.AddField('y', s32, 'top');
    trect.AddField('w', s32, 'width');
    trect.AddField('h', s32, 'height');

    arr1 := tl.CreateArray2('TArr1', s32, 10);
    arr1.Comment := 'Sample array';

    trect2 := tl.CreateRecord('TRect2', trect);
    trect2.Comment := 'Extended TRect';
    trect2.AddField('arr', arr1, 'array');
    trect2.AddField('area', s32, 'Rect simple area');

    bitofs := 168;
    trect2.FindFieldByBitOffset(bitofs, Info);

    writeln(PrintType(trect));
    writeln(PrintType(trect2));
    writeln(PrintType(arr1));
  finally
    tl.Free;
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := true;
  main;

  readln;

end.
