program libmedianadelphi;

{$APPTYPE CONSOLE}

{$R *.res}


uses
  System.SysUtils,
  mediana in 'mediana.pas';

procedure asm1;
asm
  mov eax, $123
end;

procedure main;
var
  ins: TINSTRUCTION;
  params: TDISASM_PARAMS;
  size: Integer;
begin
  size := medi_disassemble_default(@asm1, 10, 0, ins, params, 32);
end;

begin
  try
    main;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;

end.
