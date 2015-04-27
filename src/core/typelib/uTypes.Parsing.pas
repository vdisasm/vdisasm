unit uTypes.Parsing;

interface

procedure FullTypeNameToTypeLibAndType(
  Full: string;
  out TypeLibName, TypeName: string);

implementation

uses
  System.SysUtils;

procedure FullTypeNameToTypeLibAndType(
  Full: string;
  out TypeLibName, TypeName: string);
var
  dotIndex: Integer;
begin
  dotIndex := Full.LastIndexOf('.');
  if dotIndex < 0 then
  begin
    TypeLibName := '';
    TypeName := Full;
    exit;
  end;
  TypeLibName := Full.Substring(0, dotIndex);
  TypeName := Full.Substring(dotIndex + 1);
end;

end.
