unit uEncodingAdapter;

interface

uses
  System.SysUtils,
  VDAPI;

function GetEncodingByCodepage(CodePage: TCodePage): TEncoding;

implementation

{
  see code page identifiers
  http://msdn.microsoft.com/en-us/library/bb643325.aspx
}

function GetEncodingByCodepage(CodePage: TCodePage): TEncoding;
begin
  case CodePage of
    1200, // UTF-16LE
    1201: // UTF-16BE
      Result := TUnicodeEncoding.Create;
  else
    Result := TMBCSEncoding.Create(CodePage);
  end;
end;

end.
