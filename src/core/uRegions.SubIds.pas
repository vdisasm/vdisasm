{
  SubId:
  SUBID_TYPE_UID: TTypeUId, this region is typed bytes
  SUBID_STRING_CODEPAGE: UInt16, this region is string
}
unit uRegions.SubIds;

interface

uses
  VDAPI;

const
  REGION_SUBID_TYPE_UID = 1;
  REGION_SUBID_STRING   = 2;

  { REGION_SUBID_UID }

procedure Region_Write_UID(const io: IVDStreamIO;
  UID: TTypeUID); inline;
function Region_Parse_UID(const io: IVDStreamIO;
  out UID: TTypeUID): boolean; inline;

{ REGION_SUBID_STRING }

procedure Region_Write_String(const io: IVDStreamIO;
  CodePage: TCodePage); inline;
function Region_Parse_String(const io: IVDStreamIO;
  out CodePage: TCodePage): boolean; inline;

implementation

uses
  uStream;

{ REGION_SUBID_UID }

procedure Region_Write_UID;
begin
  io.WriteWord(SizeOf(TTypeUID), UID);
end;

function Region_Parse_UID;
begin
  UID := io.ReadWord(SizeOf(TTypeUID));
  Result := True;
end;

{ REGION_SUBID_STRING }

procedure Region_Write_String;
begin
  io.WriteWord(SizeOf(CodePage), CodePage);
end;

function Region_Parse_String;
begin
  CodePage := io.ReadWord(SizeOf(CodePage));
  Result := True;
end;

end.
