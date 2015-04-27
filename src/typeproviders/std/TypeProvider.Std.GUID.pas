unit TypeProvider.Std.GUID;

interface

uses
  VDAPI;

type
  TTP_GUID = class(TInterfacedObject, IVDTypeProviderBase)
  public
    function Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
    function Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
  end;

implementation

uses
  System.SysUtils;

{ TTypeProvider_GUID }

function TTP_GUID.Decode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T; stdcall;
var
  GUID: TGUID;
begin
  Result := CoreGet().VM.Read(VA, @GUID, 16);
  if Result <> 16 then
    exit(0);
  case Kind of
    TVDDataEncoding.Text:
      IVDVATextLayout(Param).AddText(BSTR_IN(GUIDToString(GUID)), TTag.TAGID_NUMBER);
  end;
end;

function TTP_GUID.Encode(VA: TVA; Kind: TVDDataEncoding; Param: Pointer): SIZE_T;
begin
  Result := SIZE_T(-1);

  // not supported, let user modify it as structure
end;

end.
