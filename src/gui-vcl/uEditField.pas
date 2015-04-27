unit uEditField;

interface

uses
  System.SysUtils,
  VDAPI;

function TryEditFieldText(VA: TVA; BitOfs: Byte; const TypeName: string): boolean;

implementation

uses
  uCore.Strings,
  uFormTextInput;

function TryEditFieldText(VA: TVA; BitOfs: Byte; const TypeName: string): boolean;
var
  c: IVDCore;
  Provider: IVDTypeProviderBase;
  TextLayout: IVDVATextLayout;
  Text, Caption: string;
begin
  result := false;

  c := CoreGet;

  // get provider
  Provider := c.TypeMgr.GetProvider(BSTR_IN(TypeName));
  if not Assigned(Provider) then
    exit;

  // can encode?
  if Provider.Encode(BAD_VA, TVDDataEncoding.Text, nil) = VDAPI.SIZE_T(-1) then
  begin
    c.Log.WriteLn(Format(STypeDoesntSupportEncoding, [TypeName]));
    exit;
  end;

  // get existing value
  TextLayout := CreateVATextLayout(TVDTextFlag.Plain);
  if Provider.Decode(VA, TVDDataEncoding.Text, Pointer(TextLayout)) = 0 then
    exit;
  Text := TextLayout.Get;

  // try get new value
  Caption := Format('%s (%s)', [SModifyData, TypeName]);
  while InputText(Text, Caption) do
  begin
    if Text = '' then
      continue;

    case Provider.Encode(VA, TVDDataEncoding.Text, BSTR_IN(Text)) of
      VDAPI.SIZE_T(-1):
        break; // unsupported, shouldn't happen
      0:
        c.Log.WriteLn(SEncodingFailed); // failed, try again
    else
      result := true;
      break; // encoded, ok
    end;
  end;
end;

end.
