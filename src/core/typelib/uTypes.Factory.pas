{
  Factory for creating type-interfaces.
}
unit uTypes.Factory;

interface

uses
  VDAPI,
  uTypes.Base,
  uTypes.Lib,
  uTypes.Arrays,
  uTypes.Records;

type
  TVDTypeFactory = class(TInterfacedObject, IVDTypeFactory)
  private
    FLibClass: TVDTypeLibrary;
    function TryAddType(AType: TVDType): TVDType; inline;
  public
    constructor Create(Lib: IVDTypeLibrary);

    // Create signed or unsigned integer type.
    function CreateIntegerType(Name: BSTR_IN; Signed: BOOL;
      BitSize: int): IVDType; stdcall;

    function CreateFloatType(Name: BSTR_IN; BitSize: int): IVDType; stdcall;

    // Create simple type by string, like u32,s32.
    function CreateSimpleTypeFromString(Text: BSTR_IN): IVDType; stdcall;

    // Record.
    function CreateRecord(Name: BSTR_IN;
      InheritedRecord: IVDRecordType = nil): IVDRecordType; stdcall;

    // Array.
    function CreateArray1(Name: BSTR_IN; FieldType: IVDType;
      Lower, Upper: int): IVDArrayType; stdcall;
    function CreateArray2(Name: BSTR_IN; FieldType: IVDType;
      Count: int): IVDArrayType; stdcall;
  end;

implementation

{ TVDTypeFactory }

constructor TVDTypeFactory.Create(Lib: IVDTypeLibrary);
begin
  inherited Create;
  FLibClass := Lib as TVDTypeLibrary;
end;

function TVDTypeFactory.TryAddType(AType: TVDType): TVDType;
begin
  if FLibClass <> nil then
  begin
    FLibClass.AddType(AType, True);
  end;
  Result := AType;
end;

function TVDTypeFactory.CreateArray1(Name: BSTR_IN;
  FieldType: IVDType; Lower, Upper: int): IVDArrayType;
var
  typ: TVDArrayType;
begin
  typ := TVDArrayType.Create(FLibClass, Name, FieldType as TVDType, Lower, Upper);
  TryAddType(typ);
  Result := typ;
end;

function TVDTypeFactory.CreateArray2(Name: BSTR_IN; FieldType: IVDType;
  Count: int): IVDArrayType;
var
  typ: TVDArrayType;
begin
  typ := TVDArrayType.Create(FLibClass, Name, FieldType as TVDType, Count);
  TryAddType(typ);
  Result := typ;
end;

function TVDTypeFactory.CreateFloatType(Name: BSTR_IN; BitSize: int): IVDType;
begin
  Result := TryAddType(TVDFloatType.Create(FLibClass, Name, BitSize));
end;

function TVDTypeFactory.CreateIntegerType(Name: BSTR_IN; Signed: BOOL; BitSize: int): IVDType;
begin
  Result := TryAddType(TVDIntType.Create(FLibClass, Name, Signed, BitSize));
end;

function TVDTypeFactory.CreateRecord(Name: BSTR_IN;
  InheritedRecord: IVDRecordType): IVDRecordType;
var
  typ: TVDRecordType;
begin
  typ := TVDRecordType.Create(FLibClass, Name, InheritedRecord as TVDRecordType);
  TryAddType(typ);
  Result := typ;
end;

function TVDTypeFactory.CreateSimpleTypeFromString(Text: BSTR_IN): IVDType;
var
  Kind: char;
  s: string;
  bitness: integer;
  err: integer;
begin
  Result := nil;
  if Text = '' then
    Exit;
  Kind := Text[0];
  case Kind of
    's', 'u':
      begin
        s := Text;
        delete(s, 1, 1);
        val(s, bitness, err);
        if err = 0 then
        begin
          Result := TryAddType(TVDIntType.Create(FLibClass, Text, Kind = 's', bitness));
        end;
      end;
  end;
end;

// If Lib isn't nil, created types are added to the Lib.
function TypeFactoryCreate(Lib: IVDTypeLibrary): IVDTypeFactory; stdcall;
begin
  if Lib = nil then
    Result := nil
  else
    Result := TVDTypeFactory.Create(Lib);
end;

exports
  TypeFactoryCreate;

end.
