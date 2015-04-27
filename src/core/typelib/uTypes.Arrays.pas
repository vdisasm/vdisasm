unit uTypes.Arrays;

interface

uses
  System.SysUtils,
  VDAPI,
  uTypes.Base;

type
  TVDArrayType = class(TVDTypeWithFields, IVDArrayType)
  private
    FLowerBound, FUpperBound: integer;
    FFieldType: TVDType;
    function GetCount: integer; inline;
  protected
    function FindFieldByBitOffsetInternal(var BitOffset: TVDBitSize;
      FieldInfo: PVDFieldInfo): boolean; override;
  public
    constructor Create(Lib: IVDTypeLibrary; const Name: string; FieldType: TVDType; Lower, Upper: integer); overload;
    constructor Create(Lib: IVDTypeLibrary; const Name: string; FieldType: TVDType; Count: integer); overload;

    property LowerBound: integer read FLowerBound;
    property UpperBound: integer read FUpperBound;
    property FieldType: TVDType read FFieldType;
    property Count: integer read GetCount;
  end;

implementation

{ TArrayType }

constructor TVDArrayType.Create(Lib: IVDTypeLibrary; const Name: string; FieldType: TVDType; Lower,
  Upper: integer);
begin
  if Lower > Upper then
    raise Exception.Create('TArrayType bounds error.');
  inherited Create(Lib, Name);
  FTypeKind := TYPEKIND_ARRAY;
  FFieldType := FieldType;
  FLowerBound := Lower;
  FUpperBound := Upper;
  FBitSize := FieldType.BitSize * Count;
end;

constructor TVDArrayType.Create(Lib: IVDTypeLibrary; const Name: string;
  FieldType: TVDType; Count: integer);
begin
  Create(Lib, Name, FieldType, 0, Count - 1);
end;

function TVDArrayType.FindFieldByBitOffsetInternal(var BitOffset: TVDBitSize;
  FieldInfo: PVDFieldInfo): boolean;
var
  FieldIndex: uint32;
begin
  FieldIndex := BitOffset div FFieldType.BitSize;
  if FieldIndex < Count then
  begin
    BitOffset := FieldIndex * FFieldType.BitSize;
    if Assigned(FieldInfo) then
    begin
      FieldInfo.Name := '';
      FieldInfo.Comment := '';
      FieldInfo.&Type := FFieldType;
    end;
    Exit(True);
  end;
  Exit(false);
end;

function TVDArrayType.GetCount: integer;
begin
  Result := FUpperBound - FLowerBound + 1;
end;

end.
