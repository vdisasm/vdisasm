{
  *  Supported types.
  *
  *  Each type should belong to some type library (e.g. std.s32 - signed 32
  *  from std typelib).
  *
  *  FTypeLib is TObject to avoid circular refernces to uTypes.Lib.
}
unit uTypes.Base;

interface

uses
  System.Generics.Collections,
  System.SysUtils, // raise
  gRBTree,
  uStream,
  uTypes.Kind,
  VDAPI;

type
  // ---------------------------------------------------------------------------
  // Base type.
  TVDType = class(TInterfacedObject, IVDType)
  protected
    FTypeLib: Pointer;      // [weak] IVDTypeLibrary. type library where this type is located.
    FTypeKind: TVDTypeKind; //
    FName: string;          // optional, can be unnamed
    FComment: string;       // optional
    FBitSize: TVDBitSize;   // fixed size for this type
    procedure SetTypeLib(const Value: IVDTypeLibrary);
  public
    constructor Create(Lib: IVDTypeLibrary; const Name: string; BitSize: integer = 0);
    // constructor CreateDummy(Kind: TVDTypeKind);

    // Save/load type to stream. Processes base info, override and call inherited.
    // Owner typelib must be assigned before calling these functions.
    procedure SaveToStream(AStream: TVDStreamIO); virtual;
    procedure LoadFromStream(AStream: TVDStreamIO); virtual;

    // Create type from stream (read kind, name and data).
    class function CreateFromStream(const Name: string;
      AStream: TVDStreamIO; const OwnerLib: IVDTypeLibrary): TVDType; virtual;

    property Kind: TVDTypeKind read FTypeKind;
  public
    { VDAPI }
    function GetTypeLib: IVDTypeLibrary; stdcall;
    function GetKind: TVDTypeKind; stdcall;
    function GetName: BSTR; stdcall;
    procedure SetName(Value: BSTR); stdcall;
    // function GetFullName: BSTR; stdcall; // TypeLibName + Name
    function GetComment: BSTR; stdcall;
    procedure SetComment(Value: BSTR); stdcall;
    function GetBitsize: TVDBitSize; stdcall;

    property TypeLib: IVDTypeLibrary read GetTypeLib write SetTypeLib;
    property Name: string read FName;
    property Comment: string read FComment write FComment;
    property BitSize: TVDBitSize read FBitSize write FBitSize;
  end;

  // ---------------------------------------------------------------------------
  // TVDEnumType = class(TVDType)
  // private
  // end;

  // ---------------------------------------------------------------------------
  TVDSimpleType = class(TVDType)
  end;

  // ---------------------------------------------------------------------------
  // Signed or Unsigned Integer.
  TVDIntType = class(TVDSimpleType)
  private
    FSigned: boolean;
  public
    constructor Create(Lib: IVDTypeLibrary; const Name: string; Signed: boolean; BitSize: integer);
    procedure SaveToStream(AStream: TVDStreamIO); override;
    procedure LoadFromStream(AStream: TVDStreamIO); override;
  end;

  // ---------------------------------------------------------------------------
  // Float types.
  TVDFloatType = class(TVDSimpleType)
    constructor Create(Lib: IVDTypeLibrary; const Name: string; BitSize: integer);
  end;

  // ---------------------------------------------------------------------------
  // Character types.
  // TVDCharType = class(TVDType)
  // end;

  // TVDWideCharType = class(TVDType)
  // end;

  // TVDStringType = class(TVDSimpleType)
  // end;

  // ---------------------------------------------------------------------------
  TVDTypeWithFields = class(TVDType)
  protected
    function FindFieldByBitOffsetInternal(var BitOffset: TVDBitSize;
      FieldInfo: PVDFieldInfo): boolean; virtual; abstract;
  public
    // BitOffset:
    // [in]  offset to search for field.
    // [out] start offset of found field.
    // Result show if field was found. If found Info is filled.
    function FindFieldByBitOffset(BitOffset: PVDBitSize; Info: PVDFieldInfo): BOOL; stdcall;
  end;

implementation

uses
  uTypes.Arrays,
  uTypes.Records;

{ TIntType }

constructor TVDIntType.Create(Lib: IVDTypeLibrary; const Name: string; Signed: boolean;
  BitSize: integer);
begin
  inherited Create(Lib, Name, BitSize);
  FTypeKind := TYPEKIND_INT;
  FSigned := Signed;
end;

{ TVDType }

constructor TVDType.Create(Lib: IVDTypeLibrary; const Name: string; BitSize: integer);
begin
  inherited Create;
  FTypeLib := Pointer(Lib);
  FName := IVDTypeLibrary(FTypeLib).GetName + '.' + Name;
  FBitSize := BitSize;
end;

function TVDType.GetBitsize: TVDBitSize;
begin
  Result := FBitSize;
end;

function TVDType.GetComment: BSTR;
begin
  Result := FComment;
end;

function TVDType.GetKind: TVDTypeKind;
begin
  Result := FTypeKind;
end;

function TVDType.GetName: BSTR;
begin
  Result := FName;
end;

function TVDType.GetTypeLib: IVDTypeLibrary;
begin
  Result := IVDTypeLibrary(FTypeLib);
end;

procedure TVDType.LoadFromStream(AStream: TVDStreamIO);
begin
  // override
end;

class function TVDType.CreateFromStream(const Name: string;
  AStream: TVDStreamIO; const OwnerLib: IVDTypeLibrary): TVDType;
var
  Comment: string;
  BitSize: TVDBitSize;
  Kind: TVDTypeKind;
begin
  // Result := nil;

  Kind := TVDTypeKind(AStream.ReadU8);
  // Name := AStream.ReadStr;

  Comment := AStream.ReadStr;
  BitSize := AStream.ReadU32;

  // Create type to support needed interface.
  case Kind of
    TYPEKIND_INT:
      Result := TVDIntType.Create(OwnerLib, '', false, BitSize);
    TYPEKIND_Float:
      Result := TVDFloatType.Create(OwnerLib, '', BitSize);
    TYPEKIND_Array:
      Result := TVDArrayType.Create(OwnerLib, '');
    TYPEKIND_Record:
      Result := TVDRecordType.Create(OwnerLib, '');
  else
    raise Exception.Create('Unsupported type kind.');
  end;

  Result.FTypeLib := Pointer(OwnerLib);

  Result.LoadFromStream(AStream);

  if Result <> nil then
  begin
    Result.FTypeKind := Kind;
    // Result.FTypeLib := Pointer(OwnerLib);
    Result.FName := Name;
    Result.Comment := Comment;
    Result.BitSize := BitSize;
  end;
end;

procedure TVDType.SaveToStream(AStream: TVDStreamIO);
begin
  // Name should be saved in header.
  // AStream.WriteStr(FName);

  AStream.WriteStr(FComment);
  AStream.WriteU32(FBitSize);
end;

procedure TVDType.SetComment(Value: BSTR);
begin
  FComment := Value;
end;

procedure TVDType.SetName(Value: BSTR);
begin
  if FName <> Value then
  begin
    FName := Value;
    if FTypeLib <> nil then
      IVDTypeLibrary(FTypeLib).MakeDirty;
  end;
end;

procedure TVDType.SetTypeLib(const Value: IVDTypeLibrary);
begin
  FTypeLib := Pointer(Value);
end;

{ TTypeWithFields }

function TVDTypeWithFields.FindFieldByBitOffset(BitOffset: PVDBitSize;
  Info: PVDFieldInfo): BOOL;
begin
  if Assigned(BitOffset) then
    Result := FindFieldByBitOffsetInternal(BitOffset^, Info)
  else
    Result := false;
end;

procedure TVDIntType.LoadFromStream(AStream: TVDStreamIO);
var
  flags: byte;
begin
  inherited;
  flags := AStream.ReadU8;
  FSigned := (flags and 1) <> 0;
end;

procedure TVDIntType.SaveToStream(AStream: TVDStreamIO);
var
  flags: byte;
begin
  inherited;
  flags := 0;
  if FSigned then
    flags := flags or 1;
  AStream.WriteU8(flags);
end;

{ TVDFloatType }

constructor TVDFloatType.Create(Lib: IVDTypeLibrary; const Name: string;
  BitSize: integer);
begin
  inherited Create(Lib, Name, BitSize);
  FTypeKind := TYPEKIND_Float;
end;

end.
