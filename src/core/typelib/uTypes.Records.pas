unit uTypes.Records;

interface

uses
  VDAPI,
  uTypes.Base,
  uStream,
  gRBTree;

type
  TRecordFieldInfo = record
    BitOffset: TVDBitSize;
    Info: TVDFieldInfo;
  end;

  TRecordFields = TRBTree<TRecordFieldInfo>;

  TVDRecordType = class(TVDTypeWithFields, IVDType, IVDRecordType)
  private
    FInheritedRecord: TVDRecordType;
    FFields: TRecordFields;
  protected
    function FindFieldByBitOffsetInternal(var BitOffset: TVDBitSize;
      FieldInfo: PVDFieldInfo): boolean; override;
  public
    constructor Create(Lib: IVDTypeLibrary; const Name: string; InheritedType: TVDRecordType = nil);
    destructor Destroy; override;
    property InheritedRecord: TVDRecordType read FInheritedRecord;
    property Fields: TRecordFields read FFields;

    procedure SaveToStream(AStream: TVDStreamIO); override;
    procedure LoadFromStream(AStream: TVDStreamIO); override;
  public
    { VDAPI }
    function GetInherited: IVDRecordType; stdcall;
    procedure AddField(FieldName: BSTR_IN; FieldType: IVDType;
      Comment: BSTR_IN = nil); stdcall;
    procedure AddGap(BitSize: TVDBitSize); stdcall;
    procedure EnumFields(EnumFunc: TRecordFieldEnumFunc; ud: Pointer); stdcall;
  end;

implementation

{ TRecordType }

procedure TVDRecordType.AddField(FieldName: BSTR_IN; FieldType: IVDType;
  Comment: BSTR_IN);
var
  Field: TRecordFieldInfo;
begin
  Field.BitOffset := FBitSize;
  Field.Info.Name := FieldName;
  Field.Info.Comment := Comment;
  Field.Info.&Type := FieldType as TVDType;
  FFields.Add(Field);
  inc(FBitSize, FieldType.BitSize);
end;

procedure TVDRecordType.AddGap(BitSize: TVDBitSize);
begin
  inc(FBitSize, BitSize);
end;

constructor TVDRecordType.Create(Lib: IVDTypeLibrary; const Name: string; InheritedType: TVDRecordType);
begin
  if InheritedType <> nil then
    inherited Create(Lib, Name, InheritedType.BitSize)
  else
    inherited Create(Lib, Name);
  FTypeKind := TYPEKIND_RECORD;
  FInheritedRecord := InheritedType;
  FFields := TRecordFields.Create(
    function(const a, b: TRecordFieldInfo): boolean
    begin
      Result := a.BitOffset < b.BitOffset;
    end);
end;

destructor TVDRecordType.Destroy;
begin
  FFields.Free;
  inherited;
end;

procedure TVDRecordType.EnumFields(EnumFunc: TRecordFieldEnumFunc; ud: Pointer);
var
  FieldInfo: TRecordFieldInfo;
begin
  if Assigned(EnumFunc) then
    if FFields <> nil then
      for FieldInfo in FFields do
      begin
        if not EnumFunc(
          FieldInfo.BitOffset,
          FieldInfo.Info.Name,
          FieldInfo.Info.Comment,
          FieldInfo.Info.&Type, ud) then
          break;
      end;
end;

function TVDRecordType.FindFieldByBitOffsetInternal(var BitOffset: TVDBitSize;
FieldInfo: PVDFieldInfo): boolean;
var
  Key: TRecordFieldInfo;
  prv, cur, nxt: TRecordFields.TRBNodePtr;
begin
  if FFields.Count = 0 then
    Exit(false);
  // In inherited record?
  if (BitOffset < FFields.First^.K.BitOffset) then
  begin
    if FInheritedRecord <> nil then
      Exit(FInheritedRecord.FindFieldByBitOffsetInternal(BitOffset, FieldInfo))
    else
      Exit(false);
  end;
  // In this record.
  Key.BitOffset := BitOffset;
  // Find field.
  FFields.FindEx(Key, prv, cur, nxt);
  if cur = nil then
    cur := prv; // try prv
  if cur = nil then
    Exit(false); // not found
  // BitOffset >= cur^.K.BitOffset
  if BitOffset >= (cur^.K.BitOffset + cur^.K.Info.&Type.BitSize) then
    Exit(false);
  // Found.
  BitOffset := cur^.K.BitOffset;
  if Assigned(FieldInfo) then
    FieldInfo^ := cur^.K.Info;
  Exit(True);
end;

function TVDRecordType.GetInherited: IVDRecordType;
begin
  Result := FInheritedRecord;
end;

procedure TVDRecordType.LoadFromStream(AStream: TVDStreamIO);
var
  cnt, u: uint32;
  Key: TRecordFieldInfo;
  // bsize: uint32;
  inheritedtypename, typename: string;
  Lib: IVDTypeLibrary;
begin
  inherited;

  Lib := IVDTypeLibrary(FTypeLib);

  inheritedtypename := AStream.ReadStr(); // inherited typename
  if inheritedtypename <> '' then
    FInheritedRecord := Lib.FindType(nil, BSTR_IN(inheritedtypename), True) as TVDRecordType;

  // field count
  cnt := AStream.ReadU32;
  // fields
  for u := 1 to cnt do
  begin
    Key.BitOffset := AStream.ReadU32;      // bitoffset
    { bsize := } AStream.ReadU32;          // bitsize
    Key.Info.Name := AStream.ReadStr();    // name
    Key.Info.Comment := AStream.ReadStr(); // comment
    typename := AStream.ReadStr();         // typename

    Key.Info.&Type := Lib.FindType(nil, BSTR_IN(typename), True) as TVDType;

    // add
    FFields.Add(Key);
  end;
end;

procedure TVDRecordType.SaveToStream(AStream: TVDStreamIO);
var
  cnt, bofs, bsize: uint32;
  rec: TRecordFieldInfo;
begin
  inherited;
  // inherited type name
  if FInheritedRecord = nil then
    AStream.WriteStr('')
  else
    AStream.WriteStr(FInheritedRecord.Name);
  // field count
  cnt := FFields.Count;
  AStream.WriteU32(cnt);
  // fields
  if cnt <> 0 then
  begin
    for rec in FFields do
    begin
      bofs := rec.BitOffset;
      bsize := rec.Info.&Type.BitSize;
      AStream.WriteU32(bofs);                // bitoffset
      AStream.WriteU32(bsize);               // bitsize
      AStream.WriteStr(rec.Info.Name);       // name
      AStream.WriteStr(rec.Info.Comment);    // comment
      AStream.WriteStr(rec.Info.&Type.Name); // typename
    end;
  end;
end;

end.
