unit uSections.Utils;

interface

uses
  System.SysUtils,
  VDAPI,
  uDB,
  uStream,
  uStream.MemoryStream;

type
  TSectionKeyType = (skLastId, skData);

function VAToHexDigitCount(VA: TVA): Int; inline;

procedure CreateSectionKey(Kind: TSectionKeyType; pID: PVDSectionId; out Key: TBytes);

function FetchSectionLastId: TVDSectionId;
function FetchSectionNewId: TVDSectionId;

function SectionIsFlushable(const Sec: IVDSection): boolean; {$IFNDEF DEBUG}inline; {$ENDIF}
function SectionIsSaveable(const Sec: IVDSection): boolean; {$IFNDEF DEBUG}inline; {$ENDIF}

function FetchSectionNewDebugId: TVDSectionId;
procedure ResetSectionNewDebugId;

implementation

uses
  BPlusTree.Intf,
  uCore;

function VAToHexDigitCount(VA: TVA): Int; inline;
begin
  if VA = 0 then
    exit(1);
  result := 0;
  while VA <> 0 do
  begin
    inc(result);
    VA := VA shr 4;
  end;
end;

procedure CreateSectionKey(Kind: TSectionKeyType; pID: PVDSectionId; out Key: TBytes);
var
  s: TVDStreamIO;
begin
  s := TVDStreamIO.Create(TVDMemoryStream.Create());
  try
    // key
    s.WriteTag(DBTAG_Section);
    case Kind of
      skLastId:
        s.WriteU8(DBTAG_SECTION_LASTID);
      skData:
        s.WriteU8(DBTAG_SECTION_DATA);
    end;
    // id
    if Assigned(pID) then
      s.WriteWord(SizeOf(TVDSectionId), pID^);
    Key := s.ToBytes;
  finally
    s.Free;
  end;
end;

function ReadSectionLastId(out ID: TVDSectionId): boolean;
var
  Key, Value: TBytes;
  s: TVDStreamIO;
begin
  CreateSectionKey(skLastId, nil, Key);
  result := TVDCore(CoreGet()).DB.Get(Key, Value) = BP_OK;
  if not result then
    exit;
  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Value));
  try
    ID := s.ReadWord(SizeOf(TVDSectionId));
  finally
    s.Free;
  end;
end;

function StoreSectionLastId(ID: TVDSectionId): boolean;
var
  Key: TBytes;
  Value: TVDStreamIO;
begin
  CreateSectionKey(skLastId, nil, Key);
  Value := TVDStreamIO.Create(TVDMemoryStream.Create());
  try
    Value.WriteWord(SizeOf(TVDSectionId), ID);
    result := TVDCore(CoreGet()).DB.Put(Key, Value.ToBytes) = BP_OK;
  finally
    Value.Free;
  end;
end;

function FetchSectionLastId: TVDSectionId;
begin
  if not ReadSectionLastId(result) then
    result := 0;
end;

function FetchSectionNewId: TVDSectionId;
begin
  result := FetchSectionLastId() + 1;
  if not StoreSectionLastId(result) then
    raise Exception.Create('failed to store section id');
end;

// -----------------------------------------------------------------------------
function SectionIsFlushable(const Sec: IVDSection): boolean;
begin
  // We can flush
  // - non-debugger sections
  result := (Sec.GetFlags and TVDSectionFlag.Debugger) = 0;
end;

function SectionIsSaveable(const Sec: IVDSection): boolean; {$IFNDEF DEBUG}inline; {$ENDIF}
begin
  Result := SectionIsFlushable(Sec);
end;

// -----------------------------------------------------------------------------
const
  LOW_DBG_SEC_ID = $F0000000;

var
  LastDebugSectionId: TVDSectionId = LOW_DBG_SEC_ID;

function FetchSectionNewDebugId: TVDSectionId;
begin
  result := LastDebugSectionId;
  inc(LastDebugSectionId);
end;

procedure ResetSectionNewDebugId;
begin
  LastDebugSectionId := LOW_DBG_SEC_ID;
end;
// -----------------------------------------------------------------------------

end.
