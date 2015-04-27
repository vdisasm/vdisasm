unit uSections;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  gRBTree,
  BPlusTree.Intf,
  uStream,
  uStream.MemoryStream,
  uUpdateable,
  VDAPI,
  uCore.Strings;

type
  TVDSectionItems = TRBTree<IVDSection>;

  TSectionAction =
    (
    SecAct_None,
    SecAct_Added,
    SecAct_Removed,
    SecAct_Changed
    );

  TVDSections = class(TVDUpdateable, IVDSections)
  private
    FItemsByVA: TVDSectionItems;
    FItemsByID: TVDSectionItems; // secondary index.

    // Used for mapping contigious pos to VA.
    // The order is same as FItems (by va)
    FItemsByContVA: TVDSectionItems;

    FTotalSize: UInt64; // Size of all sections.
    FSaveSectionWhenAdded: boolean;

    procedure OnItemsNotify(Sender: TObject; const Item: IVDSection;
      Action: TCollectionNotification); inline;
    procedure DoSectionsChanged(const Sec: IVDSection; bSave: boolean; Action: TSectionAction);

    function AddEmptyInternal(Name: BSTR; Size: UInt64; VA: TVA;
      Flags: TVDSectionFlags; ID: TVDSectionId): IVDSection; stdcall;
    function AddFromMappedFileInternal(FileName, Name: BSTR; Offset: UInt64;
      Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags; ID: TVDSectionId): IVDSection;

    procedure UpdateTotalSize(Delta: UInt64; bAdd: boolean);

    procedure SafeNotifyChanged;

  private
    FNeedRebuildContigiousPositions: boolean;
    procedure RebuildContigiousPositions;
  public
    { in-core }
    constructor Create;
    destructor Destroy; override;

    property Items: TVDSectionItems read FItemsByVA;
  public
    // Number of digits for any address (as hex number).
    HexAddressDigitCount: integer;

    procedure Clear;

    // Save section to database (it should handle all kind of sections).
    function SaveSecToDB(const Sec: IVDSection): boolean;
    // Delete section from database.
    function DelSecFromDB(const Sec: IVDSection): boolean;
    // Load and create section from stream.
    function CreateSectionFromStream(const Stream: IVDMemoryStream): IVDSection;
    // Load and create section from bytes.
    function LoadFromBytes(const Bytes: TBytes): boolean;
    // Load and create all sections from DB.
    function LoadAllFromDB: boolean;

    // Try to add section if it is not duplicate. Result is true on success.
    // False can mean it's duplicate section.
    function AddInternal(const Sec: IVDSection; bNotify: boolean = True): boolean;

    function DelInternal(const Sec: IVDSection; bNotify: boolean = True): boolean;
    procedure ClearInternal;

    // Delete old section and re-add it with new VA.
    // Result is false if section was not found.
    function ModifySection(var Sec: IVDSection; NewVA: TVA): boolean;
  protected
    procedure DoEndUpdate; override;
  public

    { VDAPI }

    function GetCount: Int; stdcall;
    function GetFirst: IVDSection; stdcall;
    function GetLast: IVDSection; stdcall;

    // Add section w/o raw data.
    function AddEmpty(Name: BSTR; Size: UInt64; VA: TVA;
      Flags: TVDSectionFlags): IVDSection; stdcall;

    // Copy section into database and add it as mapped file.
    // If RawSize <> 0 then FileName and Offset  used to copy data.
    // Otherwise FileName and Offset are ignored and section is filled with zeros.
    function AddFromFile(FileName, Name: BSTR; Offset: UInt64;
      RawSize, VirtSize: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags)
      : IVDSection; stdcall;

    // Add section mapped directly to file.
    function AddFromMappedFile(FileName, Name: BSTR; Offset: UInt64;
      Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags): IVDSection; stdcall;

    // Add temp. debug section (deleted when debugger stopped).
    function AddDebugSection(Name: BSTR; Size: TVDSectionSize;
      VA: TVA; Flags: TVDSectionFlags): IVDSection; stdcall;

    procedure DeleteDebugSections;

    // Find section. Result is Nil if not found.
    function Find(VA: TVA): IVDSection; stdcall;

    function FindByID(ID: TVDSectionId): IVDSection; stdcall;

    // Find section and previous/next sections.
    procedure FindEx(VA: TVA; OutPrev, OutCur, OutNext: PVDSection); stdcall;

    // Map VA to contigious position in range MinPos .. MaxPos.
    function MapVAToContigiousPos(VA: TVA; MinPos, MaxPos: Int): Int; stdcall;

    // Map contigious position in range to VA.
    function MapContigiousPosToVA(Pos, MinPos, MaxPos: Int): TVA; stdcall;

    // Enumerate sections.
    procedure Enumerate(cb: TSectionEnumProc; ud: pointer); stdcall;

    // Relocate section.
    function Relocate(Section: IVDSection; NewVA: TVA): BOOL; stdcall;

    function CalcContDelta(VABegin, VAEnd: TVA): TVA; stdcall;

    // Save all section data modifications.
    procedure Flush; stdcall;

    procedure Delete(Sec: IVDSection); stdcall;

    function SaveToFile(Sec: IVDSection; Path: BSTR_IN): BOOL; stdcall;
  end;

function SectionIdToFileName(c: IVDCore; ID: TVDSectionId): string;

implementation

uses
  uCore,
  uDB,
  uEndianness,
  uSection,
  uSection.Debugger,
  uSection.FileMapped,
  uSections.Utils;

function CompareIVDSectionLess(const a, b: IVDSection): boolean;
begin
  Result := a.GetLastVA < b.GetStartVA;
end;

function CompareIVDSectionIDLess(const a, b: IVDSection): boolean;
begin
  Result := a.GetId < b.GetId;
end;

function CompareSecLessByContVA(const a, b: IVDSection): boolean;
var
  tA, tB: TVDSection;
begin
  tA := TVDSection(a);
  tB := TVDSection(b);
  Result := (tA.ContigiousPosition + tA.GetSize - 1) < tB.ContigiousPosition;
end;

{ TVDSections }

function SectionIdToFileName(c: IVDCore; ID: TVDSectionId): string;
begin
  Result := Format('%%db%%\sec_%x', [ID]);
end;

// -----------------------------------------------------------------------------
function TVDSections.AddEmpty(Name: BSTR; Size: UInt64; VA: TVA;
  Flags: TVDSectionFlags): IVDSection;
begin
  Result := AddFromFile('', Name, 0, Size, Size, VA, Flags);
end;

function TVDSections.AddEmptyInternal(Name: BSTR; Size: UInt64; VA: TVA;
  Flags: TVDSectionFlags; ID: TVDSectionId): IVDSection;
begin
  Result := TVDSection.Create(VA, Size, ID, Flags, Name);
  AddInternal(Result);
end;
// -----------------------------------------------------------------------------

function TVDSections.AddFromFile(FileName, Name: BSTR; Offset: UInt64; RawSize,
  VirtSize: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags): IVDSection;
var
  c: IVDCore;
  io: IVDIO;
  DstFileName: string;
  ID: TVDSectionId;
  bCopied: boolean;
begin
  Result := nil;
  c := CoreGet;
  io := IOGet;
  ID := FetchSectionNewId();
  DstFileName := SectionIdToFileName(c, ID);

  if (RawSize <> 0) and (FileName <> '') then
    // Copy file into database.
    bCopied := io.Copy(FileName, DstFileName, Offset, RawSize, VirtSize)
  else
    // No raw data, create empty file.
    bCopied := io.CreateEmptyFile(DstFileName, VirtSize);

  if bCopied then
    // Add mapped section from copied file.
    Result := AddFromMappedFileInternal(DstFileName, Name, 0, VirtSize, VA, Flags, ID);
end;

function TVDSections.AddFromMappedFile(FileName, Name: BSTR; Offset: UInt64;
  Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags): IVDSection;
begin
  Result := AddFromMappedFileInternal(FileName, Name, Offset, Size, VA, Flags, FetchSectionNewId());
end;

function TVDSections.AddFromMappedFileInternal(FileName, Name: BSTR;
  Offset: UInt64; Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags;
  ID: TVDSectionId): IVDSection;
begin
  // Create mapped section.
  Result := TVDFileMappedSection.Create(FileName, Name, 0, Size, Size, VA, ID, Flags);
  AddInternal(Result);
end;

// -----------------------------------------------------------------------------
function TVDSections.AddDebugSection(Name: BSTR; Size: TVDSectionSize; VA: TVA;
  Flags: TVDSectionFlags): IVDSection;
begin
  Result := TVDDebuggerSection.Create(VA, Size, FetchSectionNewDebugId, Flags or TVDSectionFlag.Debugger, Name);
  AddInternal(Result);
end;

procedure TVDSections.DeleteDebugSections;
var
  list: TList<IVDSection>;
  Sec: IVDSection;
  i: integer;
begin
  list := TList<IVDSection>.Create;
  try
    // Make list of debugger sections.
    for Sec in FItemsByVA do
      if (Sec.GetFlags and TVDSectionFlag.Debugger) <> 0 then
        list.add(Sec);

    // Delete debugger sections.
    for i := 0 to list.Count - 1 do
    begin
      self.Delete(list[i]);
      list[i] := nil;
    end;

    ResetSectionNewDebugId;
  finally
    list.Free;
  end;
end;

// -----------------------------------------------------------------------------
function TVDSections.AddInternal(const Sec: IVDSection; bNotify: boolean): boolean;
var
  Present: TVDSectionItems.TRBNodePtr;
  c: IVDCore;
begin
  c := CoreGet();

  if Sec.GetSize = 0 then
    raise Exception.Create('Attempt to add zero-sized section.');

  Present := self.FItemsByVA.Find(Sec);
  if Assigned(Present) then
  begin
    c.Log.WriteLn(
      Format('*** Section "%s" at 0x%x conflicts with #%d "%s" at 0x%x. Deleting existing.',
      [Sec.GetName, Sec.GetStartVA,
      Present.K.GetId, Present.K.GetName, Present.K.GetStartVA]));

    Delete(Present.K);
  end;

  FItemsByVA.add(Sec, bNotify);
  FItemsByID.add(Sec, false);

  c.VM.CacheInvalidate;

  exit(True);
end;

// -----------------------------------------------------------------------------
procedure TVDSections.Delete(Sec: IVDSection);
begin
  if not Assigned(Sec) then
    exit;

  DelInternal(Sec, True);
end;

function TVDSections.DelInternal(const Sec: IVDSection; bNotify: boolean): boolean;
var
  p1, p2, p3: TVDSectionItems.TRBNodePtr;
begin
  if not Assigned(Sec) then
    exit(false);

  p1 := FItemsByVA.Find(Sec);
  p2 := FItemsByID.Find(Sec);
  p3 := FItemsByContVA.Find(Sec);
  if (p1 = nil) and (p2 = nil) then
    exit(false); // nothing to delete
  if (p1 <> nil) and (p2 <> nil) then
  begin
    // all items found
    FItemsByVA.Delete(p1, bNotify);
    FItemsByID.Delete(p2, false);
    FItemsByContVA.Delete(p3, false);
    exit(True);
  end;
  raise Exception.Create('Inconsistent sections.');
end;

function TVDSections.DelSecFromDB(const Sec: IVDSection): boolean;
var
  ID: TVDSectionId;
  key: TBytes;
begin
  ID := Sec.GetId;
  CreateSectionKey(skData, @ID, key);
  Result := TVDCore(CoreGet()).DB.Delete(key) = BP_OK;
end;

// -----------------------------------------------------------------------------

function TVDSections.ModifySection(var Sec: IVDSection; NewVA: TVA): boolean;
begin
  if not DelInternal(Sec, false) then // del, no notification
    exit(false);
  (Sec as TVDSection).SetStartVA(NewVA); // mod
  AddInternal(Sec, false);               // add, no notification
  exit(True);
end;

// -----------------------------------------------------------------------------

function TVDSections.CalcContDelta(VABegin, VAEnd: TVA): TVA;
var
  cur: TVDSectionItems.TRBNodePtr;
  dummy: IVDSection;
  Delta: uint32;
begin
  Result := 0;
  dummy := TVDSection.CreateDummy(VABegin);
  cur := FItemsByVA.Find(dummy);
  while (cur <> nil) and (VABegin < VAEnd) do
  begin
    if cur.K.Contains(VAEnd) then
    begin
      Delta := VAEnd - cur.K.GetStartVA;
      inc(Result, Delta);
      break;
    end;
    Delta := cur.K.GetSize;
    inc(Result, Delta);
    VABegin := cur.K.GetEndVA;
    FItemsByVA.Next(cur);
  end;
end;

procedure TVDSections.Clear;
begin
  BeginUpdate;
  try
    ClearInternal;
    FTotalSize := 0;
  finally
    EndUpdate;
  end;
end;

procedure TVDSections.ClearInternal;
begin
  FItemsByVA.Clear;
  FItemsByID.Clear;
  FItemsByContVA.Clear;
end;

constructor TVDSections.Create;
begin
  inherited Create();
  FSaveSectionWhenAdded := True;

  FItemsByVA := TVDSectionItems.Create(CompareIVDSectionLess);
  FItemsByVA.OnNotify := OnItemsNotify;
  // Secondary index don't need notifications.
  FItemsByID := TVDSectionItems.Create(CompareIVDSectionIDLess);
  FItemsByContVA := TVDSectionItems.Create(CompareSecLessByContVA);
end;

destructor TVDSections.Destroy;
begin
  FItemsByVA.Free;
  FItemsByID.Free;
  FItemsByContVA.Free;
  inherited;
end;

procedure TVDSections.DoEndUpdate;
begin
  SafeNotifyChanged;
end;

procedure TVDSections.DoSectionsChanged(const Sec: IVDSection; bSave: boolean; Action: TSectionAction);
begin
  case Action of
    SecAct_None:
      exit;
    SecAct_Added:
      begin
        // update db
        if bSave and FSaveSectionWhenAdded then
          if not SaveSecToDB(Sec) then
            raise Exception.Create('Failed to save section.');
        // update vm
        UpdateTotalSize(Sec.GetSize, True);
      end;
    SecAct_Removed:
      begin
        UpdateTotalSize(Sec.GetSize, false);
      end;
    SecAct_Changed:
      begin
        // update db
        if bSave and FSaveSectionWhenAdded then
          if not SaveSecToDB(Sec) then
            raise Exception.Create('Failed to save section.');
      end;
  end;

  // Update HexAddressDigitCount.
  if Assigned(Sec) and Assigned(FItemsByVA.Last) and (FItemsByVA.Last.K = Sec) then
    HexAddressDigitCount := VAToHexDigitCount(Sec.GetLastVA);

  FNeedRebuildContigiousPositions := True;

  SafeNotifyChanged;
end;

procedure TVDSections.Enumerate(cb: TSectionEnumProc; ud: pointer);
var
  Item: IVDSection;
begin
  if Assigned(cb) then
    for Item in FItemsByVA do
      if not cb(Item, ud) then
        break;
end;

function TVDSections.Find(VA: TVA): IVDSection;
var
  tmpSec: IVDSection;
  tmpRes: TVDSectionItems.TRBNodePtr;
begin
  tmpSec := TVDSection.CreateDummy(VA);
  try
    tmpRes := FItemsByVA.Find(tmpSec);
    if tmpRes <> nil then
      Result := tmpRes^.K
    else
      Result := nil;
  finally
    tmpSec := nil;
  end;
end;

function TVDSections.FindByID(ID: TVDSectionId): IVDSection;
var
  tmpSec: IVDSection;
  tmpRes: TVDSectionItems.TRBNodePtr;
begin
  tmpSec := TVDSection.Create(0, 1, ID, 0, '');
  try
    tmpRes := FItemsByID.Find(tmpSec);
    if tmpRes <> nil then
      Result := tmpRes^.K
    else
      Result := nil;
  finally
    tmpSec := nil;
  end;
end;

procedure TVDSections.FindEx(VA: TVA; OutPrev, OutCur, OutNext: PVDSection);
  procedure SetVal1(OutSec: PVDSection; NodePtr: TVDSectionItems.TRBNodePtr); inline;
  begin
    if (OutSec <> nil) then
      if (NodePtr <> nil) then
        OutSec^ := NodePtr^.K
      else
        OutSec^ := nil;
  end;

var
  tmpSec: IVDSection;
  prv, cur, nxt: TVDSectionItems.TRBNodePtr;
begin
  tmpSec := TVDSection.CreateDummy(VA);
  try
    FItemsByVA.FindEx(tmpSec, prv, cur, nxt);
    SetVal1(OutPrev, prv);
    SetVal1(OutCur, cur);
    SetVal1(OutNext, nxt);
  finally
    tmpSec := nil;
  end;
end;

procedure TVDSections.Flush;
var
  Sec: IVDSection;
  c: IVDCore;
begin
  c := CoreGet();
  for Sec in FItemsByVA do
  begin
    if SectionIsFlushable(Sec) then
      if not Sec.Flush() then
        c.Log.WriteLn(Format('Section flush failed for "%s"', [Sec.GetName]));
  end;
end;

function TVDSections.GetCount: Int;
begin
  Result := FItemsByVA.Count;
end;

function TVDSections.GetFirst: IVDSection;
begin
  if FItemsByVA.First <> nil then
    Result := FItemsByVA.First^.K
  else
    Result := nil;
end;

function TVDSections.GetLast: IVDSection;
begin
  if FItemsByVA.Last <> nil then
    Result := FItemsByVA.Last^.K
  else
    Result := nil;
end;

function TVDSections.MapContigiousPosToVA(Pos, MinPos, MaxPos: Int): TVA;
var
  posOfs: integer;
  vaOfs: TVA;
  userDelta: integer;
  SecPtr: TVDSectionItems.TRBNodePtr;
  Sec: TVDSection;
  tmp: IVDSection;

begin
  Result := BAD_VA;
  userDelta := MaxPos - MinPos;
  if userDelta = 0 then
    exit;
  posOfs := Pos - MinPos;

  // Refresh contigious positions.
  if FNeedRebuildContigiousPositions then
    RebuildContigiousPositions;

  // ofs in va cont space
  vaOfs := Trunc(posOfs * FTotalSize / userDelta);

  // search sec for vaOfs
  tmp := TVDSection.CreateDummy(0, vaOfs);

  SecPtr := FItemsByContVA.Find(tmp);
  if SecPtr = nil then
    exit;
  Sec := SecPtr^.K as TVDSection;
  Result := Sec.GetStartVA + (vaOfs - Sec.ContigiousPosition);
end;

function TVDSections.MapVAToContigiousPos(VA: TVA; MinPos, MaxPos: Int): Int;
var
  userDelta: integer;
  vaPos: TVA; // contigious VA pos.
  Sec: TVDSection;
begin
  Result := 0;
  userDelta := MaxPos - MinPos;
  if userDelta = 0 then
    exit;

  // Refresh contigious positions.
  if FNeedRebuildContigiousPositions then
    RebuildContigiousPositions;

  Sec := Find(VA) as TVDSection;
  if Sec = nil then
    exit;
  vaPos := Sec.ContigiousPosition + (VA - Sec.GetStartVA);
  // map.
  Result := (MinPos + Trunc(vaPos * userDelta / FTotalSize));
end;

procedure TVDSections.OnItemsNotify(Sender: TObject; const Item: IVDSection;
  Action: TCollectionNotification);
begin
  case Action of
    cnAdded:
      DoSectionsChanged(Item, True, SecAct_Added);
    cnRemoved:
      DoSectionsChanged(Item, True, SecAct_Removed);
  end;
end;

procedure TVDSections.RebuildContigiousPositions;
var
  SecIntf: IVDSection;
  Sec: TVDSection;
  contPos: TVA;
begin
  FItemsByContVA.Clear;
  contPos := 0;
  for SecIntf in self.FItemsByVA do
  begin
    Sec := TVDSection(SecIntf);
    Sec.ContigiousPosition := contPos;
    FItemsByContVA.add(Sec, false);
    inc(contPos, Sec.GetSize);
  end;
  FNeedRebuildContigiousPositions := false;
end;

// todo: TVDSections.Relocate: why not del old, add new section?
function TVDSections.Relocate(Section: IVDSection; NewVA: TVA): BOOL;
const
  SRelocationFailed = 'Relocation Failed';
var
  VABegin, VAEnd: TVA;
  Sec: IVDSection;
  cur, nxt, problem: TVDSectionItems.TRBNodePtr;
  c: IVDCore;
  Pos: TVDVAPos;
  scrRel: TRelVA;
  scrAbs: TVA;
  // usrRel: TRelVA;
  // usrAbs: TVA;
  // usrVA: TVA;
begin
  VABegin := NewVA;
  VAEnd := NewVA + Section.GetSize;
  // We have to check there will be no any overlapping section.
  problem := nil;
  Sec := TVDSection.CreateDummy(VABegin);

  FItemsByVA.FindEx(Sec, nil, @cur, @nxt);
  // cur?
  if (cur <> nil) then
  begin
    if cur^.K <> Section then
      problem := cur;
  end;
  // nxt?
  if (nxt <> nil) then
  begin
    if nxt^.K <> Section then
      if nxt^.K.GetStartVA < VAEnd then
        problem := cur;
  end;
  if problem <> nil then
    exit(false); // overlapped

  c := CoreGet;
  c.GetVAPos(@Pos);
  // if not c.GetUserVA(@usrVA) then
  // raise Exception.Create('usrva error');

  c.VM.AbsToRelVA(Pos.ScrVA, scrRel);
  // c.VM.AbsToRelVA(UsrVA, usrRel);
  // Modify.
  if not ModifySection(Section, NewVA) then
    exit(false);
  // Adjust VAs.
  if not c.VM.RelToAbsVA(scrRel, scrAbs) then
    raise Exception.Create(SRelocationFailed);
  // if not c.VM.RelToAbsVA(usrRel, usrAbs) then
  // raise Exception.Create(SRelocationFailed);
  Pos.ScrVA := scrAbs;
  // Pos.UsrVA := usrAbs;
  // Notify (change and save).
  DoSectionsChanged(Section, True, SecAct_Changed);
  c.ChangeVAPos(@Pos, false);
  // Done.
  exit(True);
end;

function TVDSections.SaveSecToDB(const Sec: IVDSection): boolean;
var
  ID: TVDSectionId;
  key: TBytes;
  Value: IVDStreamIO;
  SecFM: TVDFileMappedSection;
begin
  // Not all sections must be saved.
  // For example debug sections must not be saved.
  if not SectionIsSaveable(Sec) then
    exit(True);

  ID := Sec.GetId;
  CreateSectionKey(skData, @ID, key);
  Value := TVDStreamIO.Create(TVDMemoryStream.Create);

  // Common.
  Value.WriteU32(Sec.GetFlags);                         // flags
  Value.WriteU32(Sec.GetId);                            // id
  (Value as TVDStreamIO).WriteStr(Sec.GetName);         // name
  Value.WriteVA(Sec.GetStartVA);                        // start va
  (Value as TVDStreamIO).WriteSectionSize(Sec.GetSize); // size
  // File mapped section.
  if (Sec.GetFlags and TVDSectionFlag.FileMapped) <> 0 then
  begin
    SecFM := Sec as TVDFileMappedSection;
    (Value as TVDStreamIO).WriteStr(SecFM.FileName); // filename
    Value.WriteU64(SecFM.FileOfs);                   // fileofs
  end;
  // Put.
  Result := (CoreGet as TVDCore).DB.Put(key, (Value as TVDStreamIO).ToBytes) = BP_OK;
end;

function TVDSections.CreateSectionFromStream(const Stream: IVDMemoryStream): IVDSection;
var
  io: IVDStreamIO;
  Flags: TVDSectionFlags;
  Name: string;
  ID: TVDSectionId;
  StartVA: TVA;
  Size: TVDSectionSize;
  FMFileName: string;
  FMOffset: UInt64;
begin
  Result := nil;
  io := TVDStreamIO.Create(Stream);
  Flags := io.ReadU32;
  ID := io.ReadU32;
  Name := (io as TVDStreamIO).ReadStr;
  StartVA := io.ReadVA;
  Size := (io as TVDStreamIO).ReadSectionSize;
  // File mapped.
  if (Flags and TVDSectionFlag.FileMapped) <> 0 then
  begin
    FMFileName := (io as TVDStreamIO).ReadStr;
    FMOffset := io.ReadU64;
    Result := AddFromMappedFileInternal(FMFileName, Name, FMOffset, Size, StartVA, Flags, ID);
    exit;
  end
  else
  // Empty section.
  begin
    Result := AddEmptyInternal(Name, Size, StartVA, Flags, ID);
    exit;
  end;

  raise Exception.Create('CreateSectionFromStream failure.');
end;

function TVDSections.LoadAllFromDB: boolean;
var
  c: TVDCore;
  key, Bytes: TBytes;
  cursor: IBPlusTreeCursor;
  OldFSaveSectionWhenAdded: boolean;
begin
  OldFSaveSectionWhenAdded := FSaveSectionWhenAdded;
  try
    BeginUpdate;

    // Disable writing sections to db.
    FSaveSectionWhenAdded := false;
    c := TVDCore(CoreGet);

    CreateSectionKey(skData, nil, key);
    cursor := c.DB.CursorCreateEx(key, [kpGreater], True);
    if Assigned(cursor) then
      repeat
        Bytes := cursor.Value;
        LoadFromBytes(Bytes);
      until not cursor.Next;

    exit(True);
  finally
    // Restore.
    FSaveSectionWhenAdded := OldFSaveSectionWhenAdded;
    EndUpdate;
  end;
end;

procedure TVDSections.SafeNotifyChanged;
begin
  if FUpdateCount = 0 then
    CoreGet.Msg.Broadcast(TVDMessage.MSG_SECTIONS_CHANGED);
end;

function TVDSections.LoadFromBytes(const Bytes: TBytes): boolean;
var
  data: IVDMemoryStream;
  Sec: IVDSection;
begin
  Result := false;

  data := TVDMemoryStream.CreateFromMemory(@Bytes[0], length(Bytes));
  Sec := CreateSectionFromStream(data);
  if Sec <> nil then
    exit(True);
end;

procedure TVDSections.UpdateTotalSize(Delta: UInt64; bAdd: boolean);
var
  NewTotalSize: UInt64;
begin
  if bAdd then
    NewTotalSize := FTotalSize + Delta
  else
    NewTotalSize := FTotalSize - Delta;
  FTotalSize := NewTotalSize;
end;

function TVDSections.SaveToFile(Sec: IVDSection; Path: BSTR_IN): BOOL;
const
  bufsize = 128 * 1024;
var
  fn: string;
  left, Size: UInt64;
  buffer: TBytes;
  fs: TFileStream;
  VA: TVA;
begin
  if not Assigned(Sec) then
    exit(false);

  left := Sec.GetSize;

  fn := IOGet.ExpandPath(Path);

  fs := TFileStream.Create(fn, fmCreate);
  try
    if left <> 0 then
    begin
      VA := Sec.GetStartVA;
      SetLength(buffer, bufsize);
      while left > 0 do
      begin
        if left > bufsize then
          Size := bufsize
        else
          Size := left;

        Size := Sec.Read(VA, @buffer[0], Size);
        if Size = 0 then
          break;

        fs.Write(buffer, size);

        dec(left, Size);
        inc(VA, Size);
      end;
    end;
    Result := left = 0;
  finally
    fs.Free;
  end;
end;

end.
