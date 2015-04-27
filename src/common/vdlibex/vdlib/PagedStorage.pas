{$WARN COMPARING_SIGNED_UNSIGNED OFF}

{$IFDEF DEBUG}
{$DEFINE READWRITE_CHECK}
{$ENDIF}

unit PagedStorage;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  gRBTree;

const
  DEFAULT_PAGE_SIZE      = 8192;
  DEFAULT_PAGES_IN_CACHE = 128;

{$REGION 'TypeDefs'}


type
  TStatus =
    (
    STATUS_OK,                 // no error
    STATUS_ERROR,              // generic error
    STATUS_ERROR_READONLY,     // attempt to write in readonly mode
    STATUS_PAGE_ALREADY_LOCKED // attempt to lock already locked page
    );

  // Determines state of a page. It can be locked either for read or for write.
  TLockKind =
    (
    LOCK_NONE, // page is not locked
    LOCK_READ, // page is read-only
    LOCK_WRITE // while page is locked can't read, can't write
    );

  TLockKindHelper = record helper for TLockKind
    function ToString: string;
  end;

  TStoragePageSize = type uint32;
  TStoragePageNumber = type uint64;
  TStorageOffset = type uint64;
  PUInt32 = ^uint32;

  PStoragePageSize = ^TStoragePageSize;

  TPagedStorageBase = class;
{$ENDREGION 'TypeDefs'}
{$REGION 'TStoragePage'}
  TStoragePage = class;
  TStoragePageTree = TRBTree<TStoragePage>;

  TStoragePageAcessKind = (SPAK_READ, SPAK_WRITE);

  TOnStoragePageAccess = procedure(Page: TStoragePage; Kind: TStoragePageAcessKind) of object;

  TStoragePage = class
  private
    FNo: TStoragePageNumber;
    FUsedCount: uint32;

    FLock: TLockKind;
    FLockCount: integer;

    FOnAccess: TOnStoragePageAccess;

    procedure IncUsage(Kind: TStoragePageAcessKind); inline;
  public
    IsDirty: boolean;

    // Any user data. You must free it yourself on page deletion.
    // Use TStorageCache cnRemove notification.
    CustomData: pointer;

    ActualSize: TStoragePageSize; // Size of actual data.
    Data: TBytes;                 // Page data (PageSize).

    constructor Create(No: TStoragePageNumber; Lock: TLockKind; PageSize: TStoragePageSize);
    constructor CreateDummy(No: TStoragePageNumber);

    // Page must be LOCK_WRITE'ed before.
    // Write modifies Actual Size.
    procedure Write(Offset, Size: TStoragePageSize; Buf: PByte);
{$IFNDEF DEBUG} inline; {$ENDIF}
    // Page must be LOCK_READ'ed before.
    procedure Read(Offset, Size: TStoragePageSize; Buf: PByte);
{$IFNDEF DEBUG} inline; {$ENDIF}
    function LockPage(aLock: TLockKind): TStatus;
    procedure UnlockPage; // inline;
    function IsLocked: boolean; inline;

    property No: TStoragePageNumber read FNo write FNo;
  end;

  // Function to save all page data from in-memory page.
  // Must return True if succeeded or False on failure.
  TFlushPageFunc = function(Page: TStoragePage): boolean of object;

{$ENDREGION 'TPage'}
{$REGION 'TStorageCache'}

  TStorageCache = class
  private
    FStorage: TPagedStorageBase;
    FFlushPageFunc: TFlushPageFunc;
    FMax: TStoragePageNumber;
    FPagesByNumbers: TStoragePageTree;

    // Flush page.
    procedure DoFlushPage(Page: TStoragePage); {$IFNDEF DEBUG}inline; {$ENDIF}

    // Drop less used (non-locked) page out of cache.
    // If there is no non-locked pages exception is raised.
//    procedure DropPage;

    procedure DropAllNonLockedPages;

  public
    constructor Create(
      Storage: TPagedStorageBase;
      PageNotifyMethod: TCollectionNotifyEvent<TStoragePage>;
      MaxCount: TStoragePageNumber;
      FlushPageFunc: TFlushPageFunc);

    destructor Destroy; override;

    // Add page to cache.
    // If cache is filled, delete one item and insert new instead.
    procedure AddSafe(Page: TStoragePage);

    // Remove page.
    procedure Del(Page: TStoragePage); inline;

    // Flush all pages (not free it).
    procedure Flush;

    // Find page by number.
    function Find(No: TStoragePageNumber; out Page: TStoragePage): boolean;

{$IFDEF DEBUG}
    function Dump(Tree: TStoragePageTree; Full: boolean = False): string;
{$ENDIF}
  end;
{$ENDREGION 'TCache'}
{$REGION 'TPagedStorageStat'}

  TPagedStorageStats = record
    PageFlushCount: uint32;
    PageSeekCount: uint32;
  end;
{$ENDREGION}
{$REGION 'TPagedStorageBase'}
  { TPagedStorageBase }

  TPagedStorageBase = class
  private

    function GetSize: TStorageOffset; inline;
    function GetPageCountInStorage: TStoragePageNumber;
    procedure SetPageCountInStorage(const Value: TStoragePageNumber); // inline;

  protected

    FStats: TPagedStorageStats;

    FReadOnly: boolean;
    FPageSize: TStoragePageSize;
    FPagesInCache: TStoragePageNumber;
    FStream: TStream;
    FCache: TStorageCache;

    // Create page and read its data from storage. It is internal function, so
    // page is not cached at this stage.
    function CreateNewPage(No: TStoragePageNumber; Lock: TLockKind): TStoragePage;

    // Seek page.
    // If Page No does not exists, create it (only for Writable stream).
    // PageSizeAvail: returns size of page at current offset available. It's
    // always <= PageSize.
    procedure SeekPageBeginOffset(No: TStoragePageNumber;
      PageSizeAvailable: PStoragePageSize);

    // Save page data to stream.
    function FlushPage(Page: TStoragePage): boolean;

    // If you have some data bound to page that must be freed, override this
    // method and free your data before inherited.
    procedure PageCollectionNotify(Sender: TObject; const Item: TStoragePage;
      Action: TCollectionNotification); virtual;

  public

    constructor Create(Stream: TStream; ReadOnly: boolean; PageSize: TStoragePageSize;
      PagesInCache: TStoragePageNumber);

    destructor Destroy; override;

    // Save all dirty pages.
    procedure Flush; inline;

    // Lock page for access.
    function BeginPageAccess(No: TStoragePageNumber; Lock: TLockKind;
      out Page: TStoragePage): TStatus;

    // End page update and unlock page. It does not mean it will be
    // immediately saved to file. But can be cached until next flush.
    // Page is set to nil, to avoid mistakes, when trying to access Page after
    // it was released.
    procedure EndPageAccess(var Page: TStoragePage);

    // Convert absolute file offset to Page Number and Page Offset.
    procedure OffsetToPageNoOfs(Offset: TStorageOffset; out PgNo: TStoragePageNumber;
      out PgOfs: TStoragePageSize); inline;

    // Seek page.
    // Create page if it does not exist.
    procedure SeekPage(No: TStoragePageNumber; PageSizeAvailable: PStoragePageSize = nil);
      inline;

    // Write buffer.
    // cbProcessed: optional, returns number of bytes written.
    function Write(Offset: TStorageOffset; const Buf; Size: uint32;
      cbProcessed: PUInt32 = nil): TStatus;

    // Read buffer.
    // cbProcessed: optional varialble for number of bytes read.
    function Read(Offset: TStorageOffset; var Buf; Size: uint32;
      cbProcessed: PUInt32 = nil): TStatus;

    // Copy bytes from other storage.
    procedure CopyFrom(Src: TPagedStorageBase; SrcOfs, DstOfs: TStorageOffset;
      Size: TStorageOffset);

    // Append new empty page and return its number.
    function AppendPage: TStoragePageNumber;

    { Properties }

    // Size of storage (of underlying stream).
    property Size: TStorageOffset read GetSize;

    property ReadOnly: boolean read FReadOnly;
    property PagesInCache: TStoragePageNumber read FPagesInCache;

    // Number of full or partial pages currently saved to file.
    // Cached pages are not counted.
    property PageCountInStorage: TStoragePageNumber read GetPageCountInStorage;

    property Stats: TPagedStorageStats read FStats;

  end;
{$ENDREGION 'TPagedStorageBase'}
{$REGION 'TFileStorage'}

  TFileStorage = class(TPagedStorageBase)
  private
    FFileName: string;
  public
    constructor Create(const FileName: string; ReadOnly: boolean;
      PageSize: TStoragePageSize; PagesInCache: TStoragePageNumber);
    property FileName: string read FFileName;
  end;
{$ENDREGION 'TFileStorage'}
{$REGION 'TTextFileStorage'}

  TTextFileStorage = class(TFileStorage)
  private
    FOffset: TStorageOffset;
  public
    constructor Create(const FileName: string; ReadOnly: boolean;
      PageSize: TStoragePageSize);
    procedure WriteChars(const Chars: string);
    procedure WriteLn(const Text: string = ''); overload;
    procedure WriteLn(const Text: string; const Args: array of const); overload;
  end;
{$ENDREGION}

implementation

{$REGION 'TPage'}
{ TPage }

constructor TStoragePage.Create(
  No: TStoragePageNumber;
  Lock: TLockKind;
  PageSize: TStoragePageSize);
begin
  self.No := No;
  self.IsDirty := False;
  self.FLock := Lock;
  SetLength(self.Data, PageSize);
end;

constructor TStoragePage.CreateDummy(No: TStoragePageNumber);
begin
  self.No := No;
end;

procedure TStoragePage.IncUsage(Kind: TStoragePageAcessKind);
begin
  if FUsedCount = High(FUsedCount) then
    raise Exception.Create('overflow')
  else
    inc(FUsedCount);
  if Assigned(FOnAccess) then
    FOnAccess(self, Kind);
end;

function TStoragePage.IsLocked: boolean;
begin
  Result := FLockCount <> 0;
end;

function TStoragePage.LockPage(aLock: TLockKind): TStatus;
begin
{$IFDEF CDEBUG}
  try
{$ENDIF}
    // If page is not locked, we can acquire any lock.
    if not IsLocked then
    begin
      FLock := aLock;
      inc(FLockCount);
      exit(STATUS_OK);
    end;

    // READ. Lock only if already in read mode.
    if (aLock = LOCK_READ) and (FLock = LOCK_READ) then
    begin
      // FLock is LOCK_READ
      inc(FLockCount);
      exit(STATUS_OK);
    end;

    // All other cases fail.
    Result := STATUS_ERROR;
{$IFDEF CDEBUG}
  finally
    WriteLn(Format('LockPage(%d); locks:%d', [self.FNo, self.FLockCount]));
  end;
{$ENDIF}
end;

procedure TStoragePage.UnlockPage;
begin
  if FLockCount <> 0 then
  begin
    dec(FLockCount);
    if FLockCount = 0 then
      FLock := LOCK_NONE;
{$IFDEF CDEBUG}
    WriteLn(Format('UnlockPage(%d); locks:%d', [self.FNo, self.FLockCount]));
{$ENDIF}
  end;
end;

procedure TStoragePage.Write(Offset, Size: TStoragePageSize; Buf: PByte);
var
  EndPosition: TStoragePageSize;
begin
  if Size = 0 then
    exit;
  if IsLocked and (FLock <> LOCK_WRITE) then
    raise Exception.Create('Page is not locked for write.');
  EndPosition := Offset + Size;
{$IFDEF READWRITE_CHECK}
  if EndPosition > Length(self.Data) then
    raise Exception.Create('Write overflow.');
{$ENDIF}
  // Move data.
  Move(Buf^, Data[Offset], Size);
  // Fix actual size.
  if EndPosition > ActualSize then
    ActualSize := EndPosition;
  IsDirty := True;
  IncUsage(SPAK_WRITE);
end;

procedure TStoragePage.Read(Offset, Size: TStoragePageSize; Buf: PByte);
begin
  if Size = 0 then
    exit;
{$IFDEF READWRITE_CHECK}
  if (Offset + Size) > Length(self.Data) then
    raise Exception.Create('Read overflow.');
{$ENDIF}
  Move(Data[Offset], Buf^, Size);
  IncUsage(SPAK_READ);
end;

{$ENDREGION 'TPage'}
{$REGION 'TCache'}


function CompareLessPageNo(const A, B: TStoragePage): boolean; inline;
begin
  Result := A.No < B.No;
end;

{ TCache }

constructor TStorageCache.Create(
  Storage: TPagedStorageBase;
  PageNotifyMethod: TCollectionNotifyEvent<TStoragePage>;
  MaxCount: TStoragePageNumber;
  FlushPageFunc: TFlushPageFunc);
begin
  FStorage := Storage;
  FFlushPageFunc := FlushPageFunc;
  FMax := MaxCount;

  // Primary tree. For fast search.
  FPagesByNumbers := TStoragePageTree.Create(CompareLessPageNo);
  FPagesByNumbers.OnNotify := PageNotifyMethod;
end;

destructor TStorageCache.Destroy;
begin
  FPagesByNumbers.Free;
  inherited;
end;

procedure TStorageCache.DoFlushPage(Page: TStoragePage);
begin
  if Page.IsDirty then
  begin
    if Page.IsLocked then
      raise Exception.CreateFmt('Can''t flush locked page %d (locks:%d).',
        [Page.No, Page.FLockCount]);
    if FFlushPageFunc(Page) then
    begin
      Page.IsDirty := False;
      inc(FStorage.FStats.PageFlushCount);
    end;
  end;
end;


(*
procedure TStorageCache.DropPage;
var
  Page, Target: TStoragePage;
begin

  // todo: TStorageCache.DropPage loops through all!!! pages (256 in current case)
  // each time it's called (i.e. when cache is full). After new page is added
  // cache is full again and this hell repeat again.

  // Find less used non-locked page.
  Target := nil;
  for Page in FPagesByNumbers do
    if (not Page.IsLocked) then
      if (Target = nil) or (Page.FUsedCount < Target.FUsedCount) then
        Target := Page;

  if Target = nil then
    raise Exception.Create('Cannot drop page. No unlocked page found.');

{$IFDEF CDEBUG}
  WriteLn('Unloading page # ', Target.No);
{$ENDIF}
  Del(Target);
  // Don't have to free and flush page manually, because it will be
  // made automatically on cnRemove notification.
end;
*)

procedure TStorageCache.DropAllNonLockedPages;
var
  cur, next: TStoragePageTree.TRBNodePtr;
begin
  cur := FPagesByNumbers.First;
  while cur <> nil do
  begin
    next := FPagesByNumbers.GetNext(cur);
    if not cur.K.IsLocked then
      FPagesByNumbers.Delete(cur);
    cur := next;
  end;
end;

procedure TStorageCache.AddSafe(Page: TStoragePage);
begin
  // Remove page if cache is full.
  if (FPagesByNumbers.Count = FMax) then
  begin
{$IFDEF CDEBUG}
    WriteLn('Cache is full.');
{$ENDIF}

    // DropPage;
    // It is too slow and thus commented out.

    // DropAllNonLockedPages used instead of DropPage and gives about 4x
    // speedup on 256 pages when cache is full. The more pages the more speedup.
    DropAllNonLockedPages;
  end;
  // Add.
  FPagesByNumbers.Add(Page);
end;

procedure TStorageCache.Del(Page: TStoragePage);
begin
  FPagesByNumbers.Remove(Page);
end;

procedure TStorageCache.Flush;
var
  Page: TStoragePage;
begin
  for Page in FPagesByNumbers do
    DoFlushPage(Page);
end;

function TStorageCache.Find(No: TStoragePageNumber; out Page: TStoragePage): boolean;
var
  K: TStoragePage;
  p: TStoragePageTree.TRBNodePtr;
begin
  K := TStoragePage.CreateDummy(No);
  try
    p := FPagesByNumbers.Find(K);
    if p <> nil then
    begin
      Page := p^.K;
      exit(True);
    end;
  finally
    K.Free;
  end;
  Result := False;
end;

{$IFDEF DEBUG}


function TStorageCache.Dump(Tree: TStoragePageTree; Full: boolean): string;
var
  p: TStoragePage;
begin
  Result := '';

  if Tree = nil then
    Tree := FPagesByNumbers;

  for p in Tree do
    if p.IsLocked or Full then
      Result := Result + Format('%d-%s(%d,%d);',
        [p.No, p.FLock.ToString, p.FLockCount, p.FUsedCount])
end;
{$ENDIF}

{$ENDREGION 'TCache'}
{$REGION 'TPagedStorageBase'}
{ TPagedStorageBase }

function TPagedStorageBase.AppendPage: TStoragePageNumber;
begin
  Result := PageCountInStorage;
  SetPageCountInStorage(Result + 1);
end;

function TPagedStorageBase.BeginPageAccess(No: TStoragePageNumber; Lock: TLockKind;
  out Page: TStoragePage): TStatus;
var
  CachedPage: TStoragePage;
begin
  Page := nil;
{$IFDEF READWRITE_CHECK}
  try
{$ENDIF}
    if FCache.Find(No, CachedPage) then
    begin
      Page := CachedPage;
      Result := Page.LockPage(Lock);
{$IFDEF CDEBUG}
      WriteLn(Format('BeginPageAccess(%d); Page Found In Cache. Locks:%d',
        [No, CachedPage.FLockCount]));
{$ENDIF}
      exit;
    end;

    // Not in cache.
    Page := CreateNewPage(No, Lock);
    Result := Page.LockPage(Lock);
    FCache.AddSafe(Page);
{$IFDEF READWRITE_CHECK}
  finally
    if Page.FNo <> No then
      raise Exception.Create('BeginPageAccess: fetched page number is wrong');
  end;
{$ENDIF}
end;

procedure TPagedStorageBase.EndPageAccess(var Page: TStoragePage);
begin
  Page.UnlockPage;
{$IFDEF CDEBUG}
  WriteLn(Format('EndPageAccess(%d); locks:%d', [Page.No, Page.FLockCount]));
{$ENDIF}
  Page := nil;
end;

procedure TPagedStorageBase.CopyFrom(Src: TPagedStorageBase;
  SrcOfs, DstOfs: TStorageOffset; Size: TStorageOffset);
var
  Buf: PByte;
  tmpSize, doneSize: TStoragePageSize;
begin
  if Size = 0 then
    exit;
  tmpSize := FPageSize;
  Buf := GetMemory(FPageSize);
  try
    while Size <> 0 do
    begin
      if Src.Read(SrcOfs, Buf, tmpSize, @doneSize) <> STATUS_OK then
        raise Exception.Create('Read Error.');
      if self.Write(DstOfs, Buf, doneSize) <> STATUS_OK then
        raise Exception.Create('Write Error.');
      dec(Size, doneSize);
      inc(SrcOfs, doneSize);
      inc(DstOfs, doneSize);
    end;
  finally
    FreeMem(Buf);
  end;
end;

constructor TPagedStorageBase.Create(Stream: TStream; ReadOnly: boolean;
  PageSize: TStoragePageSize; PagesInCache: TStoragePageNumber);
begin
  inherited Create();

  FStream := Stream;
  FReadOnly := ReadOnly;
  FPageSize := PageSize;
  FPagesInCache := PagesInCache;
  FCache := TStorageCache.Create(self, PageCollectionNotify, PagesInCache, FlushPage);
end;

function TPagedStorageBase.CreateNewPage(No: TStoragePageNumber;
  Lock: TLockKind): TStoragePage;
var
  PageSizeAvail: TStoragePageSize;
begin
  // Seek page and get max size we can read.
  SeekPageBeginOffset(No, @PageSizeAvail);
  // Alloc new page.
  Result := TStoragePage.Create(No, Lock, FPageSize);
  // Read page data.
  Result.ActualSize := PageSizeAvail;
  if FStream.Read(Result.Data[0], PageSizeAvail) <> PageSizeAvail then
    raise Exception.Create('Page read error.');
{$IFDEF CDEBUG}
  WriteLn(Format('CreateNewPage(%d); read %d bytes @ %d', [No, PageSizeAvail,
    FStream.Position - PageSizeAvail]));
{$ENDIF}
end;

destructor TPagedStorageBase.Destroy;
begin
  if FCache <> nil then
  begin
    FCache.Flush;
    FCache.Free;
  end;
  FStream.Free;
  inherited;
end;

procedure TPagedStorageBase.Flush;
begin
  FCache.Flush;
end;

function TPagedStorageBase.FlushPage(Page: TStoragePage): boolean;
begin
{$IFDEF READWRITE_CHECK}
  if Length(Page.Data) > FPageSize then
    raise Exception.Create('Page corrupted.');
{$ENDIF}
  SeekPageBeginOffset(Page.No, nil);
  // FStream.Write(Page.Data[0], Length(Page.Data));
  FStream.Write(Page.Data[0], Page.ActualSize);
  Result := True;
{$IFDEF CDEBUG}
  WriteLn('FlushPage done, # ', Page.No);
{$ENDIF}
end;

function TPagedStorageBase.GetPageCountInStorage: TStoragePageNumber;
begin
  Result := (FStream.Size div FPageSize);
  if (FStream.Size mod FPageSize) <> 0 then
    inc(Result);
end;

function TPagedStorageBase.GetSize: TStorageOffset;
begin
  Result := FStream.Size;
end;

procedure TPagedStorageBase.SeekPage(No: TStoragePageNumber;
  PageSizeAvailable: PStoragePageSize);
begin
  SeekPageBeginOffset(No, PageSizeAvailable);
end;

procedure TPagedStorageBase.SeekPageBeginOffset(No: TStoragePageNumber;
  PageSizeAvailable: PStoragePageSize);
var
  BeginOffset, NextPageOffset: TStorageOffset;
begin
  BeginOffset := No * FPageSize; // where to seek
  NextPageOffset := BeginOffset + FPageSize;

  if FStream.Position <> BeginOffset then
  begin

    if BeginOffset >= FStream.Size then
    begin
      // Handle access out of file.
      if FReadOnly then
        raise Exception.CreateFmt('Seek out of file (%d >= %d)',
          [BeginOffset, FStream.Position])
      else
        FStream.Size := NextPageOffset; // alloc new page
    end;

    // Do seek.
    FStream.Position := BeginOffset;

    inc(FStats.PageSeekCount);
  end;

  // FStream.Position = BeginOffset

  // Get PageSizeAvailable
  if PageSizeAvailable <> nil then
  begin
    if NextPageOffset >= FStream.Size then
      PageSizeAvailable^ := FStream.Size - BeginOffset
    else
      PageSizeAvailable^ := NextPageOffset - BeginOffset;
  end;
end;

procedure TPagedStorageBase.SetPageCountInStorage(
  const Value: TStoragePageNumber);
begin
  FStream.Size := Value * FPageSize;
end;

procedure TPagedStorageBase.OffsetToPageNoOfs(Offset: TStorageOffset;
  out PgNo: TStoragePageNumber; out PgOfs: TStoragePageSize);
begin
  PgNo := Offset div FPageSize;
  PgOfs := Offset mod FPageSize;
end;

procedure TPagedStorageBase.PageCollectionNotify(Sender: TObject;
  const Item: TStoragePage; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
  begin
    FCache.DoFlushPage(Item);
    Item.Free;
  end;
end;

function TPagedStorageBase.Read(Offset: TStorageOffset; var Buf;
  Size: uint32; cbProcessed: PUInt32): TStatus;
var
  Status: TStatus;
  PgNo: TStoragePageNumber;
  PgOfs: TStoragePageSize;
  Page: TStoragePage;
  cbToDo: uint32;
  Buffer: PByte;
begin
  if (cbProcessed <> nil) then
    cbProcessed^ := 0;

  Buffer := @Buf;

  // Process by pages.
  while Size <> 0 do
  begin
    if Offset = FStream.Size then
      break;

    // Get page number and offset by file offset.
    OffsetToPageNoOfs(Offset, PgNo, PgOfs);

    // Fetch page.
    Status := BeginPageAccess(PgNo, LOCK_READ, Page);
    if Status <> STATUS_OK then
      exit(Status);

    // Get data.
    if (PgOfs + Size) > Page.ActualSize then
      cbToDo := Page.ActualSize - PgOfs
    else
      cbToDo := Size;

    if cbToDo <> 0 then
      Page.Read(PgOfs, cbToDo, Buffer);

    // Done with page.
    EndPageAccess(Page);

    if cbToDo <> 0 then
    begin
      // Iterate.
      dec(Size, cbToDo);
      inc(Buffer, cbToDo);
      if (cbProcessed <> nil) then
        inc(cbProcessed^, cbToDo);

      inc(Offset, cbToDo);
    end;

  end;

  exit(STATUS_OK);
end;

function TPagedStorageBase.Write(Offset: TStorageOffset; const Buf;
  Size: uint32; cbProcessed: PUInt32): TStatus;
var
  Status: TStatus;
  PgNo: TStoragePageNumber;
  PgOfs: TStoragePageSize;
  Page: TStoragePage;
  cbToDo: uint32;
  Buffer: PByte;
begin
  if Assigned(cbProcessed) then
    cbProcessed^ := 0;

  if Size = 0 then
    exit(STATUS_OK);

  if FReadOnly then
    exit(STATUS_ERROR_READONLY); // we can't write in read only mode.

  // Get page number and offset by file offset.
  OffsetToPageNoOfs(Offset, PgNo, PgOfs);

  Buffer := @Buf;

  // Process by pages.
  while Size <> 0 do
  begin
    // Fetch page.
    Status := BeginPageAccess(PgNo, LOCK_WRITE, Page);
    if Status <> STATUS_OK then
      exit(Status);

    // Modify data.
    // Can write more than current ActualSize.
    // Maybe this will be changed in future.
    if (PgOfs + Size) > FPageSize then
      cbToDo := FPageSize - PgOfs
    else
      cbToDo := Size;

    Page.Write(PgOfs, cbToDo, Buffer);

    // Done with page.
    EndPageAccess(Page);

    // Iterate.
    dec(Size, cbToDo);
    inc(Buffer, cbToDo);
    if (cbProcessed <> nil) then
      inc(cbProcessed^, cbToDo);

    inc(PgNo);
    PgOfs := 0;
  end;

  exit(STATUS_OK);
end;
{$ENDREGION 'TPagedStorageBase'}
{$REGION 'TFileStorage'}


constructor TFileStorage.Create(const FileName: string; ReadOnly: boolean;
  PageSize: TStoragePageSize; PagesInCache: TStoragePageNumber);
var
  bFileExists: boolean;
  Mode: word;
  Stream: TFileStream;
begin
  // Open/Create file.
  bFileExists := FileExists(FileName);

  if (not bFileExists) then
  begin
    if ReadOnly then
      raise Exception.Create('File not exists.')
    else
      Mode := fmCreate or fmShareDenyWrite;
  end
  else // File exists.
  begin
    if ReadOnly then
      Mode := fmOpenRead or fmShareDenyWrite
    else
      Mode := fmOpenReadWrite or fmShareDenyWrite;
  end;

  Stream := TFileStream.Create(FileName, Mode);

  if PageSize = 0 then
    PageSize := DEFAULT_PAGE_SIZE;

  if PagesInCache = 0 then
    PagesInCache := DEFAULT_PAGES_IN_CACHE;

  self.FFileName := FileName;

  inherited Create(Stream, ReadOnly, PageSize, PagesInCache);
end;
{$ENDREGION 'TFileStorage'}
{$REGION 'TLockKindHelper'}
{ TLockKindHelper }

function TLockKindHelper.ToString: string;
const
  A: array [TLockKind] of string = ('', 'R', 'W');
begin
  Result := A[self];
end;
{$ENDREGION}
{$REGION 'TTextFileStorage'}

{ TTextFileStorage }

constructor TTextFileStorage.Create(const FileName: string; ReadOnly: boolean;
  PageSize: TStoragePageSize);
begin
  inherited Create(FileName, ReadOnly, PageSize, 1);
end;

procedure TTextFileStorage.WriteChars(const Chars: string);
const
  UTF8BOM: array [0 .. 2] of byte = ($EF, $BB, $BF);
var
  Bytes: TBytes;
  Size: integer;
begin
  Bytes := TEncoding.UTF8.GetBytes(Chars);
  if FOffset = 0 then
  begin
    Write(0, UTF8BOM, 3);
    FOffset := 3;
  end;
  Size := Length(Bytes);
  if Size <> 0 then
  begin
    Write(FOffset, bytes[0], Size);
    inc(FOffset, Size);
  end;
end;

procedure TTextFileStorage.WriteLn(const Text: string;
  const Args: array of const);
begin
  WriteLn(Format(Text, Args));
end;

procedure TTextFileStorage.WriteLn(const Text: string);
begin
  WriteChars(Text + sLineBreak);
end;

{$ENDREGION}

end.
