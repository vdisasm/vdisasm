{
  * Current implementation loads/parses all types into memory.
  * Later it can be changed to load/parse on demand (for better speed, when
  * there's a lot of types).
}
unit uTypes.Lib;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  uStream,
  uStream.MemoryStream,
  uTypes.Base,
  uTypes.Kind,
  uCore.Strings,
  gRBTree,
  VDAPI;

type
  // ---------------------------------------------------------------------------
  TVDTypeLibrary = class;

  TLibItem = record
    // Name of type. Must be always defined because of type lookups.
    TypeName: string;
    // Offset is used when library loaded (headers) to parse type on demand.
    Offset: UInt32;
    // The type itself. If type library was created it is not Nil.
    // If type library was loaded it can be nil until first fetch. On first
    // access TypeValue will be loaded from stream (in library).
    TypeValue: IVDType;
  end;

  TTypeLibItemsMap = TRBTree<TLibItem>; // map of Name:Type

  TLibImport = record
    FileName: string;
    ShortName: string;
    Lib: IVDTypeLibrary;
  end;

  TLibImports = TRBTree<TLibImport>;

  // ---------------------------------------------------------------------------
  // Type library is container for types. It should contain pure definitions,
  // like arrays, records, i.e. info about structure of composite types. The
  // composite are built from simple types. The simple types must be provided by
  // Type Providers, which are standalone modules, loaded on demand.
  TVDTypeLibrary = class(TInterfacedObject, IVDTypeLibrary)
  private const
    TYPELIB_SIG: array [0 .. 3] of AnsiChar = 'VDTL';
    TYPELIB_VER: word                       = $0100;
  private
    // When library loaded, it's loaded into this stream. Then types can be
    // loaded from stream on demand. If it's nil, library was not yet loaded.
    FStream: TVDStreamIO;
    FMemoryStream: TVDMemoryStream;

    FDirty: boolean;
    FFileName: string;
    FShortName: string;
    FItems: TTypeLibItemsMap;
    FImportedLibsByFileName: TLibImports;  // sorted by file name, case insensitive
    FImportedLibsByShortName: TLibImports; // sorted by file name, case insensitive
    FPointerBitSize: TVDBitSize;
    procedure ItemsValueNotify(Sender: TObject; const Item: TLibItem; Action: TCollectionNotification);
    // Loads full node for type item. Before this call it can be loaded just
    // partially: name and offset.
    procedure FetchType(var Item: TLibItem);
  public
    constructor Create(const AName: string);
    destructor Destroy; override;

    // if OwnTheValue is True, set self as type library for the value.
    procedure AddType(Value: TVDType; OwnTheValue: boolean);

    function FindTypeInImports(TypeName: BSTR_IN): IVDType; stdcall;

    procedure LoadFromStream(const Stream: TVDMemoryStream);
    procedure SaveToStream(const Stream: IVDBaseStream);

    // Save to FileName file.
    function Save: boolean;
  public
    procedure MakeDirty; stdcall;

    function GetName: BSTR; stdcall;

    procedure Clear;

    function LoadFromFile(FileName: BSTR_IN): BOOL; stdcall;
    function SaveToFile(FileName: BSTR_IN): BOOL; stdcall;

    function ImportTypeLib(FileName: BSTR_IN): IVDTypeLibrary; stdcall;
    function FindImportedTypeLibByShortName(FileName: BSTR_IN): IVDTypeLibrary; stdcall;
    function FindImportedTypeLibByFileName(FileName: BSTR_IN): IVDTypeLibrary; stdcall;

    // Search type by name. If SearchInImports is False, only current lib
    // is searched, otherwise imports searched too.
    // If LibName is nil, it will search in self.
    function FindType(LibName, TypeName: BSTR_IN; SearchInImports: BOOL): IVDType; stdcall;

    procedure EnumImportedLibFileNames(EnumFunc: TTextEnumFunc; ud: pointer); stdcall;
    procedure EnumTypeNames(EnumFunc: TTextEnumFunc; ud: pointer); stdcall;

    property Dirty: boolean read FDirty;
  end;

  // Create new type library.
function TypeLibraryCreate(Name: BSTR_IN; FileName: BSTR_IN = nil): IVDTypeLibrary; stdcall;
// Load existing type library.
function TypeLibraryLoad(FileName: BSTR_IN): IVDTypeLibrary; stdcall;

implementation

uses
  uTypes.Parsing;

const
  DEF_PTR_BITSIZE = 32;

procedure MakeSureTypeLibExtPresent(var Name: string);
begin
  if not Name.EndsWith(SExtTypeLib, True) then
    Name := Name + SExtTypeLib;
end;

{ TTypeLibrary }

procedure TVDTypeLibrary.AddType(Value: TVDType; OwnTheValue: boolean);
var
  Item: TLibItem;
begin
  if Value <> nil then
  begin
    if OwnTheValue then
      Value.TypeLib := self;
    Item.TypeName := Value.Name;
    Item.TypeValue := Value;
    FItems.Add(Item);
    FDirty := True;
  end;
end;

function TLibImportsByFileName_Less(const a, b: TLibImport): boolean;
var
  sa, sb: string;
begin
  sa := a.FileName.ToUpper;
  sb := b.FileName.ToUpper;
  Result := sa < sb;
end;

function TLibImportsByShortName_Less(const a, b: TLibImport): boolean;
var
  sa, sb: string;
begin
  sa := a.ShortName.ToUpper;
  sb := b.ShortName.ToUpper;
  Result := sa < sb;
end;

function TLibItem_Less(const a, b: TLibItem): boolean;
begin
  Result := a.TypeName < b.TypeName;
end;

procedure TVDTypeLibrary.Clear;
begin
  FItems.Clear;
  FImportedLibsByFileName.Clear;
  FImportedLibsByShortName.Clear;
end;

constructor TVDTypeLibrary.Create(const AName: string);
begin
  inherited Create;
  FShortName := AName;
  FItems := TTypeLibItemsMap.Create(TLibItem_Less);
  FItems.OnNotify := ItemsValueNotify;
  FImportedLibsByFileName := TLibImports.Create(TLibImportsByFileName_Less);
  FImportedLibsByShortName := TLibImports.Create(TLibImportsByShortName_Less);
  FPointerBitSize := DEF_PTR_BITSIZE;
end;

destructor TVDTypeLibrary.Destroy;
begin
  FItems.Free;
  FImportedLibsByFileName.Free;
  FImportedLibsByShortName.Free;
  FStream.Free;
  inherited;
end;

procedure TVDTypeLibrary.EnumImportedLibFileNames(EnumFunc: TTextEnumFunc;
  ud: pointer);
var
  Imp: TLibImport;
begin
  if Assigned(EnumFunc) then
    for Imp in FImportedLibsByFileName do
      if not EnumFunc(Imp.FileName, ud) then
        break;
end;

procedure TVDTypeLibrary.EnumTypeNames(EnumFunc: TTextEnumFunc;
  ud: pointer);
var
  Item: TLibItem;
begin
  if Assigned(EnumFunc) then
    for Item in FItems do
      if not EnumFunc(Item.TypeName, ud) then
        break;
end;

procedure TVDTypeLibrary.FetchType(var Item: TLibItem);
var
  SavePos: uint64;
begin
  if (FStream = nil) or (Item.Offset = 0) then
    raise Exception.Create('Wrong type desc.');
  SavePos := FStream.GetPosition;
  FStream.SetPosition(Item.Offset);
  Item.TypeValue := TVDType.CreateFromStream(Item.TypeName, FStream, self);
  FStream.SetPosition(SavePos);
end;

function TVDTypeLibrary.FindImportedTypeLibByFileName(
  FileName: BSTR_IN): IVDTypeLibrary;
var
  key: TLibImport;
  cur: TLibImports.TRBNodePtr;
begin
  key.FileName := FileName;
  cur := FImportedLibsByFileName.Find(key);
  if cur = nil then
    exit(nil);
  Result := cur^.K.Lib;
end;

function TVDTypeLibrary.FindImportedTypeLibByShortName(FileName: BSTR_IN): IVDTypeLibrary;
var
  key: TLibImport;
  cur: TLibImports.TRBNodePtr;
begin
  key.ShortName := FileName;
  cur := FImportedLibsByShortName.Find(key);
  if cur = nil then
    exit(nil);
  Result := cur^.K.Lib;
end;

function TVDTypeLibrary.FindType(LibName, TypeName: BSTR_IN;
  SearchInImports: BOOL): IVDType;
var
  sLibName, sTypeName: string;
  key: TLibItem;
  cur: TTypeLibItemsMap.TRBNodePtr;
  PartLib, PartType: string;
  SearchLib: IVDTypeLibrary;
begin
  Result := nil;
  sLibName := LibName;
  sTypeName := TypeName;

  if (sLibName = '') or (FShortName = sLibName) then
    SearchLib := self
  else
  begin
    SearchLib := FindImportedTypeLibByShortName(BSTR_IN(sLibName));
    if SearchLib = nil then
      exit; // not found in imports
    Result := SearchLib.FindType(nil, BSTR_IN(sTypeName), SearchInImports);
    exit;
  end;

  // Search current types.
  // Try exact type name.
  key.TypeName := self.FShortName + '.' + sTypeName;
  cur := FItems.Find(key);
  if cur <> nil then
  begin
    if cur^.K.TypeValue = nil then
      FetchType(cur^.K);
    Result := cur^.K.TypeValue;
    exit;
  end;

  // If not found exactly same type name.
  // Maybe there's type lib name prefixed.
  FullTypeNameToTypeLibAndType(sTypeName, PartLib, PartType);
  if PartLib <> '' then
  begin
    Result := FindType(BSTR_IN(PartLib), BSTR_IN(PartType), SearchInImports);
    exit;
  end;

  // Try imports.
  if SearchInImports then
    Result := FindTypeInImports(BSTR_IN(sTypeName));
end;

function TVDTypeLibrary.FindTypeInImports(TypeName: BSTR_IN): IVDType;
var
  Imp: TLibImport;
begin
  for Imp in FImportedLibsByFileName do
  begin
    Result := Imp.Lib.FindType(nil, TypeName, False);
    if Result <> nil then
      exit;
  end;
  Result := nil;
end;

function TVDTypeLibrary.GetName: BSTR;
begin
  Result := FShortName;
end;

function TVDTypeLibrary.ImportTypeLib(FileName: BSTR_IN): IVDTypeLibrary;
var
  c: IVDCore;
  key: TLibImport;
  LibName: string;
begin
  LibName := FileName;
  // MakeSureTypeLibExtPresent(LibName);
  // Try to find existing.
  Result := FindImportedTypeLibByFileName(BSTR_IN(LibName));
  if Result <> nil then
    exit;
  // New import.
  c := CoreGet();
{$IFDEF DEBUG}
  c.Log.WriteLn('Importing type library: ' + LibName);
{$ENDIF}
  Result := TypeLibraryLoad(BSTR_IN(LibName));
  key.FileName := LibName;
  key.ShortName := Result.GetName;
  key.Lib := Result;

  FImportedLibsByFileName.Add(key);
  FImportedLibsByShortName.Add(key);

  FDirty := True;
end;

procedure TVDTypeLibrary.ItemsValueNotify(Sender: TObject; const Item: TLibItem;
  Action: TCollectionNotification);
begin
  case Action of
    cnRemoved:
      begin
        // Item.TypeValue.Free;
      end;
  end;
end;

procedure TVDTypeLibrary.LoadFromStream(const Stream: TVDMemoryStream);
var
  sig: array [0 .. 3] of AnsiChar;
  ver: uint16;
  cnt, u: UInt32;
  Name: string;
  Item: TLibItem;
  pItem: TTypeLibItemsMap.TRBNodePtr;
begin
  Clear;

  // Copy into FStream.
  if FStream = nil then
  begin
    FMemoryStream := TVDMemoryStream.Create;
    FStream := TVDStreamIO.Create(FMemoryStream);
  end;
  FMemoryStream.LoadFromStream(Stream);

  // Parse FStream header.
  FStream.ReadBuf(@sig[0], Length(TYPELIB_SIG));
  if sig <> TYPELIB_SIG then
    raise Exception.Create('Signature failed.');

  // Version.
  ver := FStream.ReadU16;
  if ver > TYPELIB_VER then
    raise Exception.Create('Version unsupported.');

  // Name.
  FShortName := FStream.ReadStr;

  // Imports.
  cnt := FStream.ReadU32;
  for u := 1 to cnt do
  begin
    Name := FStream.ReadStr;
    ImportTypeLib(BSTR_IN(Name));
  end;

  // Types.
  cnt := FStream.ReadU32;
  if cnt <> 0 then
  begin

    // read names
    for u := 1 to cnt do
    begin
      Name := FStream.ReadStr;
      Item.TypeName := Name;
      Item.TypeValue := nil;
      Item.Offset := 0;
      FItems.Add(Item);
    end;

    // read offsets
    pItem := FItems.First;
    while (pItem <> nil) do
    begin
      pItem^.K.Offset := FStream.ReadU32;
      FItems.Next(pItem);
    end;

  end;

  FDirty := False;
end;

procedure TVDTypeLibrary.MakeDirty;
begin
  FDirty := True;
end;

procedure TVDTypeLibrary.SaveToStream(const Stream: IVDBaseStream);
var
  io: TVDStreamIO;
  cnt, ofsPtrs: UInt32;
  Item: TLibItem;
  pItem: TTypeLibItemsMap.TRBNodePtr;
  Kind: byte;
  typ: TVDType;
  Imp: TLibImport;
begin
  io := TVDStreamIO.Create(Stream);
  try
    // hdr
    io.Write(TYPELIB_SIG, SizeOf(TYPELIB_SIG));
    io.WriteU16(TYPELIB_VER);
    io.WriteStr(FShortName);

    // Save imports.
    cnt := FImportedLibsByFileName.Count;
    io.WriteU32(cnt);
    if cnt <> 0 then
      for Imp in FImportedLibsByFileName do
      begin
        io.WriteStr(Imp.FileName);
      end;

    // write names
    cnt := FItems.Count;
    io.WriteU32(cnt);
    for Item in FItems do
    begin
      // io.WriteStr(Item.TypeValue.Name); // name
      io.WriteStr(Item.TypeValue.Name); // name
    end;

    // Reserve space for offsets.
    ofsPtrs := io.GetPosition;
    io.FillByte(0, cnt * SizeOf(UInt32));

    // Item data.
    pItem := FItems.First;
    while (pItem <> nil) do
    begin
      typ := pItem^.K.TypeValue as TVDType;

      pItem^.K.Offset := io.GetPosition; // update type offset

      Kind := byte(typ.Kind);
      io.WriteU8(Kind); // kind

      typ.SaveToStream(io);

      FItems.Next(pItem);
    end;
    // Item pointers(offsets).
    pItem := FItems.First;
    if pItem <> nil then
      io.SetPosition(ofsPtrs);
    while (pItem <> nil) do
    begin
      io.WriteU32(pItem^.K.Offset);
      FItems.Next(pItem);
    end;
  finally
    io.Free;
  end;
end;

function TVDTypeLibrary.LoadFromFile(FileName: BSTR_IN): BOOL;
var
  Stream: TVDMemoryStream;
begin
  Stream := TVDMemoryStream.Create();
  try
    Stream.LoadFromFile(FileName);
    LoadFromStream(Stream);
    Result := True;
  finally
    Stream.Free;
  end;
end;

function TVDTypeLibrary.Save: boolean;
begin
  if FFileName <> '' then
    Result := SaveToFile(BSTR_IN(FFileName))
  else
    Result := False;
end;

function TVDTypeLibrary.SaveToFile(FileName: BSTR_IN): BOOL;
var
  Stream: IVDBaseStream;
begin
  Stream := TVDMemoryStream.Create();
  SaveToStream(Stream);
  (Stream as TVDMemoryStream).SaveToFile(FileName);
  Result := True;
  FDirty := False;
end;

{$REGION 'Functions'}


// Create new type library.
function TypeLibraryCreate(Name: BSTR_IN; FileName: BSTR_IN): IVDTypeLibrary; stdcall;
var
  Lib: TVDTypeLibrary;
begin
  Lib := TVDTypeLibrary.Create(Name);
  Lib.FFileName := FileName;
  Result := Lib;
end;

// Load existing type library.
function TypeLibraryLoad(FileName: BSTR_IN): IVDTypeLibrary; stdcall;
var
  Lib: TVDTypeLibrary;
  fn: string;
begin
  fn := FileName;
  // MakeSureTypeLibExtPresent(fn);

  Lib := TVDTypeLibrary.Create('');
  Lib.LoadFromFile(BSTR_IN(fn));
  Lib.FFileName := fn;
  Result := Lib;
end;

exports
  TypeLibraryCreate,
  TypeLibraryLoad;
{$ENDREGION}

end.
