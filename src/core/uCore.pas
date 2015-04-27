unit uCore;

interface

uses
  System.Diagnostics,
  System.IniFiles,

{$IFDEF MSWINDOWS}
  WinApi.Windows,
{$ENDIF}
  BPlusTree,
  BPlusTree.Intf,
  uDirtyFlags,
  uRegions.TypeData,
  uRegions.GlobalBasicBlocks,
  uRememberedVAs,
  VDLib.Platform,
  VDAPI;

const
  DB_MAX_KEY_LEN    = 256;
  DB_PAGE_SIZE      = 8192;
  DB_PAGES_IN_CACHE = 256;

  STypeLibPath_Std = '%typelibs%\std.typelib';

type
  TVDDatabaseType =
    (
    DBTYPE_DISK,
    DBTYPE_MEM
    );

  TVDCore = class(TInterfacedObject, IVDCore)
  strict private
    FDBPath: string;
    FMainDBPath: string; // %db%\main.db
  private
    FDBType: TVDDatabaseType;
    FDBOpened: BOOL;
    FDirtyFlags: TDirtyFlags;
    FAnalysisVA: TVA;

    FData: TVDCoreData;
    FRememberedVAs: TRememberedVAs; // for navigation
    FPluginManager: IVDPluginManager;
    FInputFile: IVDInputFile;
    FMessageManager: IVDMessageManager;
    FLog: IVDLog;
    FMenu: IVDMenu;
    FNames: IVDNames;
    FComments: IVDComments;
    FDecoder: IVDDecoder;
    FRefs: IVDVAReferences;
    FUI: IVDUserInterface;
    FVM: IVDVirtualMemory;
    FTypeDataRegions: TVDRegionsTypeData;
    FGlobalBasicBlocks: IVDGlobalBasicBlocks;
    FAnalysisHelper: IVDAnalysisHelper;
    FUserData: IVDUserData;
    FTypeLib: IVDTypeLibrary;
    FTypeMgr: IVDTypeMgr;
    FJobs: IVDJobs;
    // FDbImportedPlugins: IVDDBImportedPlugins;
    FCPUs: IVDCpus;
    FFunctions: IVDFunctions;
    FImportSymbols: IVDImportSymbols;
    FExportSymbols: IVDExportSymbols;
    FDebugSession: IVDDebugSession;
    FSearch: IVDSearch;
    FStrings: IVDStrings;
    FProblems: IVDProblems;

    // Main DB. Must be nil if db isn't opened.
    FDB: IBPlusTree;
    procedure SetDBPath(const Value: string);
    procedure DeleteDbPathVariableAndClearDbPath;

    // Common functionality on db closing. Used in few places.
    procedure CloseDatabaseCommonProc(DeleteDbDir: boolean);
    procedure SetDBType(const Value: TVDDatabaseType);
  protected
    procedure CreateDefaultDirectories;

    procedure CreateDatabaseDirectory;
    procedure DeleteDatabaseDirectory;

    // Create new database dir and do setup.
    function CreateNewDatabaseInternal(
      Path: string;
      const LoaderFileName: string;
      LoaderFormatId: int;
      ClearInputFileInfo: boolean
      ): boolean;
    // Open input file and make database.
    function OpenNewInputFile(const InputFileName: string): boolean;
    // Open existing database.
    function OpenDatabase(const Path: string): boolean;

    // Call it to update all windows.
    // Recommended to call it right before function exit.
    procedure NotifyAllWindowsChanged;
  public
    constructor Create;

    // Initialization done once on first CoreGet.
    procedure InitOnce;

    // Done before Destroy.
    procedure ShutDown;

    procedure ClearStructures;

    procedure MainDBClose;
    function MainDBReopen(Exists: boolean): boolean;

    // If IsClosingDb True, main.db may not be reopened after zip saved.
    function SaveDatabaseInternal(IsClosingDb: boolean): boolean;

    procedure CloseDatabaseWithoutSaving(Delete: BOOL);

    function ChangeVAPosEx(Pos: PVDVAPos; Remember: BOOL = False; Force: BOOL = False): BOOL;

    property DBPath: string read FDBPath write SetDBPath;
    property DirtyFlags: TDirtyFlags read FDirtyFlags write FDirtyFlags;
    property TypeDataRegions: TVDRegionsTypeData read FTypeDataRegions;
  public
    { VDAPI }
    function GetData: PVDCoreData; stdcall;

    // Get currently opened file interface or nil if nothing opened.
    function GetPluginManager: IVDPluginManager; stdcall;
    function GetInputFile: IVDInputFile; stdcall;
    // function GetIO: IVDIO; stdcall;
    function GetMessageManager: IVDMessageManager; stdcall;
    function GetVM: IVDVirtualMemory; stdcall;
    function GetDecoder: IVDDecoder; stdcall; inline;
    function GetLog: IVDLog; stdcall; inline;
    function GetMenu: IVDMenu; stdcall; inline;
    function GetNames: IVDNames; stdcall; inline;
    function GetComments: IVDComments; stdcall; inline;
    function GetRefs: IVDVAReferences; stdcall; inline;
    function GetUI: IVDUserInterface; stdcall; inline;
    function GetUserData: IVDUserData; stdcall;
    function GetTypeLib: IVDTypeLibrary; stdcall;
    function GetTypeMgr: IVDTypeMgr; stdcall;
    function GetJobs: IVDJobs; stdcall;
    // function GetDBImportedPlugins: IVDDBImportedPlugins; stdcall;
    function GetCPUs: IVDCpus; stdcall;
    function GetFunctions: IVDFunctions; stdcall;
    function GetImportSymbols: IVDImportSymbols; stdcall;
    function GetExportSymbols: IVDExportSymbols; stdcall;
    function GetDebugSession: IVDDebugSession; stdcall;
    function GetSearch: IVDSearch; stdcall;
    function GetStrings: IVDStrings; stdcall;
    function GetProblems: IVDProblems; stdcall;
    function GetGlobalBasicBlocks: IVDGlobalBasicBlocks;
    function GetAnalysisHelper: IVDAnalysisHelper;

    // Other.

    // Check if database opened.
    function IsDatabaseOpened: BOOL; stdcall;
    // Open Source File or Database.
    function OpenPath(Path: BSTR): BOOL; stdcall;
    // Create new empty database.
    // Path defines database directory.
    function CreateNewDatabase(Path: BSTR): BOOL; stdcall;
    // Close current database.
    procedure CloseDatabase(Delete: BOOL); stdcall;
    procedure CleanDatabase; stdcall;
    // Save database, return True on success.
    function SaveDatabase: BOOL; stdcall;
    // Save database with name, return True on success.
    function SaveDatabaseAs(FileName: BSTR): BOOL; stdcall;
    // Zip database.
    function ZipDatabase: BOOL; stdcall;

    // UI.

    function GetVAPos(Pos: PVDVAPos): BOOL; stdcall;
    function GetUserVA: TVA; stdcall;
    procedure SetUserVA(VA: TVA); stdcall;

    function GetAnalysisVA: TVA; stdcall;

    // Change current VA position.
    function ChangeVAPos(Pos: PVDVAPos; Remember: BOOL = False): BOOL; stdcall;
    // Simplified version.
    function ChangeVA(VA: TVA; Remember: BOOL = False): BOOL; stdcall;
    // Go forward or backward by list of remembered VAs.
    function NavigateDirection(Direction: int = 1): BOOL; stdcall;
    procedure NavigateListClear; stdcall;

    // Other.
    // Try to get VA from Text (i.e. convert number or name to VA).
    function EvaluateVA(
      Expr: BSTR_IN;
      out VA: TVA;
      Flags: TExpressionEvaluateFlags = TNumberEvaluateFlag.EVAL_DEFAULT): BOOL; stdcall;

    function EvaluateExpr(
      Expr: BSTR_IN;
      out Value: IVDConstExpression;
      Flags: TExpressionEvaluateFlags = TNumberEvaluateFlag.EVAL_DEFAULT
      ): BOOL; stdcall;

    function GetTypeAt(VA: TVA; out TypeStr: BSTR): BOOL; stdcall;
    function MakeType(VA: TVA; TypeStr: BSTR_IN; Flags: TMakeTypeFlags = 0): UInt32; stdcall;
    function Undefine(VA: TVA; Size: SIZE_T = 1): SIZE_T; stdcall;
    function MakePointer(VA: TVA; Flags: TMakeTypeFlags = 0): SIZE_T; stdcall;

    procedure DumpDatabaseText(VA0, VA1: TVA; FileName: BSTR_IN); stdcall;
    procedure DumpDatabaseExports(FileName: BSTR_IN); stdcall;

    // procedure ImportCPU(Name: BSTR_IN); stdcall;

    function CodeAnalysis(VA: TVA; CodeType: BSTR_IN; Flags: TVDCodeAnalysisFlags): IVDFunction; stdcall;
    procedure CodeAnalysisStop; stdcall;

    // Properties.
    property RememberedVAs: TRememberedVAs read FRememberedVAs;
    property DB: IBPlusTree read FDB;
    property Log: IVDLog read FLog;
    property TypeMgr: IVDTypeMgr read FTypeMgr;
    property TypeLib: IVDTypeLibrary read FTypeLib write FTypeLib;
    property Functions: IVDFunctions read FFunctions;
    property Strings: IVDStrings read GetStrings;
    property VM: IVDVirtualMemory read GetVM;
    property ExportSymbols: IVDExportSymbols read GetExportSymbols;

    property AnalysisVA: TVA read FAnalysisVA write FAnalysisVA;
    property DBType: TVDDatabaseType read FDBType write SetDBType;

    property AnalysisHelper: IVDAnalysisHelper read GetAnalysisHelper;
  end;

  { Functions }

  // For CORE these functions are internal.

function CoreGet(
  CreateIfNotExists: boolean = False;
  ListenerProc: TVDMessageListenerProc = nil
  ): IVDCore; stdcall;

procedure CoreFree(); stdcall;

implementation

uses
  System.IOUtils,
  System.SysUtils,
  System.SyncObjs,

  uLoader,

  uCore.Load,
  uCore.MakeAndUndef,
  uCore.Save,
  uCore.Search,
  uCore.Strings,

  uDB,
  uDecoder,
  uEndianness,
  uEvaluate,
  uFindLoaders,
  uIO,
  uInputFile,
  uLog,
  uMenu,
  uMsg,
  uTextLayout,
  uVATextLayout,
  uTextual,
  uPlugins,
  uReferences,
  uSections,
  uUI,
  uVirtualMemory,
  uUserData,
  uTypes.Lib,
  uTypes.Mgr,
  uJobs,

  uDumpDBText,
  uDumpExports,

  uCPUs,
  uFunction,
  uFunctions,
  uSymbols.Import,
  uSymbols.Export,
  uStrings,
  uProblems,

  uFastCodeAnalysis,

  uDebugSession;

{ Globals }

var
  // Core singleton interface.
  // It must be IVDCore to make compiler able to trace interface references
  // and keep it alive.
  CoreSingleton: IVDCore;

  CoreInDestroying: boolean;

procedure FreeDB(var DB: IBPlusTree);
begin
  DB := nil;
end;

{ TVDCore }

constructor TVDCore.Create;
begin
  FRememberedVAs := TRememberedVAs.Create;
  FPluginManager := TVDPluginManager.Create;
  FInputFile := TVDInputFile.Create;
  // FIO := TVDIO.Create;
  FMessageManager := TVDMsgMgr.Create;
  FLog := TVDLog.Create;
  FMenu := TVDMenu.Create;
  // textual
  FComments := TVDComments.Create;
  FNames := TVDNames.Create;
  // ---
  FDecoder := TVDDecoder.Create;
  FRefs := TVDVAReferences.Create;
  FUI := TVDUserInterface.Create;
  FVM := TVDVirtualMemory.Create;
  // ---

  FTypeDataRegions := TVDRegionsTypeData.Create;
  FGlobalBasicBlocks := TVDGlobalBasicBlocks.Create;
  // FAnalysisHelper := TVDAnalysisHelper.Create;
  FUserData := TVDUserData.Create;

  // TypeLib must be created either when new file is loaded, or when existing
  // db is opened.
  // FTypeLib := TVDTypeLibrary.Create;

  FTypeMgr := TVDTypeMgr.Create;
  FJobs := TVDJobs.Create();
  // FDbImportedPlugins := TVDDbImportedPlugins.Create;
  FCPUs := TVDCPUs.Create;
  FFunctions := TVDFunctions.Create;
  FImportSymbols := TVDImportSymbols.Create;
  FExportSymbols := TVDExportSymbols.Create;
  FDebugSession := TVDDebugSession.Create;
  FSearch := TVDSearch.Create;
  FStrings := TVDStrings.Create;
  FProblems := TVDProblems.Create;

  // FUserVA := BAD_VA;
  FAnalysisVA := BAD_VA;
end;

procedure TVDCore.DumpDatabaseExports(FileName: BSTR_IN);
begin
  uDumpExports.DumpDatabaseExports(self, FileName);
end;

procedure TVDCore.DumpDatabaseText(VA0, VA1: TVA; FileName: BSTR_IN);
begin
  uDumpDBText.DumpDatabaseText(self, VA0, VA1, FileName);
end;

procedure buf2file(const buf; Size: integer; const FileName: string);
var
  f: file of byte;
begin
  AssignFile(f, FileName);
  Rewrite(f);
  BlockWrite(f, buf, Size);
  CloseFile(f);
end;

procedure TVDCore.CreateDefaultDirectories;
var
  io: IVDIO;
begin
  io := IOGet;
  io.CreateDirectory('%local%');
  io.CreateDirectory('%loaders%');
  io.CreateDirectory('%plugins%');
  io.CreateDirectory('%typelibs%');
end;

procedure TVDCore.CreateDatabaseDirectory;
{$IFDEF MSWINDOWS}
  procedure PutDesktopIni;
  const
    db_ico_raw: array [0 .. 198 - 1] of byte = (
      $0, $0, $1, $0, $1, $0, $10, $10, $2, $0, $1, $0, $1, $0, $B0, $0,
      $0, $0, $16, $0, $0, $0, $28, $0, $0, $0, $10, $0, $0, $0, $20, $0,
      $0, $0, $1, $0, $1, $0, $0, $0, $0, $0, $80, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $FF, $FF, $FF, $0, $FF, $FF, $0, $0, $80, $1, $0, $0, $FF, $FF,
      $0, $0, $E3, $7, $0, $0, $EB, $73, $0, $0, $C9, $49, $0, $0, $DD, $45,
      $0, $0, $94, $55, $0, $0, $B6, $55, $0, $0, $A2, $45, $0, $0, $AA, $49,
      $0, $0, $AA, $73, $0, $0, $88, $7, $0, $0, $FF, $FF, $0, $0, $80, $1,
      $0, $0, $FF, $FF, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0, $0,
      $0, $0, $0, $0, $0, $0
      );
  var
    ini: TIniFile;
    fn_desktop_ini: string;
  begin
    // db.ico
    buf2file(db_ico_raw[0], length(db_ico_raw), FDBPath + '\db.ico');

    // desktop.ini
    fn_desktop_ini := FDBPath + '\desktop.ini';
    ini := TIniFile.Create(fn_desktop_ini);
    try
      ini.WriteString('.ShellClassInfo', 'IconResource', 'db.ico');
      ini.WriteString('ViewState', 'FolderType', 'Generic');
    finally
      ini.Free;
    end;

    // attributes - for desktop.ini
    SetFileAttributes(PChar(fn_desktop_ini), FILE_ATTRIBUTE_SYSTEM or FILE_ATTRIBUTE_HIDDEN);
    // for folder
    SetFileAttributes(PChar(FDBPath), FILE_ATTRIBUTE_SYSTEM);
  end;
{$ENDIF}


begin
  if not TDirectory.Exists(FDBPath) then
  begin
    TDirectory.CreateDirectory(FDBPath);
{$IFDEF MSWINDOWS} PutDesktopIni; {$ENDIF}
  end;
end;

procedure TVDCore.DeleteDatabaseDirectory;
begin
  TDirectory.Delete(FDBPath, True);
end;

function TVDCore.EvaluateVA(Expr: BSTR_IN; out VA: TVA;
  Flags: TExpressionEvaluateFlags): BOOL;
var
  Value: IVDConstExpression;
begin
  if uEvaluate.EvaluateExpr(self, Expr, Value, Flags) then
    if Value.GetType = IRCT_UINT then
    begin
      VA := Value.GetUInt;
      Exit(True);
    end;
  Exit(False);
end;

function TVDCore.EvaluateExpr(
  Expr: BSTR_IN;
  out Value: IVDConstExpression;
  Flags: TExpressionEvaluateFlags = TNumberEvaluateFlag.EVAL_DEFAULT
  ): BOOL; stdcall;
begin
  Result := uEvaluate.EvaluateExpr(self, Expr, Value, Flags);
end;

function TVDCore.GetTypeAt(VA: TVA; out TypeStr: BSTR): BOOL;
begin
  Result := False;
end;

function TVDCore.MakePointer(VA: TVA; Flags: TMakeTypeFlags): SIZE_T;
var
  sType: string;
begin
  case FData.AddressSize of
    1:
      sType := TVDStdTypeName.u8;
    2:
      sType := TVDStdTypeName.u16;
    4:
      sType := TVDStdTypeName.u32;
    8:
      sType := TVDStdTypeName.u64;
  else
    Exit(0);
  end;
  Result := MakeType(VA, BSTR_IN(sType), Flags);
end;

function TVDCore.MakeType(VA: TVA; TypeStr: BSTR_IN; Flags: TMakeTypeFlags): UInt32;
begin
  Result := uCore.MakeAndUndef.DoMakeType(self, VA, TypeStr, Flags);
end;

function TVDCore.Undefine(VA: TVA; Size: SIZE_T): SIZE_T;
begin
  Result := uCore.MakeAndUndef.DoUndef(self, VA, Size);
end;

function TVDCore.ZipDatabase: BOOL;
begin
  uCore.Save.ZipDb(self, False); // reopen after zip
  Result := True;
end;

function TVDCore.GetAnalysisVA: TVA;
begin
  Result := FAnalysisVA;
end;

function TVDCore.GetComments: IVDComments;
begin
  Result := FComments;
end;

function TVDCore.GetCPUs: IVDCpus;
begin
  Result := FCPUs;
end;

function TVDCore.GetData: PVDCoreData;
begin
{$IFDEF DEBUG}
  // if not FDBOpened then
  // raise Exception.Create('Attempt to get CoreData when db is not opened.');
{$ENDIF}
  Result := @FData;
end;

// function TVDCore.GetDBImportedPlugins: IVDDBImportedPlugins;
// begin
// Result := FDbImportedPlugins;
// end;

function TVDCore.GetDebugSession: IVDDebugSession;
begin
  Result := FDebugSession;
end;

function TVDCore.GetDecoder: IVDDecoder;
begin
  Result := FDecoder;
end;

function TVDCore.GetExportSymbols: IVDExportSymbols;
begin
  Result := FExportSymbols;
end;

function TVDCore.GetFunctions: IVDFunctions;
begin
  Result := FFunctions;
end;

function TVDCore.GetUserData: IVDUserData;
begin
  Result := FUserData;
end;

function TVDCore.GetImportSymbols: IVDImportSymbols;
begin
  Result := FImportSymbols;
end;

function TVDCore.GetInputFile: IVDInputFile;
begin
  Result := FInputFile;
end;

function TVDCore.GetJobs: IVDJobs;
begin
  Result := FJobs;
end;

function TVDCore.GetLog: IVDLog;
begin
  Result := FLog;
end;

function TVDCore.GetMenu: IVDMenu;
begin
  Result := FMenu;
end;

function TVDCore.GetMessageManager: IVDMessageManager;
begin
  Result := FMessageManager;
end;

function TVDCore.GetNames: IVDNames;
begin
  Result := FNames;
end;

function TVDCore.GetPluginManager: IVDPluginManager;
begin
  Result := FPluginManager;
end;

function TVDCore.GetRefs: IVDVAReferences;
begin
  Result := FRefs;
end;

function TVDCore.GetSearch: IVDSearch;
begin
  Result := FSearch;
end;

function TVDCore.GetStrings: IVDStrings;
begin
  Result := FStrings;
end;

function TVDCore.GetProblems: IVDProblems;
begin
  Result := FProblems;
end;

function TVDCore.GetGlobalBasicBlocks: IVDGlobalBasicBlocks;
begin
  Result := FGlobalBasicBlocks;
end;

function TVDCore.GetAnalysisHelper: IVDAnalysisHelper;
begin
  Result := FAnalysisHelper;
end;

function TVDCore.GetTypeLib: IVDTypeLibrary;
begin
  Result := FTypeLib;
end;

function TVDCore.GetTypeMgr: IVDTypeMgr;
begin
  Result := FTypeMgr;
end;

function TVDCore.GetUI: IVDUserInterface;
begin
  Result := FUI;
end;

function TVDCore.GetVM: IVDVirtualMemory;
begin
  Result := FVM;
end;

function DummyPluginInfoCallback(FileName: BSTR_IN; Plugin: IUnknown;
  LoadedInfo: PVDPluginInfo; ud: pointer): BOOL; stdcall;
begin
  Result := True;
end;

// procedure TVDCore.ImportCPU(Name: BSTR_IN);
// var
// Path: string;
// begin
// Path := '%cpu%\' + Name;
// FDbImportedPlugins.Add(BSTR_IN(Path));
// end;

procedure TVDCore.InitOnce;
begin
  CreateDefaultDirectories;

  // Scan plugins to create mapping of "plugin name" -> "provided types"
  // Dirs scanned: CPU, TypeProviders
  (FPluginManager as TVDPluginManager).ScanPluginDir(TIOVar.CPU, DummyPluginInfoCallback, TPluginScanFlag.PLGSCAN_TYPEPROVIDERS);
  (FPluginManager as TVDPluginManager).ScanPluginDir(TIOVar.TypeProviders, DummyPluginInfoCallback, TPluginScanFlag.PLGSCAN_TYPEPROVIDERS);

  // Scan plugins that will be alive for a whole CoreGet session.
  (FPluginManager as TVDPluginManager).ScanPluginDir(TIOVar.Plugins, DummyPluginInfoCallback, 0 { TPluginScanFlag.PLGSCAN_TYPEPROVIDERS } );
end;

function TVDCore.IsDatabaseOpened: BOOL;
begin
  Result := self.FDBOpened;
end;

function TVDCore.OpenPath(Path: BSTR): BOOL;
var
  dbDir: string;
  fileExists, dirExists: boolean;
begin
  Result := False;

  // Close current db to avoid surprises.
  CloseDatabase(False);

  // Small filter.
  if Path = '' then
    Exit(False);

  fileExists := TFile.Exists(Path);
  dirExists := TDirectory.Exists(Path);

  // If neither file nor dir exists, exit.
  if (not fileExists) and (not dirExists) then
    Exit(False);

  // If it's dir, try to open it as a database.
  if dirExists then
    Exit(OpenDatabase(Path));

  // It's file.
  // Check if there's db folder for this file and open it.
  dbDir := inputFileNameToDatabasePath(Path);
  if TDirectory.Exists(dbDir) then
    Exit(OpenDatabase(dbDir));

  // If no existing database found, load file into new database.
  if TFile.Exists(Path) then
    Exit(OpenNewInputFile(Path));
end;

function TVDCore.OpenDatabase(const Path: string): boolean;
var
  VAPos: TVDVAPos;
begin
  FLog.WriteLn('Opening database: ' + Path);

  // ... Load input file from db.

  SetDBPath(Path);

  // Open main database.
  if MainDBReopen(True) then
  begin
    if CoreLoad(self) then
    begin
      FDBOpened := True;
      FDirtyFlags := [];

      // Notify DB Opened.
      FMessageManager.Broadcast(TVDMessage.MSG_DB_OPENED);

      // Set initial position.
      if GetVAPos(@VAPos) then
        ChangeVAPosEx(@VAPos, False, True);

      NotifyAllWindowsChanged;
      Exit(True);
    end;
  end
  else
  begin
    FLog.WriteLn('Failed to open main database');
  end;
  FreeDB(FDB);
  Exit(False);
end;

function TVDCore.CreateNewDatabaseInternal(
  Path: string;
  const LoaderFileName: string;
  LoaderFormatId: int;
  ClearInputFileInfo: boolean): boolean;
var
  Entry: TVA;
begin
  Path := IOGet.ExpandPath(Path);

  SetDBPath(Path);

  if ClearInputFileInfo then
  begin
    FInputFile.FileName := '';
    FInputFile.Params := '';
    FInputFile.WorkingDir := '';
  end;

  CreateDatabaseDirectory;

  // Create main database.
  if not MainDBReopen(False) then
  begin
    FLog.WriteLn('Failed to create main database');
    Exit(False);
  end;

  // Load db typelib.
  InitialImportTypeLibs(self);
  // Run found loader.
  if LoaderFileName <> '' then
    if not RunLoader(self, LoaderFileName, LoaderFormatId, Entry) then
    begin
      CloseDatabaseCommonProc(True);
      FLog.WriteLn('Loading cancelled');
      Exit(False);
    end;
  FDBOpened := True;

  // Load db imported plugins (could have been added by loader).
  // (FDbImportedPlugins as TVDDbImportedPlugins).LoadPlugins;

  // Initial save.
  include(FDirtyFlags, TDirtyFlag.DirtyCoreData);
  SaveDatabaseInternal(False);
  // Notify DB Opened.
  FMessageManager.Broadcast(TVDMessage.MSG_DB_OPENED);
  NotifyAllWindowsChanged;
  // Set initial position.
  ChangeVA(Entry, True);
  Exit(True);
end;

function TVDCore.CreateNewDatabase(Path: BSTR): BOOL;
begin
  // Close current (if opened).
  CloseDatabase(False);
  // Create new.
  Log.WriteLn('Creating new database');
  Result := CreateNewDatabaseInternal(Path, '', 0, True);
end;

function TVDCore.OpenNewInputFile(const InputFileName: string): boolean;
var
  LdrInfo: TLoaderInfoRec;
begin
  if TFile.Exists(InputFileName) then
  begin
    FLog.WriteLn('Opening input file: ' + InputFileName);
    // Can't parse empty file.
    if IOGet.FileSize(InputFileName) <> 0 then
    begin
      // Init InputFile intf.
      FInputFile.FileName := InputFileName;
      FInputFile.Params := '';
      FInputFile.WorkingDir := ExtractFileDir(InputFileName);

      // Find loader.
      if uFindLoaders.FindLoaderToAppy(InputFileName, LdrInfo) then
      begin
        if CreateNewDatabaseInternal(
          inputFileNameToDatabasePath(InputFileName),
          LdrInfo.LoaderFileName,
          LdrInfo.FormatId,
          False) then
        begin
          Exit(True);
        end;
      end
      else
      begin
        FLog.WriteLn(SNoLoadersFound);
      end;
    end
    else
    begin
      FLog.WriteLn('File is zero sized');
    end;
  end;
  MainDBClose;
  Exit(False);
end;

function TVDCore.GetVAPos(Pos: PVDVAPos): BOOL;
begin
  Result := FRememberedVAs.GetCurrent(Pos^);
end;

function TVDCore.GetUserVA: TVA;
var
  Pos: TVDVAPos;
begin
  if GetVAPos(@Pos) then
    Result := Pos.CurVA
  else
    Result := BAD_VA;
end;

procedure TVDCore.SetUserVA(VA: TVA);
var
  Pos: TVDVAPos;
begin
  if GetVAPos(@Pos) then
  begin
    Pos.CurVA := VA;
    ChangeVAPosEx(@Pos, False);
  end
  else
    raise Exception.Create('Failed to SetUserVA');
end;

function IsVaOnScreen(const c: TVDCore; VA: TVA): boolean;
var
  tmp: TVA;
begin
  tmp := VA;
  c.FMessageManager.Broadcast(MSG_IS_VA_ON_SCREEN, @tmp);
  Result := tmp = BAD_VA;
end;

function TVDCore.ChangeVA(VA: TVA; Remember: BOOL): BOOL;
var
  Pos: TVDVAPos;
begin
  {
    * VA can be:
    * - already on the screen, in this case
    *     - ScrVA stays same
    *     - UsrVA <- VA
    * - out of the screen
    *     - ScrVA <- VA
    *     - UsrVA <- VA
  }

  if not FRememberedVAs.GetCurrent(Pos) then
  begin
    // Current position not found.
    Pos.X := 0;
    Pos.ScrVA := VA;
    Pos.CurVA := VA;
  end
  else
  begin
    // Got current position.
    if IsVaOnScreen(self, VA) then
    begin
      // Pos.ScrVA and X is same
      Pos.CurVA := VA;
    end
    else
    begin
      // X is same
      Pos.ScrVA := VA;
      Pos.CurVA := VA;
    end;
  end;

  Result := ChangeVAPos(@Pos, Remember);
end;

function TVDCore.ChangeVAPos(Pos: PVDVAPos; Remember: BOOL): BOOL;
begin
  Result := ChangeVAPosEx(Pos, Remember, False);
end;

function TVDCore.ChangeVAPosEx(Pos: PVDVAPos; Remember, Force: BOOL): BOOL;
var
  Cur: TVDVAPos;
  bNotify, bCurrentExists: boolean;
begin
  bCurrentExists := FRememberedVAs.GetCurrent(Cur);

  if bCurrentExists then
  begin
    if Force then
    begin
      bNotify := True;
    end
    else
    begin
      // Check if need change.
      if
        (Cur.ScrVA = Pos^.ScrVA) and
        (Cur.X = Pos^.X) and
        (Cur.CurVA = Pos^.CurVA) then
      begin
        Exit(True);
      end;

      bNotify := True;
    end;

    if Remember then
      FRememberedVAs.Put(Cur);

    FRememberedVAs.SetCurrent(Pos^);
  end
  else
  begin
    Cur := Pos^;
    FRememberedVAs.Put(Cur);
    bNotify := True;
  end;

  // Notify.
  if bNotify then
    FMessageManager.Broadcast(TVDMessage.MSG_VAPOS_CHANGED, @Cur);

  Exit(True);
end;

function TVDCore.NavigateDirection(Direction: int): BOOL;
var
  Value: TVDVAPos;
begin
  if Direction > 0 then
    Result := FRememberedVAs.Next(Value)
  else
    Result := FRememberedVAs.Prev(Value);
  if Result then
    Result := ChangeVAPosEx(@Value, False, True);
end;

procedure TVDCore.NavigateListClear;
var
  bAdd: boolean;
  Cur: TVDVAPos;
begin
  bAdd := FRememberedVAs.GetCurrent(Cur);
  FRememberedVAs.Init;
  if bAdd then
    FRememberedVAs.Put(Cur);
end;

procedure TVDCore.NotifyAllWindowsChanged;
begin
  FMessageManager.Broadcast(MSG_NAMES_CHANGED);
  FMessageManager.Broadcast(MSG_REFS_CHANGED);
  FMessageManager.Broadcast(MSG_EXPORTS_CHANGED);
  FMessageManager.Broadcast(MSG_IMPORTS_CHANGED);
end;

procedure TVDCore.CleanDatabase;
begin
  if FDB.Cleanup <> BP_OK then
    FLog.WriteLn(SDbCleanupFailed)
  else
    FLog.WriteLn(SDbCleanupDone);
end;

procedure TVDCore.ClearStructures;
begin
  (FImportSymbols as TVDImportSymbols).Clear;
  (FVM.Sections as TVDSections).Clear;
end;

procedure TVDCore.CloseDatabase(Delete: BOOL);
begin
  if not IsDatabaseOpened then
    Exit;

  // No need to save db, when deleting
  if not Delete then
  begin
    // Save database.
    if not SaveDatabaseInternal(True) then
    begin
      raise Exception.Create(SFailedToSaveDB);
    end;
  end;

  CloseDatabaseWithoutSaving(Delete);
end;

procedure TVDCore.CloseDatabaseWithoutSaving(Delete: BOOL);
begin
  if not IsDatabaseOpened then
    Exit;

  CodeAnalysisStop;

  // If debugging, stop debugger.
  FDebugSession.Stop;

  // Ask user to release all resources.
  FMessageManager.Broadcast(TVDMessage.MSG_DB_BEFORE_CLOSED);

  CloseDatabaseCommonProc(Delete);

  // Notify on db closed.
  FMessageManager.Broadcast(TVDMessage.MSG_DB_CLOSED);
  NotifyAllWindowsChanged;

  if Delete then
    FLog.WriteLn('Database deleted')
  else
    FLog.WriteLn('Database closed');
end;

function TVDCore.CodeAnalysis(VA: TVA; CodeType: BSTR_IN; Flags: TVDCodeAnalysisFlags): IVDFunction;
var
  sw: TStopwatch;
  Stats: TAnalysisStats;
  CPU: IVDCpu;
  bCreateFunction: boolean;
begin
  // todo: stop analysis safely
  FLog.Clear;
  FLog.WriteLn('Code analysis started. Please wait...');
  sw := TStopwatch.StartNew;
  try
    Stats.Clear;

    if not uCPUs.GetCpuFromName(self, CodeType, CPU) then
      Exit(nil);

    bCreateFunction := (Flags and TVDCodeAnalysis.CA_CREATE_FUNCTION) <> 0;

    if bCreateFunction then
      Result := TVDFunction.Create(VA, CPU)
    else
      Result := nil;

    uFastCodeAnalysis.DoCodeAnalysis(
      Result,
      CPU,
      VA,
      TRefInfo.Create(BAD_VA, REFKIND_UNKNOWN),
      CodeType,
      Flags,
      nil,
      Stats);

  finally
    sw.Stop;
  end;
  FLog.WriteLn(Format('Done in: %s', [string(sw.Elapsed)]));
  FLog.WriteLn(Format('Sub-funcs: %d', [Stats.SubFunc]));
  FLog.WriteLn(Format('Analysis recursion level: %d', [Stats.AnalysisRecursionLevel]));
end;

procedure TVDCore.CodeAnalysisStop;
begin
end;

procedure TVDCore.CloseDatabaseCommonProc(DeleteDbDir: boolean);
begin
  FTypeLib := nil;
  // (FCPUs as TVDCPUs).ClearAll; // don't clear to allow cpus names be visible after db closed?
  (FTypeMgr as TVDTypeMgr).ClearAll;
  // imported plugins
  // (FDbImportedPlugins as TVDDbImportedPlugins).ClearAll;
  MainDBClose;
  ClearStructures;
  // Opened flag.
  FDBOpened := False;
  // Deletion.
  if DeleteDbDir then
    DeleteDatabaseDirectory;
  DeleteDbPathVariableAndClearDbPath;
end;

procedure TVDCore.DeleteDbPathVariableAndClearDbPath;
begin
  // Unrergister %db% path variable.
  IOGet.DelVariable(TIOVar.DB);
  FDBPath := '';
  FMainDBPath := '';
end;

procedure TVDCore.MainDBClose;
begin
  FreeDB(FDB);
end;

function TVDCore.MainDBReopen(Exists: boolean): boolean;
var
  Status: TBPlusTreeStatus;
begin
  // Path of main database.
  FMainDBPath := FDBPath + PathDelim + S_DB_MAIN;

  // Main key-value db.
  // BPlusTreeCreate(FDB, False);
  FDB := TBPlusTree.Create;

  if not Exists then
    Status := FDB.CreateNew(FMainDBPath, DB_MAX_KEY_LEN, DB_PAGE_SIZE, DB_PAGES_IN_CACHE, False)
  else
    Status := FDB.OpenExisting(FMainDBPath, DB_PAGES_IN_CACHE, False);

  Result := Status = TBPlusTreeStatus.BP_OK;
end;

function TVDCore.SaveDatabase: BOOL;
begin
  Result := SaveDatabaseInternal(False);
end;

function TVDCore.SaveDatabaseAs(FileName: BSTR): BOOL;
begin
  Result := False;
end;

function TVDCore.SaveDatabaseInternal(IsClosingDb: boolean): boolean;
begin
  if not IsDatabaseOpened then
    Exit(True);

  // Database is opened.

  FMessageManager.Broadcast(TVDMessage.MSG_DB_BEFORE_SAVE);

  Result := CoreSave(self, IsClosingDb);
  if Result then
    FDirtyFlags := [];

  FMessageManager.Broadcast(TVDMessage.MSG_DB_AFTER_SAVE);

  if Result then
    FLog.WriteLn(SDbSaved)
  else
    FLog.WriteLn(SFailedToSaveDB);
end;

procedure TVDCore.SetDBPath(const Value: string);
begin
  FDBPath := IOGet.ExpandPath(Value);
  IOGet.SetVariable(TIOVar.DB, Value);
end;

procedure TVDCore.SetDBType(const Value: TVDDatabaseType);
begin
  if Value <> FDBType then
  begin
    if not FDBOpened then
      FDBType := Value;
  end;
end;

procedure TVDCore.ShutDown;
begin
  CoreInDestroying := True;

  // Maynot close db here, because singleton interface is already NIL and
  // any access to CoreGet will trigger creation of newly initialized interface.
  //
  // Make sure db is closed before shutdown.
  // CloseDatabase(False);

  // Notification of core finalization.
  FMessageManager.Broadcast(TVDMessage.MSG_CORE_FREE);

  // Release plugins first.
  FPluginManager := nil;

  FreeAndNil(FRememberedVAs);

  // Free modules.

  // Other.
  FInputFile := nil;
  // FIO := nil;
  FMessageManager := nil;
  FLog := nil;
  FMenu := nil;
  FComments := nil;
  FNames := nil;
  FDecoder := nil;
  FRefs := nil;
  FUI := nil;
  FVM := nil;
  FTypeDataRegions.Free;
  FGlobalBasicBlocks := nil;
  FAnalysisHelper := nil;
  FUserData := nil;
  FTypeLib := nil;
  FTypeMgr := nil;
  FJobs := nil;
  // FDbImportedPlugins := nil;
  FCPUs := nil;
  FFunctions := nil;
  FImportSymbols := nil;
  FExportSymbols := nil;
  FDebugSession := nil;
  FSearch := nil;
  FStrings := nil;
  FProblems := nil;

  CoreInDestroying := False;
end;

{ Functions }

// -----------------------------------------------------------------------------
var
  CoreGet_CritSec: TCriticalSection;

  // Return core singleton interface.
function CoreGet(
  CreateIfNotExists: boolean = False;
  ListenerProc: TVDMessageListenerProc = nil
  ): IVDCore;
begin
  if Assigned(CoreSingleton) then
    Exit(CoreSingleton);

  if not CreateIfNotExists then
    Exit(nil);

  CoreGet_CritSec.Enter;
  try
    // May not call CoreGet while it is being destroyed, because it may result
    // in creation of new (invalid) core interface.
    if CoreInDestroying then
    begin
      raise Exception.Create('Attempt to CoreGet while core is destroying.');
    end;

    CoreSingleton := TVDCore.Create;

    // Add initial listener.
    if Assigned(ListenerProc) then
      CoreSingleton.Msg.AddListenerProc(ListenerProc)
    else
      raise Exception.Create('Initial core creation must have message ListenerProc');

    (CoreSingleton as TVDCore).InitOnce;

    Result := CoreSingleton;
  finally
    CoreGet_CritSec.Leave;
  end;
end;

// -----------------------------------------------------------------------------
var
  CoreFree_CritSec: TCriticalSection;

procedure CoreFree();
var
  CoreClass: TVDCore;
begin
  if CoreSingleton = nil then
    Exit;

  CoreFree_CritSec.Enter;
  try
    CoreClass := (CoreSingleton as TVDCore);
    if CoreClass.RefCount > 1 then
      raise Exception.Create('Core has more than 1 reference.');

    // Shutdown core subclasses.
    CoreClass.ShutDown;

    // Destroy core class.
    CoreSingleton := nil;
  finally
    CoreFree_CritSec.Leave;
  end;
end;
// -----------------------------------------------------------------------------

function CreateTextLayout(Flags: TVDTextFlags): IVDTextLayout; stdcall;
begin
  Result := TVDTextLayout.Create(Flags);
end;

function CreateVATextLayout(Flags: TVDTextFlags): IVDVATextLayout; stdcall;
begin
  Result := TVDVATextLayout.Create(Flags);
end;

// -----------------------------------------------------------------------------

exports
  CoreGet,
  CoreFree,
  CreateTextLayout,
  CreateVATextLayout;

initialization

CoreGet_CritSec := TCriticalSection.Create;
CoreFree_CritSec := TCriticalSection.Create;

finalization

FreeAndNil(CoreGet_CritSec);
FreeAndNil(CoreFree_CritSec);

end.
