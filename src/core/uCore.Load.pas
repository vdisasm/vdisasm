unit uCore.Load;

interface

uses
  VDAPI,
  uCore;

function CoreLoad(const C: TVDCore): boolean;
procedure InitialImportTypeLibs(const C: TVDCore);

implementation

uses
  System.SysUtils,
  uCore.Strings,
  uDB,
//  uDB.ImportedPlugins,
  uSections,
  uInputFile,
  uTypes.Mgr,
  uTypes.Lib;

function LoadSections(C: TVDCore): boolean;
var
  Sections: TVDSections;
begin
  Sections := C.GetVM.Sections as TVDSections;
  Result := Sections.LoadAllFromDB;
end;

function LoadCoredata(C: TVDCore): boolean;
var
  Tag: TDBTagCast;
  D: TVDCoreData;
  cnt: Integer;
begin
  Tag := DBTAG_CoreData;
  cnt := C.DB.GetRaw(@Tag, 1, @D, sizeof(D));

  if cnt <> sizeof(D) then
  begin
    C.Log.WriteLn(Format('Coredata size differs: %d (expected %d)', [cnt, sizeof(D)]));
    exit(False);
  end;

  if (D.Version.Major <> D.Version._MAJOR) or
    (D.Version.Minor <> D.Version._MINOR) then
  begin
    C.Log.WriteLn(Format('Current core version %x.%x differs from db version %x.%x',
      [D.Version._MAJOR, D.Version._MINOR,
      D.Version.Major, D.Version.Minor]));
    exit(False);
  end;

  C.GetData^ := D;
  exit(True);
end;

procedure InitialImportTypeLibs(const C: TVDCore);
var
  fn: string;
  sInpFn: string;
  ImpStd: IVDTypeLibrary;
begin
  sInpFn := '%db%\main' + SExtTypeLib;

  fn := IOGet.ExpandPath(sInpFn);

  if not FileExists(fn) then
  begin
    if C.TypeLib <> nil then
      raise Exception.Create('Error');

    // Create database typelib.
    C.TypeLib := uTypes.Lib.TypeLibraryCreate('', BSTR_IN(sInpFn));
    // Import "std" type library.
    ImpStd := C.TypeLib.ImportTypeLib(STypeLibPath_Std);
    if ImpStd = nil then
      C.Log.WriteLn('Failed to load standard type library.');
  end
  else
  begin
    C.TypeLib := uTypes.Lib.TypeLibraryLoad(BSTR_IN(sInpFn));
  end;

  // Init "std" typeprovider.
  // todo: delete
  // uTypes.Provider.Std.RegisterStdTypeProvider(C);
end;

function CoreLoad(const C: TVDCore): boolean;
begin
  Result := False;

  // Core data.
  if not LoadCoredata(C) then
  begin
    C.GetLog.WriteLn(SFailedToLoadCoreData);
    exit;
  end;

  C.RememberedVAs.LoadFromDb;

  // Sections
  if not LoadSections(C) then
  begin
    C.GetLog.WriteLn(SFailedToSections);
    C.ClearStructures; // roll-back sections
    exit;
  end;

  // Input File.
  if not(C.GetInputFile as TVDInputFile).LoadFromDb(C) then
  begin
    C.GetLog.WriteLn(SFailedToLoadInputFileInfo);
    exit;
  end;

  // TypeName-UID mapping.
  if not(C.GetTypeMgr as TVDTypeMgr).LoadFromDb then
  begin
    C.GetLog.WriteLn(SFailedToLoadTypMgrInfo);
    exit;
  end;

  // Typelibs.
  InitialImportTypeLibs(C);

  // DB imported plugins.
//  if not(C.GetDBImportedPlugins as TVDDbImportedPlugins).LoadFromDb then
//  begin
//    C.GetLog.WriteLn(SFailedToLoadDBPlugins);
//    exit;
//  end
//  else
//  begin
//    (C.GetDBImportedPlugins as TVDDbImportedPlugins).LoadPlugins;
//  end;

{$IFDEF DEMO}
  C.Log.WriteLn(SDemoText);
{$ENDIF}
  Result := True;
end;

end.
