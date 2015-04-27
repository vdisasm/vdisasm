unit uCore.Save;

interface

uses
  VDAPI,
  uCore;

function CoreSave(C: TVDCore; IsClosingDb: boolean): boolean;
procedure ZipDb(C: TVDCore; IsClosingDb: boolean);

implementation

uses
  System.IOUtils,
  BPlusTree.Intf,
  uCore.Zip,
  uCore.Strings,
  uDB,
  uDirtyFlags,
  uInputFile,
  uTypes.Lib,
  uTypes.Mgr;

procedure ZipDb(C: TVDCore; IsClosingDb: boolean);
var
  fn, dbpath: string;
begin
  fn := DatabasePathToInputFileName(C.dbpath) + SVddbExt;
  dbpath := C.dbpath;

  C.CloseDatabaseWithoutSaving(False);

  ZipFolder(fn, dbpath);

  if not IsClosingDb then
    C.OpenPath(dbpath);
end;

function CoreSave(C: TVDCore; IsClosingDb: boolean): boolean;
var
  Tag: TDBTagCast;
  TypeLib: TVDTypeLibrary;
begin
  Result := False;

{$IFDEF DEBUG}
  C.GetLog.WriteLn('CoreSave begin');
{$ENDIF}

  // Sections
  // saved when added

  // Sections data
  C.GetVM.Sections.Flush;
{$IFDEF DEBUG}
  C.GetLog.WriteLn('  Section data flushed');
{$ENDIF}
  // CoreData
  if DirtyCoreData in C.DirtyFlags then
  begin
{$IFDEF DEBUG}
    C.GetLog.WriteLn('  Saving CoreData');
{$ENDIF}
    C.GetData^.Version.Major := VDAPI.TVDVersion._MAJOR;
    C.GetData^.Version.Minor := VDAPI.TVDVersion._MINOR;
    Tag := DBTAG_CoreData;
    if C.DB.PutRaw(@Tag, 1, C.GetData, SizeOf(TVDCoreData)) <> BP_OK then
      Exit;
  end;

  if not C.RememberedVAs.SaveToDb then
  begin
{$IFDEF DEBUG}
    C.GetLog.WriteLn('  RememberedVAs failed');
{$ENDIF}
  end;

  // InputFile
  if not(C.GetInputFile as TVDInputFile).SaveToDb(C) then
  begin
{$IFDEF DEBUG}
    C.GetLog.WriteLn('  Saving InputFile error');
{$ENDIF}
    Exit;
  end;

  // TypeLibs.
  TypeLib := C.GetTypeLib as TVDTypeLibrary;
  if TypeLib <> nil then
    if TypeLib.Dirty then
    begin
{$IFDEF DEBUG}
      C.GetLog.WriteLn('  Saving TypeLib');
{$ENDIF}
      TypeLib.Save;
    end;

  // main.db
  C.DB.Flush;

{$IFDEF DEBUG}
  C.GetLog.WriteLn('CoreSave end');
{$ENDIF}
  Exit(True);
end;

end.
