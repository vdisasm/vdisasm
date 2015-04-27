unit uDB;

interface

uses
  System.SysUtils, // TBytes
  VDAPI;

const
  S_DB_MAIN = 'main.db';

type

  TDBTagCast = byte;
  TDBTag = byte;

  // Tags are used in low-level database data form (1-byte per tag).

  // {$SCOPEDENUMS ON}

  //
  // Primary tags.
  //

const
  DBTAG_Null         = 0; // not used
  DBTAG_InputFile    = 1; // input file data
  DBTAG_Section      = 2; //
  DBTAG_Name         = 3; //
  DBTAG_NameBackRef  = 4; //
  DBTAG_Comment      = 5; //
  DBTAG_CoreData     = 6; //
  DBTAG_HiddenRegion = 7; // currently not used
  DBTAG_UserData     = 8; //

  // todo: maybe rename it to something like "TCustomRegionData"
  DBTAG_TypeDataId = 9; // Originally was type id only. Since 1.4. it's region.

  DBTAG_TypeDataIdName = 10; // (Tag,UID),(Name)
  DBTAG_DbImpPlugin    = 11; // imported db plugin
  DBTAG_RemeberedVAs   = 12; // navigation list
  DBTAG_Func           = 13; // function
  DBTAG_EncodedString  = 14; // string with encoding; not implemented
  DBTAG_RefFrom        = 15; // reference from VA
  DBTAG_ExportedSym    = 16; // exported symbol at VA
  DBTAG_ImportedSym    = 17; // imported symbol at VA
  DBTAG_ImportedLib    = 18; // imported library
  DBTAG_Problem        = 19; // for problems list

  DBTAG_GlobalBasicBlock = 20; // global basic block

  //
  // Secondary tags.
  //

const
  // Sections.
  DBTAG_SECTION_LASTID = 0;
  DBTAG_SECTION_DATA   = 1;

  // Get db path from input filename.
function InputFileNameToDatabasePath(const InputFn: string): string;
function DatabasePathToInputFileName(const Path: string): string;

implementation

uses
  VDLib.Math.Value,
  uCore.Strings,
  uEndianness;

function InputFileNameToDatabasePath(const InputFn: string): string;
begin
  Result := InputFn + SVddbDirPostfix;
end;

function DatabasePathToInputFileName(const Path: string): string;
begin
  if Path.ToLower.EndsWith(SVddbDirPostfix) then
    Result := Path.Remove(Path.Length - SVddbDirPostfix.Length)
  else
    Result := Path;
end;

end.
