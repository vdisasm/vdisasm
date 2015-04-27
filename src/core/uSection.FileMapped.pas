unit uSection.FileMapped;

interface

uses
  PagedStorage,
  uSection,
  VDAPI;

type

  { TVDFileMappedSection }

  TVDFileMappedSection = class(TVDSection, IVDSection)
  private
    FStorage: TFileStorage;
  private
    FOffset: UInt64; // start offset in file
    FFileName: string;
  public
    constructor Create(const FileName, Name: string; Offset: UInt64; RawSize,
      VirtSize: TVDSectionSize; VA: TVA; ID: TVDSectionId; Flags: TVDSectionFlags);
    destructor Destroy; override;

    function Read(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; override; stdcall;
    function Write(VA: TVA; Buffer: Pointer; Size: UInt32): UInt32; override; stdcall;
    function Flush: BOOL; override; stdcall;

    property FileName: string read FFileName;
    property FileOfs: UInt64 read FOffset;
  end;

implementation

{ TVDFileMappedSection }

constructor TVDFileMappedSection.Create(const FileName, Name: string;
  Offset: UInt64; RawSize, VirtSize: TVDSectionSize; VA: TVA; ID: TVDSectionId;
  Flags: TVDSectionFlags);
var
  ExpFileName: string;
begin
  inherited Create(VA, VirtSize, ID, Flags, Name);
  FFlags := FFlags or TVDSectionFlag.FileMapped;
  FFileName := FileName;
  FOffset := Offset;
  ExpFileName := IOGet.ExpandPath(FileName);
  FStorage := TFileStorage.Create(ExpFileName, False, 0, 0);
end;

destructor TVDFileMappedSection.Destroy;
begin
  FStorage.Free;
  inherited;
end;

function TVDFileMappedSection.Flush: BOOL;
begin
  FStorage.Flush;
  Result := True;
end;

function TVDFileMappedSection.Read(VA: TVA; Buffer: Pointer;
  Size: UInt32): UInt32;
var
  FileOfs: TStorageOffset;
begin
  FileOfs := (VA - FStartVA) + FOffset;
  if FStorage.Read(FileOfs, Buffer^, Size, @Result) <> TStatus.STATUS_OK then
    Result := 0;
end;

function TVDFileMappedSection.Write(VA: TVA; Buffer: Pointer;
  Size: UInt32): UInt32;
var
  FileOfs: TStorageOffset;
begin
  FileOfs := (VA - FStartVA) + FOffset;
  if FStorage.Write(FileOfs, Buffer^, Size, @Result) <> TStatus.STATUS_OK then
    Result := 0;
end;

end.
