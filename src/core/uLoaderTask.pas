unit uLoaderTask;

interface

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  System.SysUtils,
  VDAPI;

type
  TSecBase = class;
  TSecBaseArr3 = array [0 .. 2] of TSecBase;

  TSecBase = class
  public
    Name: string;
    VA: TVA;
    VirtSize: TVDSectionSize;
    Flags: TVDSectionFlags;
    IsConflict: boolean; // Section conflicts with other section (overlaps it).
    function EndVA: TVA; inline;
    function Overlaps(const Sec: TSecBase): boolean; inline;
    function Clone: TSecBase; virtual;

    // Split into 2 or 3 sections.
    // va0<va1
    // procedure Split2(va0, va1: TVA; out s: TSecBaseArr3);
  end;

  TSecFromFile = class(TSecBase)
  private
    function GetMapped: boolean;
    procedure SetMapped(const Value: boolean);
  public
    FileName: string;
    Offset: UInt64;
    RawSize: TVDSectionSize; // if Mapped to file RawSize = VirtSize
    property Mapped: boolean read GetMapped write SetMapped;
    function Clone: TSecBase; override;
  end;

  TSecList = TObjectList<TSecBase>;

  TVDLoaderTask = class(TInterfacedObject, IVDLoaderTask)
  public
    SecList: TSecList;
    constructor Create;
    destructor Destroy; override;

    procedure SortSectionsByVa;
  public
    Entry: TVA;
    Endianness: TEndianness;
    AddressSize: int; // in bytes: 2,4,8
    CpuName: string;

    procedure SetEntry(Value: TVA); stdcall;
    procedure SetEndianness(Value: TEndianness); stdcall;
    procedure SetAddressSize(Value: int); stdcall;
    procedure SetCpuName(Value: BSTR_IN); stdcall;

    // function AddModule(Path: BSTR_IN; ImageBase: TVA; ImageSize: UInt64): IVDModule; stdcall;

    // Sections may be added unsorted.
    // They are sorted during checks.
    procedure AddSectionFromFile(FileName, Name: BSTR_IN; Offset: UInt64;
      RawSize: TVDSectionSize; VirtSize: TVDSectionSize; VA: TVA;
      Flags: TVDSectionFlags); stdcall;
    procedure AddSectionFromMappedFile(FileName, Name: BSTR_IN; Offset: UInt64;
      Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags); stdcall;
  end;

  // Call it after sections sorted.
procedure FindSectionConflicts(const Task: TVDLoaderTask);

implementation

uses
  uModule;

{ TVDLoaderTask }

constructor TVDLoaderTask.Create;
begin
  inherited;
  self.SecList := TSecList.Create();
end;

destructor TVDLoaderTask.Destroy;
begin
  self.SecList.Free;
  inherited;
end;

procedure TVDLoaderTask.SetEntry(Value: TVA);
begin
  self.Entry := Value;
end;

procedure TVDLoaderTask.SortSectionsByVa;
begin
  self.SecList.Sort(TComparer<TSecBase>.Construct(
    function(const left, right: TSecBase): integer
    begin
      if left.VA > right.VA then
        result := 1
      else if left.VA < right.VA then
        result := -1
      else
        result := 0;
    end));
end;

procedure TVDLoaderTask.SetEndianness(Value: TEndianness);
begin
  self.Endianness := Value;
end;

procedure TVDLoaderTask.SetAddressSize(Value: int);
begin
  self.AddressSize := Value;
end;

procedure TVDLoaderTask.SetCpuName(Value: BSTR_IN);
begin
  self.CpuName := Value;
end;

// function TVDLoaderTask.AddModule(Path: BSTR_IN; ImageBase: TVA; ImageSize: UInt64): IVDModule;
// begin
// result := TVDModule.Create(Path, ImageBase, ImageSize);
// end;

procedure TVDLoaderTask.AddSectionFromFile(FileName, Name: BSTR_IN; Offset: UInt64;
RawSize, VirtSize: TVDSectionSize; VA: TVA;
Flags: TVDSectionFlags);
var
  Sec: TSecFromFile;
begin
  Sec := TSecFromFile.Create;

  Sec.VA := VA;
  Sec.VirtSize := VirtSize;

  Sec.Mapped := False;
  Sec.FileName := FileName;
  Sec.Name := Name;
  Sec.Offset := Offset;
  Sec.RawSize := RawSize;
  Sec.Flags := Flags;

  self.SecList.Add(Sec);
end;

procedure TVDLoaderTask.AddSectionFromMappedFile(FileName, Name: BSTR_IN;
Offset: UInt64; Size: TVDSectionSize; VA: TVA; Flags: TVDSectionFlags);
var
  Sec: TSecFromFile;
begin
  Sec := TSecFromFile.Create;

  Sec.VA := VA;
  Sec.VirtSize := Size;

  Sec.FileName := FileName;
  Sec.Name := Name;
  Sec.Offset := Offset;
  Sec.RawSize := Size;
  Sec.Flags := Flags;

  // Mapped must be set after initial Flags already set.
  Sec.Mapped := True;

  self.SecList.Add(Sec);
end;

{ TSecBase }

function TSecBase.EndVA: TVA;
begin
  result := self.VA + self.VirtSize;
end;

function TSecBase.Overlaps(const Sec: TSecBase): boolean;
begin
  result := not(
    (self.VA >= Sec.VA + Sec.VirtSize) or
    (self.VA + self.VirtSize <= Sec.VA));
end;

function TSecBase.Clone: TSecBase;
begin
  result := TSecBase(self.ClassType.Create);

  result.Name := self.Name;
  result.VA := self.VA;
  result.VirtSize := self.VirtSize;
  result.Flags := self.Flags;
  result.IsConflict := self.IsConflict;
end;

{ TSecFromFile }

function TSecFromFile.Clone: TSecBase;
begin
  result := inherited Clone;
  TSecFromFile(result).Mapped := self.Mapped;
  TSecFromFile(result).FileName := self.FileName;
  TSecFromFile(result).Offset := self.Offset;
  TSecFromFile(result).RawSize := self.RawSize;
end;

// Solve section conflict on sections sorted by VA.
// Split 2 overlapped sections into 2 or 3 (modify existing and insert new section).
// iSec and next section are conflicted.
// Result is True if there is conflict.
procedure SolveSectionConflicts(const List: TSecList; iSec: integer);
var
  s0, s1: TSecBase;
  s0EndVA, s1EndVA: TVA;
  s: TSecBaseArr3;
  i: integer;
//  sameLeft, sameRight: boolean;
  procedure safe_add(const ns: TSecBase; VA: TVA; Size: TVDSectionSize);
  var
    cloned: TSecBase;
  begin
    if Size <> 0 then
    begin
      cloned := ns.Clone;
      cloned.VA := VA;
      cloned.VirtSize := Size;
      s[i] := cloned;
      inc(i);
    end;
  end;

begin
  s0 := List[iSec];
  s1 := List[iSec + 1];

  // s1 can start either at same va as s0 or higher.
  exit;

  i := 0;
  s[0] := nil;
  s[1] := nil;
  s[2] := nil;

//  sameLeft := s0.VA = s1.VA;
//  sameRight := s0.EndVA = s1.EndVA;

  s0EndVA := s0.EndVA;
  s1EndVA := s1.EndVA;

{$IFDEF DEBUG}
  if s0.VA > s1.VA then
    raise Exception.Create('First address must be <= than second address');
{$ENDIF}
  // Check if s1 overwrites s0 (full overlap)
  if (s0.VA >= s1.VA) and (s0EndVA <= s1EndVA) then
  begin
    // * |aaaaaa|     |aa|     |aaa|
    // * |bbbbbb|  |bbbbbbbb|  |bbbbbbbb|
    // * |bbbbbb|  |bbbbbbbb|  |bbbbbbbb|
    s[0] := s1;
  end
  else
  begin
    // * |aaaaaa|        |aaaaaa|  |aaaaaa|  |aaaaaa|          |aaaaaaaa|
    // *    |bbbbbb|  |bbbbbb|     |bbb|        |bbb|             |bb|
    // * |aa|bbbbbb|  |bbbbbb|aa|  |bbb|aa|  |aa|bbb|          |aa|bb|aa|
    safe_add(s0, s1.VA, s1.VA - s0.VA);
  end;

  // Delete this and next.
  List.DeleteRange(iSec, 2);

  // Replace with splitted.
  for i := high(s) downto low(s) do
    if s[i] <> nil then
      List.Insert(iSec, s[i]);
end;

procedure FindSectionConflicts(const Task: TVDLoaderTask);
var
  i: integer;
begin
  i := 0;
  while i < Task.SecList.Count - 1 do
  begin
    if Task.SecList[i].Overlaps(Task.SecList[i + 1]) then
    begin
      // Both are in conflicted state.
      Task.SecList[i + 0].IsConflict := True;
      Task.SecList[i + 1].IsConflict := True;
      SolveSectionConflicts(Task.SecList, i);
    end;
    inc(i);
  end;
end;

function TSecFromFile.GetMapped: boolean;
begin
  result := (self.Flags and TVDSectionFlag.FileMapped) <> 0;
end;

procedure TSecFromFile.SetMapped(const Value: boolean);
begin
  if Value then
    self.Flags := self.Flags or TVDSectionFlag.FileMapped
  else
    self.Flags := self.Flags and (not TVDSectionFlag.FileMapped);
end;

end.
