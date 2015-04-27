unit ELF.StringTable;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils;

type
  TELFStringTableOffsets = TList<integer>;

  TELFStringTable = class
  private
    FData: TBytes;
    FOffsets: TELFStringTableOffsets;
  public
    constructor Create;
    destructor Destroy; override;

    procedure DumpStrings;
    function GetString(Offset: uint32; out str: string): boolean; inline;

    property Offsets: TELFStringTableOffsets read FOffsets;
  end;

function ReadElfStringTable(
  Stream: TStream;
  ElfImage: TObject; // TElfImage
  VA: uint64;
  Size: integer
  ): TELFStringTable;

implementation

uses
  ELF.Image,
  ELF.Helpers;

function ReadElfStringTable;
var
  ofs: uint64;
  p: pbyte;
  left: integer;
begin
  if Size = 0 then
    exit(nil);

  if not TELFImage(ElfImage).VAToOffset(VA, ofs) then
    exit(nil);

  // Load data.
  Stream.Position := ofs;
  Result := TELFStringTable.Create;
  SetLength(Result.FData, Size);
  if Stream.Read(Result.FData, Size) <> Size then
    raise Exception.Create('Read error');

  // Parse offsets.
  ofs := 0;
  left := Size;
  p := @Result.FData[0];
  Result.FOffsets.Add(0);
  while True do
  begin
    if Size = 0 then
      break;
    if p^ = 0 then
      if Size <> 0 then
        Result.FOffsets.Add(ofs + 1);
    inc(p);
    inc(ofs);
    dec(Size);
  end;
  Result.FOffsets.TrimExcess;
end;

{ TELFStringTable }

constructor TELFStringTable.Create;
begin
  inherited Create;
  FOffsets := TELFStringTableOffsets.Create;
end;

destructor TELFStringTable.Destroy;
begin
  FOffsets.Free;
  inherited;
end;

procedure TELFStringTable.DumpStrings;
{$IFDEF CONSOLE}
var
  i: integer;
  str: string;
{$ENDIF}
begin
{$IFDEF CONSOLE}
  writeln('String table:');
  for i := 0 to FOffsets.Count - 1 do
  begin
    GetString(FOffsets[i], str);
    writeln(format('  %-8x "%s"', [FOffsets[i], str]));
  end;
  writeln('-------------------------------------');
{$ENDIF}
end;

function TELFStringTable.GetString(Offset: uint32; out str: string): boolean;
begin
  if Offset >= Length(FData) then
    exit(false);
  str := string(PAnsiChar(@FData[Offset]));
  exit(True);
end;

end.
