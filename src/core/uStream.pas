{
  Endianness-aware stream reader/writer.
}
unit uStream;

interface

uses
  System.Classes,
  System.SysUtils,

  uCore.Strings,

  uDB,
  VDAPI;

const
  DEFAULT_ENDIANNESS = TEndianness.Big;

type

  // todo: assign FStream on create

  TVDStreamIO = class(TInterfacedObject, IVDStreamIO)
  private
    FStream: IVDBaseStream;

    // stream -> FEndiannessInternal
    // FEndiannessInternal -> stream
    FStreamEndianness: TEndianness;
    FSysEndianness: TEndianness;

    procedure SafeRead(var Buf; Size: UInt32); inline;
    function GetSizeToEnd: integer; inline;
  protected
    // currently not used
    FReadOnly: boolean;
  public
    constructor Create(
      const Stream: IVDBaseStream;
      Endianness: TEndianness = DEFAULT_ENDIANNESS);

    procedure StrToBytes(const Value: string; out Bytes: TBytes);
    procedure BytesToStr(const Bytes: TBytes; out Str: string);

    procedure WriteStr(const Value: string; WriteSize: boolean = True);
    procedure WriteTag(Tag: TDBTagCast);
    function ReadStr(ReadSize: boolean = True): String;
    function ReadTag: TDBTagCast;

    function ToBytes: TBytes;
  public

    { IVDBaseStream }

    function GetSize: UInt64; stdcall;
    function SetSize(Value: UInt64): BOOL; stdcall;
    function GetPosition: UInt64; stdcall;
    procedure SetPosition(Value: UInt64); stdcall;
    function Read(var Buffer; Size: UInt32): UInt32; stdcall;
    function Write(const Buffer; Size: UInt32): UInt32; stdcall;

    { IVDStreamIO }

    function ReadWord(WordSize: uint8): UInt64;
    procedure WriteWord(WordSize: uint8; Value: UInt64);

    function WriteBuf(Buf: Pointer; Size: UInt32): UInt32; stdcall;
    procedure WriteU8(Value: uint8); stdcall;
    procedure WriteU16(Value: uint16); stdcall;
    procedure WriteU32(Value: UInt32); stdcall;
    procedure WriteU64(Value: UInt64); stdcall;
    procedure WriteVA(Value: TVA); stdcall;
    procedure WriteSectionSize(Value: TVDSectionSize); stdcall;
    procedure WriteRelVA(const Value: TRelVA); stdcall;
    procedure WriteBSTR(Value: BSTR; WriteSize: BOOL = True); stdcall;

    procedure ReadBuf(Buf: Pointer; Size: UInt32); stdcall;
    function ReadU8: uint8; stdcall;
    function ReadU16: uint16; stdcall;
    function ReadU32: UInt32; stdcall;
    function ReadU64: UInt64; stdcall;
    function ReadVA: TVA; stdcall;
    function ReadSectionSize: TVDSectionSize; stdcall;
    procedure ReadRelVA(out Value: TRelVA); stdcall;
    function ReadBSTR(ReadSize: BOOL = True): BSTR; stdcall;

    function Skip(Size: int = 1): BOOL; stdcall;

    function PeekU8(out Value: uint8): BOOL; stdcall;

    function CopyFrom(Source: IVDStreamIO): SIZE_T; stdcall;

    function Clear: BOOL; stdcall;

    procedure FillByte(Value: uint8; Count: UInt); stdcall;

    property Size: UInt64 read GetSize;
  end;

implementation

uses
  uEndianness;

{ TMemStream }

function TVDStreamIO.GetSize: UInt64;
begin
  Result := FStream.GetSize;
end;

function TVDStreamIO.SetSize(Value: UInt64): BOOL; stdcall;
begin
  Result := FStream.SetSize(Value);
end;

function TVDStreamIO.GetPosition: UInt64;
begin
  Result := FStream.GetPosition;
end;

procedure TVDStreamIO.SetPosition(Value: UInt64); stdcall;
begin
  FStream.SetPosition(Value);
end;

function TVDStreamIO.Read(var Buffer; Size: UInt32): UInt32; stdcall;
begin
  if Size = 0 then
    Exit(0);
  Result := FStream.Read(Buffer, Size);
end;

function TVDStreamIO.Write(const Buffer; Size: UInt32): UInt32; stdcall;
begin
  if Size = 0 then
    Exit(0);
  Result := FStream.Write(Buffer, Size);
end;

function TVDStreamIO.GetSizeToEnd: integer;
begin
  Result := FStream.GetSize - FStream.GetPosition;
end;

function TVDStreamIO.PeekU8(out Value: uint8): BOOL;
var
  cnt: Cardinal;
begin
  cnt := FStream.Read(Value, SizeOf(Value));
  Result := cnt = SizeOf(Value);
  if cnt <> 0 then
    FStream.SetPosition(FStream.GetPosition - cnt);
end;

function TVDStreamIO.ReadBSTR(ReadSize: BOOL): BSTR;
begin
  Result := ReadStr(ReadSize);
end;

procedure TVDStreamIO.ReadBuf(Buf: Pointer; Size: UInt32);
begin
  if FStream.Read(Buf^, Size) <> Size then
    raise Exception.Create('Read Error');
end;

procedure TVDStreamIO.ReadRelVA(out Value: TRelVA);
begin
  Value.SecID := ReadU32;
  Value.SecOfs := ReadSectionSize;
end;

function TVDStreamIO.ReadStr(ReadSize: boolean): String;
var
  Size: uint16;
  Bytes: TBytes;
begin
  if ReadSize then
    Size := ReadU16
  else
    Size := GetSizeToEnd;
  SetLength(Bytes, Size);
  if Size <> 0 then
  begin
    SafeRead(Bytes[0], Size);
    self.BytesToStr(Bytes, Result);
    Exit;
  end;
  Result := '';
end;

function TVDStreamIO.ReadTag: TDBTagCast;
begin
  Result := ReadU8;
end;

function TVDStreamIO.ReadU16: uint16;
begin
  SafeRead(Result, SizeOf(Result));
  BufEndiannessToSys(@Result, SizeOf(Result), FStreamEndianness);
end;

function TVDStreamIO.ReadU32: UInt32;
begin
  SafeRead(Result, SizeOf(Result));
  BufEndiannessToSys(@Result, SizeOf(Result), FStreamEndianness);
end;

function TVDStreamIO.ReadU64: UInt64;
begin
  SafeRead(Result, SizeOf(Result));
  BufEndiannessToSys(@Result, SizeOf(Result), FStreamEndianness);
end;

function TVDStreamIO.ReadU8: uint8;
begin
  SafeRead(Result, SizeOf(Result));
end;

function TVDStreamIO.ReadVA: TVA;
begin
  SafeRead(Result, SizeOf(Result));
  BufEndiannessToSys(@Result, SizeOf(Result), FStreamEndianness);
end;

function TVDStreamIO.ReadSectionSize: TVDSectionSize;
begin
  SafeRead(Result, SizeOf(Result));
  BufEndiannessToSys(@Result, SizeOf(Result), FStreamEndianness);
end;

function TVDStreamIO.ReadWord(WordSize: uint8): UInt64;
begin
  case WordSize of
    1:
      Result := ReadU8;
    2:
      Result := ReadU16;
    4:
      Result := ReadU32;
    8:
      Result := ReadU64;
  else
    raise Exception.CreateFmt('TVDStreamIO.ReadWord, bad word size = %d', [WordSize]);
  end;
end;

procedure TVDStreamIO.WriteWord(WordSize: uint8; Value: UInt64);
begin
  case WordSize of
    1:
      WriteU8(Value);
    2:
      WriteU16(Value);
    4:
      WriteU32(Value);
    8:
      WriteU64(Value);
  else
    raise Exception.Create('Stream error');
  end;
end;

procedure TVDStreamIO.SafeRead(var Buf; Size: UInt32);
begin
  if FStream.Read(Buf, Size) <> Size then
    raise Exception.Create('Read Error.');
end;

function TVDStreamIO.Skip(Size: int): BOOL; stdcall;
var
  newOfs: UInt64;
begin
  newOfs := FStream.GetPosition + Size;
  if newOfs > FStream.GetSize then
    Exit(False);
  FStream.SetPosition(newOfs);
  Result := True;
end;

procedure TVDStreamIO.StrToBytes(const Value: string; out Bytes: TBytes);
begin
  Bytes := TEncoding.UTF8.GetBytes(Value);
end;

function TVDStreamIO.ToBytes: TBytes;
var
  Size: UInt64;
begin
  Size := self.GetSize;
  if Size = 0 then
    Exit(nil);
  SetLength(Result, Size);
  SetPosition(0);
  ReadBuf(@Result[0], Size);
end;

procedure TVDStreamIO.BytesToStr(const Bytes: TBytes; out Str: string);
begin
  Str := TEncoding.UTF8.GetString(Bytes);
end;

procedure TVDStreamIO.WriteBSTR(Value: BSTR; WriteSize: BOOL);
begin
  WriteStr(Value, WriteSize);
end;

function TVDStreamIO.WriteBuf(Buf: Pointer; Size: UInt32): UInt32;
begin
  Result := FStream.Write(Buf^, Size);
end;

procedure TVDStreamIO.WriteRelVA(const Value: TRelVA);
begin
  WriteU32(Value.SecID);
  WriteSectionSize(Value.SecOfs);
end;

procedure TVDStreamIO.WriteStr(const Value: string; WriteSize: boolean);
var
  Size: uint16;
  Bytes: TBytes;
begin
  self.StrToBytes(Value, Bytes);
  Size := Length(Bytes);
  if WriteSize then
    WriteU16(Size);
  if Size <> 0 then
    FStream.Write(Bytes[0], Size);
end;

procedure TVDStreamIO.WriteTag(Tag: TDBTagCast);
begin
  WriteU8(Tag);
end;

procedure TVDStreamIO.WriteU16(Value: uint16);
begin
  BufSysToEndianness(@Value, SizeOf(Value), FStreamEndianness);
  FStream.Write(Value, SizeOf(Value));
end;

procedure TVDStreamIO.WriteU32(Value: UInt32);
begin
  BufSysToEndianness(@Value, SizeOf(Value), FStreamEndianness);
  FStream.Write(Value, SizeOf(Value));
end;

procedure TVDStreamIO.WriteU64(Value: UInt64);
begin
  BufSysToEndianness(@Value, SizeOf(Value), FStreamEndianness);
  FStream.Write(Value, SizeOf(Value));
end;

procedure TVDStreamIO.WriteU8(Value: uint8);
begin
  FStream.Write(Value, SizeOf(Value));
end;

procedure TVDStreamIO.WriteVA(Value: TVA);
begin
  BufSysToEndianness(@Value, SizeOf(Value), FStreamEndianness);
  FStream.Write(Value, SizeOf(Value));
end;

procedure TVDStreamIO.WriteSectionSize(Value: TVDSectionSize);
begin
  BufSysToEndianness(@Value, SizeOf(Value), FStreamEndianness);
  FStream.Write(Value, SizeOf(Value));
end;

function TVDStreamIO.Clear: BOOL;
begin
  Result := FStream.SetSize(0);
end;

function TVDStreamIO.CopyFrom(Source: IVDStreamIO): SIZE_T;
const
  BUF_SIZE = 4096;
var
  Bytes: TBytes;
  cnt: integer;
begin
  Result := 0;
  SetLength(Bytes, BUF_SIZE);
  while True do
  begin
    cnt := Source.Read(Bytes[0], BUF_SIZE);
    if cnt = 0 then
      break;
    Write(Bytes[0], cnt);
    inc(Result, cnt);
  end;
end;

constructor TVDStreamIO.Create(
  const Stream: IVDBaseStream;
  Endianness: TEndianness = DEFAULT_ENDIANNESS);
begin
  inherited Create;

  FStream := Stream;

  // Setup endianness.
  FSysEndianness := VDAPI.GetSystemEndianness;

  if Endianness <> TEndianness.None then
    FStreamEndianness := Endianness
  else
    FStreamEndianness := FSysEndianness;
end;

procedure TVDStreamIO.FillByte(Value: uint8; Count: UInt);
var
  p: Pointer;
begin
  if Count = 0 then
    Exit;
  GetMem(p, Count);
  try
    FillChar(p^, Count, Value);
    if FStream.Write(p^, Count) <> Count then
      raise Exception.Create(SWriteError);
  finally
    FreeMem(p);
  end;
end;

end.
