{
  TBufferedStream allows to decrease number of accesses to underlying stream.
}
unit vdBufferedStream;

interface

uses
  System.Classes,
  System.SysUtils;

type
  TBufferedStream = class
  private
    FStream: TStream;
    FBuffer: TBytes;
    FBufferOffset: integer; // current offset in buffer.
    FOffset: UInt64;        // current file offset.
    // Return number of bytes to fill buffer.
    function LeftInBuffer: integer; inline;
  public
    constructor Create(Stream: TStream; BufferSize: integer);
    destructor Destroy; override;
    // Flush buffer to stream.
    procedure Flush;
    // Write buffer.
    procedure Write(const Buf; Size: integer);
    property Offset: UInt64 read FOffset;
  end;

  TBufferedTextStream = class(TBufferedStream)
  private
    FSkipBOM: boolean;
  private const
    MAX_INDENT        = 128;
    INDENT_STEP       = 2;
    INDENT_BYTE: byte = ord(' ');
  private
    FWriteIndent: boolean;
    FIndentLevel: cardinal;
    FIndentBuf: TBytes;
  public
    constructor Create(Stream: TStream; BufferSize: integer);

    procedure WriteChars(const Chars: string); overload;
    procedure WriteChars(const Chars: string; const Args: array of const); overload;

    procedure WriteLn(const Text: string = ''); overload;
    procedure WriteLn(const Text: string; const Args: array of const); overload;

    procedure WriteDump(const Data; Size: integer; Columns: integer = 16);

    procedure IndentEnter; inline;
    procedure IndentLeave; inline;

    property SkipBOM: boolean read FSkipBOM write FSkipBOM;
  end;

  TBufferedTextFileStream = class(TBufferedTextStream)
  public
    constructor Create(const FileName: string; BufferSize: integer = 0);
  end;

  TBufferedTextMemoryStream = class(TBufferedTextStream)
  public
    constructor Create(BufferSize: integer = 0);
    function AsString: string;
  end;

implementation

const
  DEFAULT_BUFFER_SIZE = 4096 * 256;

  { TBufferedStream }

constructor TBufferedStream.Create(Stream: TStream; BufferSize: integer);
begin
  if (Stream = nil) or (BufferSize = 0) then
    raise Exception.Create('Bad input parameters.');
  FStream := Stream;
  SetLength(FBuffer, BufferSize);
end;

destructor TBufferedStream.Destroy;
begin
  Flush;
  FStream.Free;
  inherited;
end;

procedure TBufferedStream.Flush;
begin
  if FBufferOffset <> 0 then
  begin
    FStream.Write(FBuffer[0], FBufferOffset);
    FBufferOffset := 0;
  end;
end;

function TBufferedStream.LeftInBuffer: integer;
begin
  Result := Length(FBuffer) - FBufferOffset;
end;

procedure TBufferedStream.Write(const Buf; Size: integer);
var
  cnt: integer;
  p: pbyte;
begin
  p := pbyte(@Buf);
  while Size > 0 do
  begin
    if FBufferOffset = Length(FBuffer) then
      Flush;
    cnt := LeftInBuffer();
    if Size < cnt then
      cnt := Size;
    Move(p^, FBuffer[FBufferOffset], cnt);
    inc(FOffset, cnt);
    inc(FBufferOffset, cnt);
    inc(p, cnt);
    dec(Size, cnt);
  end;
end;

{ TBufferedTextStream }

procedure TBufferedTextStream.WriteChars(const Chars: string);
const
  UTF8BOM: array [0 .. 2] of byte = ($EF, $BB, $BF);
var
  utf8: RawByteString;
  indentLeft, indentWrite: cardinal;
begin
  if (not FSkipBOM) and (FOffset = 0) then
    self.Write(UTF8BOM[0], 3);

  // write indent
  if FWriteIndent then
  begin
    FWriteIndent := false;
    indentLeft := FIndentLevel;
    while indentLeft > 0 do
    begin
      if indentLeft > MAX_INDENT then
        indentWrite := MAX_INDENT
      else
        indentWrite := indentLeft;

      self.Write(self.FIndentBuf[0], indentWrite);

      dec(indentLeft, indentWrite);
    end;
  end;

  utf8 := UTF8Encode(Chars); // it's faster than TEncoding.UTF8.GetBytes(Chars) at least in XE7
  self.Write(pointer(utf8)^, Length(utf8));
end;

procedure TBufferedTextStream.WriteLn(const Text: string);
const
  lf: byte = 10;
begin
  if Text <> '' then
    self.WriteChars(Text + #10)
  else
    self.Write(lf, 1);
  FWriteIndent := True;
end;

constructor TBufferedTextStream.Create(Stream: TStream; BufferSize: integer);
begin
  inherited Create(Stream, BufferSize);

  // Initialize indent buffer (for faster indent writing)
  SetLength(self.FIndentBuf, MAX_INDENT);
  FillChar(self.FIndentBuf[0], MAX_INDENT, INDENT_BYTE);
  FWriteIndent := True;
end;

procedure TBufferedTextStream.IndentEnter;
begin
  inc(FIndentLevel, INDENT_STEP);
end;

procedure TBufferedTextStream.IndentLeave;
begin
  if FIndentLevel >= INDENT_STEP then
    dec(FIndentLevel, INDENT_STEP)
  else if FIndentLevel <> 0 then
    FIndentLevel := 0;
end;

procedure TBufferedTextStream.WriteChars(const Chars: string;
  const Args: array of const);
begin
  self.WriteChars(format(Chars, Args));
end;

procedure TBufferedTextStream.WriteDump(const Data; Size, Columns: integer);
var
  sHex, sChars: string;
  s: string;
  p: pbyte;
  i, cnt: integer;
begin
  s := '';
  p := @Data;
  while Size <> 0 do
  begin
    if Size > Columns then
      cnt := Columns
    else
      cnt := Size;

    sHex := '';
    sChars := '';
    for i := 0 to cnt - 1 do
    begin
      sHex := sHex + IntToHex(p[i], 2) + ' ';
      if p[i] in [32 .. 126] then
        sChars := sChars + Char(p[i])
      else
        sChars := sChars + '.';
    end;
    for i := 0 to (Columns - cnt) - 1 do
    begin
      sHex := sHex + '   ';
      sChars := sChars + ' ';
    end;
    s := s + sHex + ' ' + sChars + sLineBreak;

    inc(p, cnt);
    dec(Size, cnt);
  end;

  WriteChars(s);
end;

procedure TBufferedTextStream.WriteLn(const Text: string;
  const Args: array of const);
begin
  self.WriteLn(format(Text, Args));
end;

{ TBufferedTextFileStream }

constructor TBufferedTextFileStream.Create(const FileName: string;
  BufferSize: integer);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareDenyWrite);
  if BufferSize = 0 then
    BufferSize := DEFAULT_BUFFER_SIZE;
  inherited Create(Stream, BufferSize);
end;

{ TBufferedTextMemoryStream }

function TBufferedTextMemoryStream.AsString: string;
var
  ms: TMemoryStream;
  bytes: TBytes;
begin
  ms := TMemoryStream(FStream);
  if ms.Size = 0 then
  begin
    Result := '';
    exit;
  end;
  SetLength(bytes, ms.Size);
  Move(ms.Memory^, bytes[0], ms.Size);
  Result := TEncoding.utf8.GetString(bytes);
end;

constructor TBufferedTextMemoryStream.Create(BufferSize: integer = 0);
begin
  if BufferSize = 0 then
    BufferSize := DEFAULT_BUFFER_SIZE;
  inherited Create(TMemoryStream.Create, BufferSize);
end;

end.
