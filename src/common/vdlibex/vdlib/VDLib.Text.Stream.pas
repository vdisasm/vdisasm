unit VDLib.Text.Stream;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Character;

type

  {
    * TAbstractTextStream
    * Very base text stream class.
  }

  TAbstractTextStream = class
  public type
    TOffset = type int32;

    TTextRegion = record
      Offset: TOffset;
      Size: integer;
    end;

    PTextRegion = ^TTextRegion;
  protected

    { Abstracts }

    // Get size of stream.
    function GetSize: TOffset; virtual; abstract;

    // Get current position.
    function GetPosition: TOffset; virtual; abstract;

    // Set current  position. If position is out of file, must raise exception.
    procedure SetPosition(const Value: TOffset); virtual;

    // Read a char or raise exception.
    function Read: char; overload; virtual; abstract;

  public

    { Wrappers upon Abstracts }

    procedure Seek(Offset: TOffset; Origin: TSeekOrigin);

    // Peek char.
    function Peek(RelOfs: integer = 0): char; overload; virtual;
    function Peek(out c: char; RelOfs: integer = 0): boolean; overload;

    // Read a char and return true on success.
    function Read(out c: char): boolean; overload;

    // Skip chars.
    // Allows to skip last char and position to point to EOF.
    function Skip(Cnt: integer = 1): boolean; inline;

    // Write text and increase size.
    procedure Write(const Text: string); overload; virtual; abstract;
    procedure Write(const FormatText: string; const Args: array of const); overload;

    property Size: TOffset read GetSize { write SetSize }; // chars
    property Position: TOffset read GetPosition write SetPosition; // chars
  end;

  {
    * TTextStreamBase
    * Extension of TAbstractTextStream.
  }

  TTextStreamBase = class(TAbstractTextStream)
  private
    FSize: TAbstractTextStream.TOffset;
    FPosition: TAbstractTextStream.TOffset;
    FText: string;
  protected
    function GetSize: TAbstractTextStream.TOffset; override;
    function GetPosition: TAbstractTextStream.TOffset; override;
    procedure SetPosition(const Value: TAbstractTextStream.TOffset); override;
    function Read: char; override;
  public
    procedure LoadFromString(const Value: string);

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);

    // Is end of file.
    function IsEOF: boolean; inline;

    procedure Write(const Text: string); override;
  end;

  {
    * TTextStreamEx
    * Extension of TTextStreamBase.
  }
  TTextStreamEx = class(TTextStreamBase)
  protected
    function IsCharOfWord(c: char): boolean; inline;
  public
    function IsDelimiter(c: char): boolean; virtual;
    function IsWhiteSpace(c: char): boolean; virtual;
    function IsLineFeed(c: char): boolean; virtual;

    // Skip whitespaces. If SkipLines is true line breaks will be also skipped.
    procedure SkipWhitespaces(SkipLines: boolean = false);

    // Skip to End Of Line.
    procedure SkipToEOL;
    function ReadToEOL: string;

    // Skip End Of Line.
    procedure SkipEOL;

    // Skip to start of next line.
    procedure SkipLine;

    // Read chars until whitespace or delimiter (leading whitespaces skipped).
    function ReadWord(SkipLines: boolean): string;

    // Read expected word. If word is found, result is true. If no word found,
    // result is false.
    function ReadWordEx(const WordText: string; IgnoreCase: boolean = True): boolean;

    // Read [0-9] chars.
    function ReadDecimalDigits: string;

    // Get word from Pos0 to Pos1 (excluding).
    // If not all positions can be read and RaiseOnPartialRead is true,
    // exception is raised. Otherwise result is partial.
    function ReadTextFromDeltaPos(
      Pos0, Pos1: TAbstractTextStream.TOffset;
      RaiseOnPartialRead: boolean = True): string;

    function ReadTextFromRegion(
      const Region: TAbstractTextStream.TTextRegion;
      RaiseOnPartialRead: boolean = True): string;

    // Scan for Text starting from current position.
    // If text is found, result is True. Position is unchanged.
    function FindNextWord(const Text: string; IgnoreCase: boolean;
      out Pos: TAbstractTextStream.TOffset): boolean;

    // Read text that is terminated with TermText and skip TermText.
    // The TermText will not be includes into result string.
    // Result is False if no TermText found.
    // Lines are skipped automatically.
    // SkipTerminator: if true and terminator found, skip it.
    function ReadTextWithTerminator(
      const TermText: string;
      IgnoreCase: boolean;
      out Text: string): boolean; overload;
  end;

implementation

const
  SReadAfterFileEnd = 'Read after file end';

  { TAbstractTextStream }

function TAbstractTextStream.Peek(RelOfs: integer = 0): char;
begin
  Seek(RelOfs, soCurrent);
  Result := Read();
  Position := Position - RelOfs - 1;
end;

function TAbstractTextStream.Peek(out c: char; RelOfs: integer = 0): boolean;
begin
  if Position + RelOfs < Size then
  begin
    c := Peek(RelOfs);
    exit(True);
  end;
  exit(false);
end;

function TAbstractTextStream.Read(out c: char): boolean;
begin
  if Position < Size then
  begin
    c := Read;
    exit(True);
  end;
  exit(false);
end;

procedure TAbstractTextStream.Seek(Offset: TOffset; Origin: TSeekOrigin);
begin
  case Origin of
    soBeginning:
      Position := Offset;
    soCurrent:
      Position := Position + Offset;
    soEnd:
      Position := Size + Offset;
  end;
end;

procedure TAbstractTextStream.SetPosition(const Value: TOffset);
begin
  // if Value >= GetSize then
  // raise Exception.Create(SReadAfterFileEnd);
end;

function TAbstractTextStream.Skip(Cnt: integer = 1): boolean;
begin
  if Cnt = 0 then
    exit(True);

  if Position + Cnt <= Size then
  begin
    Position := Position + Cnt;
    exit(True);
  end;
  exit(false);
end;

procedure TAbstractTextStream.Write(const FormatText: string;
  const Args: array of const);
begin
  Write(Format(FormatText, Args));
end;

{ TTextStream }

function TTextStreamBase.GetPosition: TAbstractTextStream.TOffset;
begin
  Result := FPosition;
end;

function TTextStreamBase.GetSize: TAbstractTextStream.TOffset;
begin
  Result := FSize;
end;

function TTextStreamBase.IsEOF: boolean;
begin
  Result := (FPosition = FSize);
end;

procedure TTextStreamBase.LoadFromString(const Value: string);
begin
  FText := Value;
  FSize := Length(FText);
end;

procedure TTextStreamBase.LoadFromFile(const FileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    LoadFromString(sl.Text);
  finally
    sl.Free;
  end;
end;

procedure TTextStreamBase.SaveToFile(const FileName: string);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    sl.Text := FText;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

function TTextStreamBase.Read: char;
begin
  if FPosition < FSize then
  begin
    Result := FText[FPosition + 1];
    inc(FPosition);
  end
  else
    raise Exception.Create(SReadAfterFileEnd);
end;

procedure TTextStreamBase.SetPosition(const Value: TAbstractTextStream.TOffset);
begin
  // inherited;
  FPosition := Value;
end;

procedure TTextStreamBase.Write(const Text: string);
var
  NewPos, DstPos: TOffset;
  i: integer;
begin
  NewPos := FPosition + Length(Text);

  if NewPos > FSize then
  begin
    SetLength(FText, NewPos);
    FSize := NewPos;
  end;

  DstPos := FPosition + 1;

  for i := 1 to Length(Text) do
  begin
    FText[DstPos] := Text[i];
    inc(DstPos);
  end;

  FPosition := NewPos;
end;

{ TTextStreamEx }

function TTextStreamEx.IsCharOfWord(c: char): boolean;
begin
  Result := (not IsDelimiter(c)) and (not IsWhiteSpace(c));
end;

function TTextStreamEx.IsDelimiter(c: char): boolean;
begin
  Result := false;
end;

function TTextStreamEx.IsLineFeed(c: char): boolean;
begin
  // It's default, you can subclass and override it.
  Result := CharInSet(c, [#13, #10]);
end;

function TTextStreamEx.IsWhiteSpace(c: char): boolean;
begin
  // It's default, you can subclass and override it.
  Result := CharInSet(c, [#8, #9, #32]);
end;

function TTextStreamEx.ReadDecimalDigits: string;
var
  c: char;
begin
  Result := '';
  while Peek(c) and (CharInSet(c, ['0' .. '9'])) do
    Result := Result + Read();
end;

function TTextStreamEx.FindNextWord(const Text: string; IgnoreCase: boolean;
  out Pos: TAbstractTextStream.TOffset): boolean;
var
  SavePos, Pos1: TOffset;
begin
  Result := false;
  SavePos := self.Position;
  while True do
  begin
    Pos1 := self.Position;
    if ReadWordEx(Text, IgnoreCase) then
    begin
      Pos := Pos1;
      Result := True;
      break;
    end;
    Skip;
  end;
  self.Position := SavePos;
end;

function TTextStreamEx.ReadTextWithTerminator(const TermText: string;
  IgnoreCase: boolean; out Text: string): boolean;
var
  Pos0, Pos1: TOffset;
begin
  Text := '';
  Result := false;
  Pos0 := self.Position;
  if FindNextWord(TermText, IgnoreCase, Pos1) then
  begin
    Text := ReadTextFromDeltaPos(Pos0, Pos1);
    self.Position := Pos1 + Length(TermText);
    exit(True);
  end;
end;

function TTextStreamEx.ReadWord(SkipLines: boolean): string;
var
  c: char;
begin
  Result := '';

  if SkipLines then
    if IsLineFeed(c) then
      SkipEOL;

  SkipWhitespaces;

  if SkipLines then
    if IsLineFeed(c) then
      SkipEOL;

  while Peek(c) and (not IsWhiteSpace(c)) and (not IsLineFeed(c)) and (not IsDelimiter(c)) do
  begin
    Result := Result + c;
    Skip;
  end;
end;

function TTextStreamEx.ReadWordEx(const WordText: string;
  IgnoreCase: boolean): boolean;
var
  c, V: char;
  i: integer;
  oldPos: TOffset;
begin
  Result := false;

  if WordText = '' then
    exit;

  oldPos := FPosition;
  i := 1;
  while Peek(c) do
  begin
    Skip;
    V := WordText[i];
    if IgnoreCase then
    begin
      c := c.ToLower;
      V := V.ToLower;
    end;
    if c <> V then
      break; // fail
    if i = Length(WordText) then
      exit(True);
    inc(i);
  end;

  // fail:
  FPosition := oldPos;
end;

procedure TTextStreamEx.SkipEOL;
var
  c: char;
begin
  while Peek(c) and IsLineFeed(c) do
    if not Skip then
      break;
end;

procedure TTextStreamEx.SkipLine;
begin
  SkipToEOL;
  SkipEOL;
end;

procedure TTextStreamEx.SkipToEOL;
var
  c: char;
begin
  while Peek(c) and (not IsLineFeed(c)) do
    Skip;
end;

function TTextStreamEx.ReadToEOL: string;
var
  c: char;
begin
  Result := '';
  while Peek(c) and (not IsLineFeed(c)) do
  begin
    Result := Result + c;
    Skip;
  end;
end;

procedure TTextStreamEx.SkipWhitespaces(SkipLines: boolean);
var
  c: char;
begin
  while Peek(c) do
  begin
    if IsWhiteSpace(c) or (SkipLines and IsLineFeed(c)) then
    begin
      // Try skip
      if not Skip then
        break; // or break if no more place to skip
    end
    else
      break;
  end;
end;

function TTextStreamEx.ReadTextFromDeltaPos(Pos0, Pos1: TAbstractTextStream.TOffset;
  RaiseOnPartialRead: boolean): string;
var
  curPos: TOffset;
  u, len: uint32;
begin
  if Pos0 > Pos1 then
    raise Exception.Create('Wrong Positions.');

  curPos := self.Position;

  len := Pos1 - Pos0;
  SetLength(Result, len);

  self.Position := Pos0;

  u := 1;
  while u <= len do
  begin
    if (not Read(Result[u])) then
      if RaiseOnPartialRead then
        raise Exception.Create('Read Error.')
      else
        break;
    inc(u);
  end;
  self.Position := curPos;
end;

function TTextStreamEx.ReadTextFromRegion(
  const Region: TAbstractTextStream.TTextRegion;
  RaiseOnPartialRead: boolean): string;
begin
  Result := ReadTextFromDeltaPos(Region.Offset, Region.Offset + Region.Size,
    RaiseOnPartialRead);
end;

end.
