{
  Hex Editor

  todos:
  - need speed optimizations
}
unit uHexEdit;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.SysUtils,
  System.Types,

  uDisAsmText,

{$IFDEF VCL}
  WinApi.Windows, // WMGetDlgCode...
  WinApi.Messages,
  Vcl.Graphics,
{$ENDIF}
  VDAPI,

  uVAColorText,
  uColorText.Selection;

type
  TVDHexEdit = class(TVDCustomDisasmText)
  private type
    TBlockType = (block_none, block_va, block_hex, block_chars);

    // User normally modifies small blocks of bytes, thus dictionary is ok
    // to store modifications.
    TModifiedByte = TPair<byte, boolean>; // byte, saved
    TModifiedBytes = TDictionary<TVA, TModifiedByte>;
  private
    FBlock: TBlockType;
    FBlockWhereSelectionStarted: TBlockType;
  private
    FHexX: integer;
    FCharsX: integer;
    FAfterCharsX: integer;
    FColumnId: integer;
    FColumnOfs: integer;
    FModifiedBytes: TModifiedBytes;
    procedure SetColumnCount(const Value: integer);
    procedure SetBlock(const Value: TBlockType);
    procedure SetColumnId(const Value: integer);
    // Set column and row at once.
    procedure SetColumnIdRow(const Col, Row: integer);
    function GetModified: boolean; inline;
    procedure SetColumnOfs(const Value: integer);

    procedure MakeHexColumnCoordinates;
  protected
    FColumnCount: integer;

    // "cached" value of GetCursorVA
    FCursorVA: TVA;
    FUserVA: TVA;

    procedure GrabTextSize; override;

    procedure CreateLine(const c: IVDCore; lpVA: PVA;
      const Layout: IVDVATextLayout); override;

    procedure DoDraw; override;

    function GetScreenVA(out Pos: TVDVAPos): boolean; override;
    function GetUserVA(out VA: TVA): boolean; override;

    procedure ChangeCaret(const Pos: TPoint; dX, dY: integer); override;

    // If col=-1, it's unavailable (e.g. va block)
    function ColToBlockInfo(X: integer; out Col, ColOfs: integer): TBlockType;
    function ColBlockToX(Col, Ofs: integer; Block: TBlockType): integer;
{$IFDEF VCL}
    procedure WMGetDlgCode(var Message: TWMNoParams); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
{$ELSE}
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
{$ENDIF}
    // Virtual Bytes (real/modifed).
    function FetchVirtualByte(VA: TVA; out Value: TModifiedByte): boolean;
    function PutVirtualByte(VA: TVA; Value: byte): boolean;
    procedure RestoreVirtualByte(VA: TVA);

    procedure CaretMoved; override;
    procedure DoEnter; override;

    procedure PaintSelection(const Rect: TRect; BlockMode: boolean); override;
    procedure Paint; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetCursorVA: TVA; override;

    procedure ToggleBlock;

    procedure GoHome(var Pos: TPoint); override;
    procedure GoEnd(var Pos: TPoint); override;

    function DoScrollOneLine(dY: integer; var VA: TVA): boolean; override;

    // Advance cursor 1 step.
    procedure AdvanceCursor;

    procedure SaveModifications;
    procedure DiscardModifications;

    procedure SelectionStart(const CellPos: TPoint); override;
    procedure SelectionUpdate(const Pos: TPoint; BlockMode: boolean); override;

    procedure GoTop(var Pos: TPoint); override;
    procedure GoBottom(var Pos: TPoint); override;
    procedure GoPageUp(var Pos: TPoint); override;
    procedure GoPageDown(var Pos: TPoint); override;

    property ColumnCount: integer read FColumnCount write SetColumnCount;
    property ColumnId: integer read FColumnId write SetColumnId;
    property ColumnOfs: integer read FColumnOfs write SetColumnOfs;
    property Block: TBlockType read FBlock write SetBlock;
    property Modified: boolean read GetModified;
  end;

implementation

uses
  System.UITypes,

  uColorText.Types,
  uColorText;

const
  MIN_COLUMNS          = 4;
  MAX_COLUMNS          = 32;
  DEF_COLUMNS          = 16;
  MAX_COLUMNS_POSSIBLE = 32;
  LINE_PER_COLUMNS     = 4; // each 4 columns - vertical line
  CHARS_PER_HEX_COLUMN = 3;

const
  DEF_COLOR_LINES    = TAlphaColorRec.Lightgray;
  IDR_HEX: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Black);
  IDR_CHR: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Black);
  IDR_MOD: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Blue);

function IsAsciiChar(b: byte): boolean; overload; inline;
begin
  result := b in [32 .. 127];
end;

function IsAsciiChar(c: Char): boolean; overload; inline;
begin
  result := IsAsciiChar(byte(c));
end;

function AsSafeAsciiChar(b: byte): Char; inline;
begin
  if IsAsciiChar(b) then
    result := Char(b)
  else
    result := '.';
end;

{ TVDHexEdit }

procedure TVDHexEdit.AdvanceCursor;
var
  IsLastColumn: boolean;
  tmpVA: TVA;
begin
  IsLastColumn := FColumnId = FColumnCount - 1;
  case Block of
    block_hex, block_chars:
      begin
        if (Block = block_hex) and (FColumnOfs = 0) then
        begin
          ColumnOfs := 1;
        end
        else
        begin
          tmpVA := Self.FCursorVA;
          if CoreGet().Decoder.AddressStep(@tmpVA, 1) = 1 then
          begin
            if not IsLastColumn then
              ColumnId := ColumnId + 1
            else
            begin
              // It's last column.
              SetColumnIdRow(0, CaretY + 1);
            end;
          end;
        end;
      end;
  end;
end;

procedure TVDHexEdit.CaretMoved;
begin
  inherited;
  FUserVA := CursorVA;
end;

procedure TVDHexEdit.ChangeCaret(const Pos: TPoint; dX, dY: integer);
var
  TmpPos: TPoint;
begin
  TmpPos := Pos;
  if TmpPos.X < FHexX then
    TmpPos.X := FHexX
  else if TmpPos.X > (FAfterCharsX - 2) then
    TmpPos.X := FAfterCharsX - 2;

  FBlock := ColToBlockInfo(TmpPos.X, FColumnId, FColumnOfs);

  case FBlock of
    block_hex:
      begin
        case FColumnOfs of
          2:
            if dX < 0 then
            begin
              FColumnOfs := 1;
              TmpPos.X := ColBlockToX(FColumnId, FColumnOfs, FBlock)
            end
            else
            begin
              if FColumnId <> (FColumnCount - 1) then
              begin
                inc(FColumnId);
                FColumnOfs := 0;
                TmpPos.X := ColBlockToX(FColumnId, FColumnOfs, FBlock);
              end
              else
              begin
                FColumnId := 0;
                FColumnOfs := 0;
                TmpPos.X := FCharsX;
                FBlock := block_chars;
              end;
            end;
        end;
      end;
  end;

  inherited ChangeCaret(TmpPos, dX, dY);

  DoVaChanged;
end;

function TVDHexEdit.ColToBlockInfo(X: integer; out Col, ColOfs: integer): TBlockType;
begin
  Col := -1;
  ColOfs := 0;
  if (X >= 0) and (X < Self.FHexX) then
  begin
    result := block_va;
  end
  else if (X < Self.FCharsX) then
  begin
    result := block_hex;
    Col := (X - FHexX) div CHARS_PER_HEX_COLUMN;
    ColOfs := (X - FHexX) mod CHARS_PER_HEX_COLUMN;
  end
  else if (X < Self.FAfterCharsX) then
  begin
    result := block_chars;
    Col := (X - FCharsX);
  end
  else
  begin
    result := block_none;
  end;
end;

function TVDHexEdit.ColBlockToX(Col, Ofs: integer; Block: TBlockType): integer;
begin
  result := -1;
  case Block of
    block_va:
      result := Col + Ofs;
    block_hex:
      result := FHexX + Col * 3 + Ofs;
    block_chars:
      result := FCharsX + Col + Ofs;
  end;
end;

constructor TVDHexEdit.Create(AOwner: TComponent);
begin
  inherited;

  ColumnCount := DEF_COLUMNS;
  EnableHighlighting := False;
  FModifiedBytes := TModifiedBytes.Create();
  FNeedUpdateCoreVAPos := False;

  // Colors.
  SetId(integer(TAGID_HEX_BYTE), IDR_HEX);
  SetId(integer(TAGID_HEX_CHR), IDR_CHR);
  SetId(integer(TAGID_HEX_MOD), IDR_MOD);
end;

procedure TVDHexEdit.CreateLine(const c: IVDCore; lpVA: PVA;
  const Layout: IVDVATextLayout);
var
  i: integer;
  Pair: TModifiedByte;
  stepped: SIZE_T;
  Chars: TList<TModifiedByte>;
begin
  Layout.AddText(BSTR_IN(c.Decoder.HexAddressPrint(lpVA^)), TTag.TAGID_VA);
  Layout.AddText(' ', TTag.TAGID_NONE);

  i := 0;
  Chars := TList<TModifiedByte>.Create;

  try

    // Add hex to layout and prepare chars.
    while i < FColumnCount do
    begin
      if FetchVirtualByte(lpVA^, Pair) then
      begin
        if Pair.Value then
          // Saved
          Layout.AddText(BSTR_IN(Format('%2.2x', [Pair.Key])), TAGID_HEX_BYTE)
        else
          // Modified
          Layout.AddText(BSTR_IN(Format('%2.2x', [Pair.Key])), TAGID_HEX_MOD);

        Layout.Skip;

        Chars.Add(Pair);
      end
      else
        break;

      inc(i);
      stepped := FCore.Decoder.AddressStep(lpVA, 1);
      if stepped = 0 then
      begin
        inc(lpVA^); // make it invalid, and next lines will not be drawn
        break;
      end;
    end;

    i := (FColumnCount - i) * 3;
    if i <> 0 then
      Layout.AddText(BSTR_IN(string.Create(' ', i)), TTag.TAGID_NONE);

    // Add Chars
    for Pair in Chars do
    begin
      if Pair.Value then
        // Saved
        Layout.AddChar(AsSafeAsciiChar(Pair.Key), TAGID_HEX_CHR)
      else
        // Modified
        Layout.AddChar(AsSafeAsciiChar(Pair.Key), TAGID_HEX_MOD);
    end;

  finally
    Chars.Free;
  end;
end;

destructor TVDHexEdit.Destroy;
begin
  FModifiedBytes.Free;
  inherited;
end;

procedure TVDHexEdit.DiscardModifications;
begin
  if Modified then
  begin
    FModifiedBytes.Clear;
    InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TVDHexEdit.DoDraw;
var
  X: integer;
  xClientHeight: TFmCoord;
begin
  inherited;

{$IFDEF VCL}
  FTargetCanvas.Pen.Color := VclFmxColor(DEF_COLOR_LINES);
{$ELSE}
TODO:
  TVDHexEdit.DoDraw
{$ENDIF}
    MakeHexColumnCoordinates;

  xClientHeight := Self.Height;

  X := FHexX;
  while X < FCharsX do
  begin
    FDraw.DrawVLine(FTargetCanvas, X, nCharX, xClientHeight, CellCoordsToPoint);
    inc(X, LINE_PER_COLUMNS * CHARS_PER_HEX_COLUMN);
  end;

  FDraw.DrawVLine(FTargetCanvas, FCharsX, nCharX, xClientHeight, CellCoordsToPoint);
  FDraw.DrawVLine(FTargetCanvas, FAfterCharsX, nCharX, xClientHeight, CellCoordsToPoint);

  if Selection.IsActive then
    DrawSelectionAddresses;
end;

procedure TVDHexEdit.DoEnter;
begin
  inherited;
  MakeHexColumnCoordinates;
end;

function TVDHexEdit.DoScrollOneLine(dY: integer; var VA: TVA): boolean;
var
  val: integer;
begin
  val := FColumnCount;
  result := FCore.Decoder.AddressStep(@VA, dY * val) = val;
  if result then
    InvalidateAllRowsWithRebuilding;
end;

function TVDHexEdit.FetchVirtualByte(VA: TVA; out Value: TModifiedByte): boolean;
var
  b: byte;
begin
  if FModifiedBytes.TryGetValue(VA, Value) then
  begin
    Exit(True);
  end;
  // No modified values, read original.
  if FCore.VM.Read(VA, @b, 1) = 1 then
  begin
    Value.Key := b;
    Value.Value := True; // saved
    Exit(True);
  end;
  Exit(False);
end;

function TVDHexEdit.GetCursorVA: TVA;
begin
  result := inherited GetCursorVA;
  if result = BAD_VA then
  begin
    FCursorVA := result;
    Exit;
  end;
  CoreGet().Decoder.AddressStep(@result, FColumnId);
  FCursorVA := result;
end;

function TVDHexEdit.GetModified: boolean;
begin
  result := FModifiedBytes.Count <> 0;
end;

function TVDHexEdit.GetScreenVA(out Pos: TVDVAPos): boolean;
begin
  Pos.ScrVA := Self.FVA;

  if LineCount <> 0 then
    Pos.CurVA := Self.CursorVA
  else
    Pos.CurVA := BAD_VA;

  Pos.X := CaretX;
  Exit(True);
end;

function TVDHexEdit.GetUserVA(out VA: TVA): boolean;
var
  tmp: TVA;
begin
  tmp := FUserVA;

  result := tmp <> BAD_VA;
  if result then
    VA := tmp;
end;

procedure TVDHexEdit.GoEnd(var Pos: TPoint);
begin
  ColumnId := FColumnCount - 1;
end;

procedure TVDHexEdit.GoHome(var Pos: TPoint);
begin
  ColumnId := 0;
end;

procedure TVDHexEdit.GoTop(var Pos: TPoint);
var
  tmp: TVA;
begin
  if FCore.VM.GetFirstVA(@tmp) then
  begin
    Self.VA := tmp;
    DoVaChanged;
    InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TVDHexEdit.GoBottom(var Pos: TPoint);
var
  tmp: TVA;
begin
  if FCore.VM.GetLastVA(@tmp) then
  begin
    Self.VA := tmp;
    DoVaChanged;
    InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TVDHexEdit.GoPageUp(var Pos: TPoint);
begin
  inherited;
  DoVaChanged;
  InvalidateAllRowsWithRebuilding;
end;

procedure TVDHexEdit.GoPageDown(var Pos: TPoint);
begin
  inherited;
  DoVaChanged;
  InvalidateAllRowsWithRebuilding;
end;

procedure TVDHexEdit.GrabTextSize;
begin
  inherited;
  CaretWidth := nCharX;
end;

function HexCharToByte(c: Char; out b: byte): boolean;
begin
  if CharInSet(c, ['a' .. 'f']) then
  begin
    b := ord(c) - ord('a') + 10;
    Exit(True);
  end;
  if CharInSet(c, ['A' .. 'F']) then
  begin
    b := ord(c) - ord('A') + 10;
    Exit(True);
  end;
  if CharInSet(c, ['0' .. '9']) then
  begin
    b := ord(c) - ord('0');
    Exit(True);
  end;
  Exit(False);
end;

{$IFDEF VCL}


procedure TVDHexEdit.KeyDown(var Key: Word; Shift: TShiftState);
{$ELSE}


procedure TVDHexEdit.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
begin
  if Shift = [] then
  begin
    case Key of
      vkDelete:
        begin
          RestoreVirtualByte(FCursorVA);
          InvalidateCurrentLine;
          Exit;
        end;
      vkRight:
        begin
          AdvanceCursor;
          Exit;
        end;
      vkSpace:
        begin
          if Block = block_hex then
            AdvanceCursor;
          Exit;
        end;
      vkTab:
        begin
          ToggleBlock;
          Exit;
        end;
    end;
  end;

  inherited;
end;

{$IFDEF VCL}


procedure TVDHexEdit.KeyPress(var Key: Char);
var
  b, hx: byte;
  Pair: TModifiedByte;
begin
  inherited;

  // todo: Hex now does InvalidateAllRowsWithRebuilding instead of current line.
  // the problem is it need rebuilding line.

  case Block of
    block_hex:
      begin
        if (FColumnOfs in [0, 1]) and HexCharToByte(Key, hx) and
          FetchVirtualByte(FCursorVA, Pair) then
        begin
          b := Pair.Key;
          case FColumnOfs of
            0:
              b := (b and $0F) or (hx shl 4);
            1:
              b := (b and $F0) or (hx);
          end;
          PutVirtualByte(FCursorVA, b);
          InvalidateAllRowsWithRebuilding;
          AdvanceCursor;
        end;
      end;
    block_chars:
      begin
        if IsAsciiChar(Key) then
        begin
          PutVirtualByte(FCursorVA, byte(Key));
          InvalidateAllRowsWithRebuilding;
          AdvanceCursor;
        end;
      end;
  end;
end;
{$ELSE}
// TODO: IMPLEMENT "TVDHexEdit.KeyPress"
{$ENDIF}


procedure TVDHexEdit.MakeHexColumnCoordinates;
begin
  if FControlIsReady then
  begin
    FHexX := FCore.Decoder.HexAddressDigitCount() + 1;
    FCharsX := FHexX + CHARS_PER_HEX_COLUMN * FColumnCount;
    FAfterCharsX := FCharsX + FColumnCount + 1;
  end;
end;

procedure TVDHexEdit.Paint;
begin
  inherited;
end;

procedure TVDHexEdit.PaintSelection(const Rect: TRect; BlockMode: boolean);
var
  r: TRect;
  leftOfsHex, rightOfsHex: integer;
  leftOfsChars, rightOfsChars: integer;
begin
  r := Rect;

  case FBlockWhereSelectionStarted of
    block_hex:
      begin
        leftOfsHex := r.Left - FHexX;
        rightOfsHex := r.Right - FHexX;

        // Get offset for chars.
        leftOfsChars := leftOfsHex div CHARS_PER_HEX_COLUMN;
        rightOfsChars := rightOfsHex div CHARS_PER_HEX_COLUMN;

        // Align left and right.
        r.Left := FHexX + (leftOfsHex div CHARS_PER_HEX_COLUMN) * CHARS_PER_HEX_COLUMN;
        r.Right := FHexX + ((rightOfsHex div CHARS_PER_HEX_COLUMN) + 1) * CHARS_PER_HEX_COLUMN - 1;

        // Paint hex-selection.
        inherited PaintSelectionEx(r, BlockMode, FHexX, FCharsX - 1);

        // Paint chars-selection.
        r.Left := FCharsX + leftOfsChars;
        r.Right := FCharsX + rightOfsChars + 1;
        inherited PaintSelectionEx(r, BlockMode, FCharsX, FAfterCharsX - 1);
      end;
    block_chars:
      begin
        // We know offsets in "chars" field.
        leftOfsChars := r.Left - FCharsX;
        rightOfsChars := r.Right - FCharsX;

        // Paint chars-selection.
        inc(r.Right);
        inherited PaintSelectionEx(r, BlockMode, FCharsX, FAfterCharsX - 1);

        // Paint hex-selection.
        r.Left := FHexX + CHARS_PER_HEX_COLUMN * leftOfsChars;
        r.Right := FHexX + CHARS_PER_HEX_COLUMN * (rightOfsChars + 1) - 1;
        inherited PaintSelectionEx(r, BlockMode, FHexX, FCharsX - 1);
      end
  else
    raise Exception.Create('Selection block is not supported');
  end;
end;

function TVDHexEdit.PutVirtualByte(VA: TVA; Value: byte): boolean;
var
  Pair: TModifiedByte;
begin
  if FModifiedBytes.TryGetValue(VA, Pair) then
  begin
    if Pair.Key <> Value then
      FModifiedBytes[VA] := TModifiedByte.Create(Value, False);
  end
  else
    FModifiedBytes.Add(VA, TModifiedByte.Create(Value, False));
  Exit(True);
end;

procedure TVDHexEdit.RestoreVirtualByte(VA: TVA);
begin
  FModifiedBytes.Remove(VA);
end;

procedure TVDHexEdit.SaveModifications;
var
  c: IVDCore;
  Pair: TPair<TVA, TModifiedByte>;
  Done: TList<TVA>;
  VA: TVA;
begin
  if not Modified then
    Exit;
  c := CoreGet;
  Done := TList<TVA>.Create;
  try
    for Pair in FModifiedBytes do
    begin
      if c.VM.Write(Pair.Key, @Pair.Value, 1) = 1 then
        Done.Add(Pair.Key);
    end;
    for VA in Done do
      FModifiedBytes.Remove(VA);
    if Done.Count <> 0 then
    begin
      c.Msg.Broadcast(TVDMessage.MSG_VM_WRITE_DONE);
      InvalidateAllRowsWithRebuilding;
    end;
    // Save sections
    c.VM.Sections.Flush;
  finally
    Done.Free;
  end;
end;

procedure TVDHexEdit.SelectionStart(const CellPos: TPoint);
begin
  inherited;
  FBlockWhereSelectionStarted := FBlock;
end;

procedure TVDHexEdit.SelectionUpdate(const Pos: TPoint; BlockMode: boolean);
begin
  // Don't allow updating selection from different block.
  if FBlockWhereSelectionStarted <> FBlock then
    Exit;

  inherited;
end;

procedure TVDHexEdit.SetBlock(const Value: TBlockType);
begin
  if Value <> FBlock then
    case Value of
      block_hex, block_chars:
        begin
          FBlock := Value;
          CaretX := ColBlockToX(FColumnId, 0, Value);
        end;
    end;
end;

procedure TVDHexEdit.SetColumnCount(const Value: integer);
begin
  if FColumnCount < MIN_COLUMNS then
    FColumnCount := MIN_COLUMNS
  else if FColumnCount > MAX_COLUMNS then
    FColumnCount := MAX_COLUMNS;
  if FColumnCount <> Value then
  begin
    FColumnCount := Value;

    MakeHexColumnCoordinates;

    if CaretX < FHexX then
      CaretX := FHexX;

    Self.InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TVDHexEdit.SetColumnId(const Value: integer);
begin
  SetColumnIdRow(Value, CaretY);
end;

procedure TVDHexEdit.SetColumnIdRow(const Col, Row: integer);
begin
  if (FColumnId <> Col) or (CaretY <> Row) then
  begin
    CaretPos := Point(ColBlockToX(Col, 0, FBlock), Row);
    FColumnId := Col;
    FColumnOfs := 0;
  end;
end;

procedure TVDHexEdit.SetColumnOfs(const Value: integer);
begin
  if (Block = block_hex) and (Value < 2) then
    FColumnOfs := Value
  else
    FColumnOfs := 0;
  CaretX := ColBlockToX(FColumnId, FColumnOfs, FBlock);
end;

procedure TVDHexEdit.ToggleBlock;
begin
  case Block of
    block_hex:
      Block := block_chars;
    block_chars:
      Block := block_hex;
  end;
end;

{$IFDEF VCL}


procedure TVDHexEdit.WMGetDlgCode(var Message: TWMNoParams);
begin
  Message.result := Message.result or DLGC_WANTTAB or DLGC_WANTARROWS;
end;
{$ENDIF}

end.
