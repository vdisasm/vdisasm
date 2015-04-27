unit uColorText.Types;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Types,
  System.UITypes,

{$IFDEF VCL}
  Vcl.Graphics,
{$ELSE}
  Fmx.Graphics,
{$ENDIF}
  VDAPI;

type
  TColRowCoord = integer;
  TColRowPoint = TPoint;
  TColRowRect = TRect;
{$IFDEF VCL}
  TFmCoord = integer;
  TFmPoint = TPoint;
  TFmRect = TRect;
  TFmBitmap = Vcl.Graphics.TBitmap;
{$ELSE}
  TFmCoord = single;
  TFmPoint = TPointF;
  TFmRect = TRectF;
  TFmBitmap = TBitmap;
{$ENDIF}


const
  ABSOLUTE_OPACITY = 1;

  COMPONENT_PAGE = 'vd2';

  MIN_FONT_SIZE = 7;

  DEFAULT_FONT_STYLE = [];

  DefaultFontName = 'Courier New';
  // DefaultFontName = 'FixedSys';

  DEFAULT_ANTIALIASING = False;
{$IFDEF VCL}
  DEFAULT_FONT_SIZE = 10;
{$ELSE}
  DEFAULT_FONT_SIZE = 15;
{$ENDIF}
  MOUSE_WHEEL_LINES = 4;  // scroll lines per wheel
  DEF_COLUMN_WIDTH  = 13; // used if no FHeaderControl set

  SPLITTER_SET: set of AnsiChar =
    [
    ' ', '!', '#', '$', '(', ')', '"', '[', ']', ',', '.', '+', '-', '*',
    '=', ':', ';', '{', '}', ''''
    ];

type
  TIDRecord = record
    Bg: TAlphaColor; // background color
    Fg: TAlphaColor; // foreground color
    constructor Create(Bg, Fg: TAlphaColor); overload;
    function IsNull: boolean; inline;
    class operator Equal(const a, b: TIDRecord): boolean; static; inline;
    class operator NotEqual(const a, b: TIDRecord): boolean; static; inline;
  end;

  PIDRecord = ^TIDRecord;

const
  DEF_IDRECORD: TIDRecord        = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Gray);
  DEF_HIGHLIGHT_COLOR: TIDRecord = (Bg: TAlphaColorRec.Yellow; Fg: TAlphaColorRec.Black); // TAlphaColorRec.Alpha or $FFFF60
  DEF_LINEHL_COLOR               = TAlphaColorRec.Alpha or $EAEAFD;

  // Qt-like selection colors
  IDR_SELECTION: TIDRecord = (Bg: $FF3399FF; Fg: TAlphaColorRec.White);

  IDR_NONE: TIDRecord   = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.LtGray);
  IDR_VA: TIDRecord     = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Navy);
  IDR_STRING: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Blue);

  IDR_COMMENT: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Green);

  IDR_CODE: TIDRecord      = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Navy);
  IDR_LABEL: TIDRecord     = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Maroon);
  IDR_HEXINCODE: TIDRecord = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Blue);
  IDR_REFS: TIDRecord      = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Black);

  IDR_IMM: TIDRecord        = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Blue);
  IDR_REGISTER: TIDRecord   = (Bg: TAlphaColorRec.White; Fg: TAlphaColorRec.Olive);
  IDR_BREAKPOINT: TIDRecord = (Bg: TAlphaColorRec.Red; Fg: TAlphaColorRec.Black);

//  IDR_BREAKPOINT_ACTIVE: TIDRecord   = (Bg: TAlphaColorRec.Red; Fg: TAlphaColorRec.Black);
//  IDR_BREAKPOINT_INACTIVE: TIDRecord = (Bg: TAlphaColorRec.Green; Fg: TAlphaColorRec.Black);
  IDR_LINEHIGHLIGHT_DBG: TIDRecord   = (Bg: TAlphaColorRec.Blue; Fg: TAlphaColorRec.Yellow);

  IDR_BREAKPOINT_ACTIVE: TIDRecord   = (Bg: $FFFFE0E0; Fg: TAlphaColorRec.Black);
  IDR_BREAKPOINT_INACTIVE: TIDRecord   = (Bg: $FFE0FFE0; Fg: TAlphaColorRec.Black);

  // Brush/Pen.
  DEF_BRUSH_COLOR = TAlphaColorRec.White;
  DEF_PEN_COLOR   = TAlphaColorRec.Black;

  // Gutter.
  DEF_GUTTER_COLOR = TAlphaColorRec.Alpha or $F0F0F0;
  DEF_GUTTER_WIDTH = 0; // 32;
  DEF_CARET_WIDTH  = 2;

type
  TIdRecords = class(TList<TIDRecord>)
  public
    function ContainIndex(Index: integer): Boolean; inline;
    function GetDefault: TIDRecord; inline;
    function Get(Index: integer): TIDRecord; inline;
  end;

  TRectArray = array of TRect;

  TGutter = record
    Width: TFmCoord;
    Color: TAlphaColor;
  end;

  TOnCaretMoved = TNotifyEvent;
  TOnTextUnderCaret = procedure(const Text: string) of object;

  TFieldRecord = record
    // True if this record is initialized.
    Present: Boolean;

    // Field rect is not strict rect around item. It may be inflated.
    // To get exact item rect see x0,x1,row.
    R: TFmRect;

    Line: integer;
    FieldId: byte; // Field number.
    TagId: integer;

    // Start/end X and Y.
    x0, x1, row: word;

    Key: word; // if keydown, keyup
    class operator Equal(const A, B: TFieldRecord): Boolean;
    class operator NotEqual(const A, B: TFieldRecord): Boolean;
  end;

  TFieldEvent = (feEnter, feLeave, feDblClick, feKeyDown);

  // MousePos is up to date.
  // CaretPos is old.
  // Return False not to allow caret be set at the pos.
  TFieldEventProc = function(Event: TFieldEvent; const FieldRec: TFieldRecord;
    Shift: TShiftState): Boolean of object;

  TLineDataObject = class
  public
    LineColorIdRecOverride: integer; // -1 if not overriden
    constructor Create;
  end;

{$SCOPEDENUMS ON}

  TLineItemFlag = (NeedPaint);
{$SCOPEDENUMS OFF}
  TLineItemFlags = set of TLineItemFlag;

  TLineItem = record
  public
    Str: string;
    Flags: TLineItemFlags;
    Data: TLineDataObject;
  end;

  TLineItems = TList<TLineItem>;

  // wtf, have to emulate bitrecord
  TWidthOfs = record
  strict private
    FByte: byte;
    function GetOffset: byte; inline;
    function GetWidth: byte; inline;
    procedure SetOffset(const Value: byte); inline;
    procedure SetWidth(const Value: byte); inline;
  public
    property Width: byte read GetWidth write SetWidth;
    property Offset: byte read GetOffset write SetOffset;
  end;

  { Cells }

  PCell = ^TCell;

  TCell = packed record
    Sym: WideChar;
    Id: byte;
    FieldId: byte;
    WO: TWidthOfs; // width/offset: cell # of chars and offset
    IdRec: TIDRecord;
    function IsClear: Boolean; inline;
    function IsField: Boolean; overload; inline;
    function IsField(AFieldId: byte): Boolean; overload; inline;
    function GetPureId: byte; inline;
    procedure SetPureId(Value: byte); inline;
    function GetFieldId: byte; inline;
    function SafeSym: WideChar; inline;
    function SafeWidth: byte; inline;
    function IsWhiteSpace: Boolean; inline;
  end;

  PCellArray = ^TCellArray;
  TCellArray = packed array [0 .. 4095] of TCell;

  { TParseLineContext }

  TCastTag = byte;

  TParseLineContext = record
    LineNumber, X, Y: integer;
    Tag: TCastTag;
    Id, FieldId, SymWdt: byte;
    Text: Punicodestring;
    ud: pointer;
  end;

  TParseLineCallback = procedure(var Ctx: TParseLineContext) of object;

  TOnScroll = procedure(dX, dY: integer) of object;

  // Convert System.UITypes.TAlphaColor to either FMX TAlphaColor or VCL TColor.
function VclFmxColor(Color: TAlphaColor): TAlphaColor; inline; overload;
function VclFmxColor(const Id: TIDRecord): TIDRecord; inline; overload;

implementation

{ TIdRecords }

function TIdRecords.ContainIndex(Index: integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
end;

function TIdRecords.Get(Index: integer): TIDRecord;
begin
  if ContainIndex(index) then
    Result := Items[index]
  else
    Result := GetDefault;
end;

function TIdRecords.GetDefault: TIDRecord;
begin
  Result := DEF_IDRECORD;
end;

{ TCell }

function TCell.GetFieldId: byte;
begin
  Result := FieldId;
end;

function TCell.GetPureId: byte;
begin
  Result := Id;
  // Result := (Id and TCastTag(TTag.TAGID_MASK));
end;

procedure TCell.SetPureId(Value: byte);
begin
  Id := Value;
  // Id := (Id and not TCastTag(TTag.TAGID_MASK)) or Value;
end;

function TCell.IsClear: Boolean;
begin
  Result := (Sym = #0);
end;

function TCell.IsField(AFieldId: byte): Boolean;
begin
  Result := self.IsField and (FieldId = AFieldId);
end;

function TCell.IsField: Boolean;
begin
  Result := self.FieldId <> TCastTag(TTag.BAD_FIELD_ID);
  // Result := ((self.Id and TCastTag(TTag.TAGID_FIELD)) <> 0);
end;

function TCell.IsWhiteSpace: Boolean;
begin
  Result := (Sym = #0) or (Sym = ' ');
end;

function TCell.SafeSym: WideChar;
begin
  if Sym = #0 then
    Result := ' '
  else
    Result := Sym;
end;

function TCell.SafeWidth: byte;
begin
  if WO.Width = 0 then
    Result := 1
  else
    Result := WO.Width;
end;

{ TItemRecord }

class operator TFieldRecord.Equal(const A, B: TFieldRecord): Boolean;
begin
  Result := (A.Present = B.Present) and (A.R = B.R);
end;

class operator TFieldRecord.NotEqual(const A, B: TFieldRecord): Boolean;
begin
  Result := not(A = B);
end;

{ TWidthOfs }

function TWidthOfs.GetOffset: byte;
begin
  Result := FByte shr 4;
end;

function TWidthOfs.GetWidth: byte;
begin
  Result := FByte and $F;
end;

procedure TWidthOfs.SetOffset(const Value: byte);
begin
  FByte := (FByte and $0F) or (Value shl 4);
end;

procedure TWidthOfs.SetWidth(const Value: byte);
begin
  FByte := (FByte and $F0) or Value;
end;

function VclFmxColor(Color: TAlphaColor): TAlphaColor;
begin
{$IFDEF vcl}
  Result :=
    ((Color and $000000FF) shl 16) or
    ((Color and $0000FF00)) or
    ((Color and $00FF0000) shr 16);
{$ELSE}
  Result := Color;
{$ENDIF};
end;

function VclFmxColor(const Id: TIDRecord): TIDRecord;
begin
  Result.Bg := VclFmxColor(Id.Bg);
  Result.Fg := VclFmxColor(Id.Fg);
end;
{ TIDRecord }

constructor TIDRecord.Create(Bg, Fg: TAlphaColor);
begin
  self.Bg := Bg;
  self.Fg := Fg;
end;

class operator TIDRecord.Equal(const a, b: TIDRecord): boolean;
begin
  result := (a.Bg = b.Bg) and (a.Fg = b.Fg);
end;

function TIDRecord.IsNull: boolean;
begin
  result := (self.Bg = 0) and (self.Fg = 0);
end;

class operator TIDRecord.NotEqual(const a, b: TIDRecord): boolean;
begin
  Result := not (a = b);
end;

{ TLineDataObject }

constructor TLineDataObject.Create;
begin
  self.LineColorIdRecOverride := -1;
end;

end.
