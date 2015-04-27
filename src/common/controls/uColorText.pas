{
  *
  *   Base component to display tagged coloured text.
  *
  *   todo
  *   - TIdRecords.Get can be changed to return PIDRecord
  *   - maybe simplify all FSelection stuff
  *
  *   OnTextUnderCaret
  *     called each time text under caret changed
  *
}

// todo:
// create layer to handle messages (adds portability)
// create built-in columns support w/o header control

// Locked selection doesn't work properly and is reason of selection bugs.
// {$DEFINE ALLOW_LOCKED_SELECTION }

unit uColorText;

interface

{$UNDEF NEED_HEADER_CONTROL}
{$UNDEF NEED_ANTIALIASING}


uses
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.SysUtils,
  System.UITypes, // expand

{$IFDEF VCL}
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms,    // TScrollBarKind
  Vcl.ComCtrls, // THeaderControl
  Vcl.ExtCtrls, // Timer

  WinAPI.Windows,
  WinAPI.Messages,
{$ELSE}
  FMX.Controls, // TControl
  FMX.StdCtrls, // TScrollBar
  FMX.Graphics, // TCanvas
  FMX.Types,    // TAlignLayout
  FMX.Forms,    // TScreen
  FMX.Platform, // services
{$ENDIF}
  uColorText.Selection,
  uColorText.Types,
  uColorText.Draw,

  VDAPI;

type

{$IFNDEF VCL}
  // Own caret for FMX only.
  TColorTextCaret = class(TCaret)
  end;
{$ENDIF}
  { TColorText }

{$IFDEF VCL}

  TColorText = class(TCustomControl)
{$ELSE}
  TColorText = class(TControl, ICaret)
{$ENDIF}
  private
    FFontSize: TFmCoord;
    FNewFontSize: TFmCoord; // to be inspected during Paint.
  private
    FLinesChanged: boolean; // need to rebuild lines
    FTextRect: TFmRect;
    FIdRecs: TIdRecords;
    FCursorItemId: integer;
    FCurrentFieldRec: TFieldRecord;
    FOnCaretMoved: TOnCaretMoved;
    FOnFieldEvent: TFieldEventProc;
    FOnScroll: TOnScroll;
    FOnIdRecsInit: TNotifyEvent;
    FIdInitDone: boolean;
    FLineItems: TLineItems;
    // FLineIndex: integer;
    FCells: array of TCell;
    FOnTextUnderCaret: TOnTextUnderCaret;
    FRowsAScreen: integer;
    FFullRowsAScreen: integer;

    FTextToHighlight: string;

    FHighlightColor: TIDRecord;
    FEnableHighlighting: boolean;

    FEnableLineHighlighting: boolean;

    FLineHighlightColor: TAlphaColor;

    procedure DoIdRecsInitOnce; inline;

    function XYInTextRect(const XY: TFmPoint): boolean; inline;
    function MouseInTextRect: boolean; inline;

    function GetRowLength(Y: integer): integer;
    procedure SetEnableHighlighting(const Value: boolean);

    function GetBkColor: TAlphaColor;
    procedure SetBkColor(const Value: TAlphaColor);

    procedure SetTextToHighlight(const Value: string);
    procedure SetLineHighlightColor(const Value: TAlphaColor);
    // procedure SetEnabled(const Value: Boolean); override;
    procedure SetEnableLineHighlighting(const Value: boolean);
    procedure SetOnIdRecsInit(const Value: TNotifyEvent);

    procedure SetScrollBarV(const Value: TScrollBar);

    procedure SetFontSize(Value: TFmCoord);

  private
    FCaretCnt: integer;
    FCaretWdt: TFmCoord;
    FCaretChanged: boolean;
    FCaretPos: TColRowPoint;
    FCaretVisible: boolean;
    FCaretCreated: boolean;

    procedure DoRecreateCaret; {$IFNDEF DEBUG}inline; {$ENDIF}
    procedure CaretSetWdt(const Value: TFmCoord);
    procedure SetCaretVisible(const Value: boolean);
    // Set caret position.
    // If Y < 0, caret is hidden.
    procedure SetCaretPosXY(const XY: TColRowPoint); // inline;
  public
    function CanCreateCaret: boolean; virtual;
    property CaretVisible: boolean read FCaretVisible write SetCaretVisible;
  protected
    FGutter: TGutter;
{$IFDEF NEED_HEADER_CONTROL}
  private
    procedure SetHeaderControl(const Value: THeaderControl);
    procedure DoOnHeaderControlSectionResize(HeaderControl: THeaderControl; Section: THeaderSection);
    procedure DrawHeaderSectionLines; virtual;
  protected
    FHeaderControl: THeaderControl;
    FHeaderControl_HandleSectionResize: boolean;
    // Get column start X by column number.
    function ColumnIdToColumnX(Id: integer; CurX: integer): integer;
  public
    property HeaderControl: THeaderControl read FHeaderControl write SetHeaderControl;
{$ENDIF}
{$IFDEF NEED_ANTIALIASING}
  private
    procedure SetTextAntialiasing(const Value: boolean);
    function GetTextAntialiasing: boolean; inline;
  public
    property TextAntiAliasing: boolean read GetTextAntialiasing write SetTextAntialiasing;
{$ENDIF}
  protected
    // It is alive only during Paint.
    FTargetCanvas: TCanvas;
    FDraw: TDrawerClass;
    // Width and height of char on screen.
    nCharX, nCharY: TFmCoord;
  private
    FScrollBarV: TScrollBar;
{$IFDEF VCL}
    // Don't need additional bitmap in FMX, canvas already has it.
    FBmp: TFmBitmap;
{$ENDIF}
    FWindowCharsX, FWindowCharsY: TColRowCoord;

    // Point set when user start selection with mousedown or shift+keys.
    FSelectionStartPoint: TPoint;
    FSelection: TSelection;
    FSelectionColor: TIDRecord;
    FSelectedTextSingleLine: string;
  protected
    // Called if selection is active and user want to clear it.
    procedure DoClearSelection; virtual;
  protected
    procedure MeasureText(const cv: TCanvas; const Text: string; var cx, cy: TFmCoord);

    procedure GrabTextSize; virtual;
    procedure GrabTextCharSize(cv: TCanvas; var cx, cy: TFmCoord);

    procedure UpdateMousePos;

    procedure UpdateCurrentItem(const XY: TFmPoint; Shift: TShiftState);
    procedure UpdateCurrentItemFromCaret;
    // function TryToFireMouseFieldEvent(Event: TFieldEvent; Shift: TShiftState; X, Y: TFmCoord): boolean;
    // function TryToFireKeyboardFieldEvent(Event: TFieldEvent; Shift: TShiftState; Key: word): boolean;

    // Get cell data.
    function CellGet(Col, Row: integer): TCell; overload;
{$IFNDEF DEBUG}inline; {$ENDIF}
    function CellGet(Pos: TPoint): TCell; overload;
{$IFNDEF DEBUG}inline; {$ENDIF}
    // Get cell raw char (i.e. empty is #0).
    function CellGetSym(Col, Row: integer): WideChar; inline;

    // Returns spaces at empty cells.
    function CellGetSymSafe(Col, Row: integer): WideChar; inline;

    function CellGetSymWidth(Sym: Char): integer; inline;
    // {$IFNDEF DEBUG}inline; {$ENDIF}

    // Resize and clear cells.
    procedure CellsInit(FirstInvalidatedLineItemIndex: integer);

    // Set cell data. Default width/offset.
    procedure CellSet(Col, Row: integer; Sym: WideChar; Id: word); overload;
      inline; // {$IFNDEF DEBUG}inline; {$ENDIF}
    // Set custom cell data.
    procedure CellSet(Col, Row: integer; const C: TCell); overload;
      inline; // {$IFNDEF DEBUG}inline; {$ENDIF}

    procedure FillCells(const R: TRect; Bg, Fg: PAlphaColor; NullsToSpaces: boolean = False);
    // {$IFNDEF DEBUG} inline; {$INLINE}

    procedure FillRow(Y: integer; Bg, Fg: PAlphaColor; NullsToSpaces: boolean = False);

    // Parse lines and fill cells (if Write is true).
    procedure LineToCells(LineNumber: integer);

    // Callback called on parsing lines.
    procedure LinesToCells_ParseCB(var Ctx: TParseLineContext);

    // Main draw, before flip.
    // If ADraw is False, nothing is drawed but cells are rebuilt.
    procedure DoDraw; virtual;
    procedure DoDrawCells(X, Y, w, h: integer);
    procedure DoDrawFieldRec;

    // procedure DoCursorItem(Item: PCursorItem); virtual;

    function GetCaretX: integer; inline;
    function GetCaretY: integer; inline;

    // Change only caret position without any notifications.
    procedure ChangeCaretOnly(const Value: TPoint);

    // Pos can be out of screen.
    // Set Pos performing scrolling.
    // If result is True, scrolling performed.
    // scrollX, scrollY return number of scrolled x/y.
    function SafeSetPos(var Pos: TPoint; out scrollX, scrollY: integer): boolean;

    // Pos is destination position.
    // dx, dy: distance moved
    procedure ChangeCaret(const Pos: TPoint; dX, dY: integer); virtual;
    procedure ChangeCaretSimple(const Pos: TPoint);
    procedure ChangeCaretX(X: integer);
    procedure ChangeCaretY(Y: integer);
    procedure CaretMoved; virtual;

    procedure CopyTextToClipboard(const Text: string);

    function GetTextToBeHighlighed: string; inline;

    //
    procedure DrawOverride; virtual;

    // It's not virtual since some moment, because strategy changed.
    // Now higher classes must only change selection rect. It will be painted
    // by this class.
    // Ask selection rect via SelectionGetPaintRect and draw it.
    procedure DrawSelection; // virtual;

    // User must set R rect which must be painted. Result is True if R is set,
    // if false selection is not painted.
    // Called for active selection, so no need to check IsActive.
    function SelectionGetPaintRect(out R: TRect): boolean; virtual;

    // Paint selection.
    // Rect contains selection region. In block mode it's simple rect.
    // Otherwise left-top is start of *row* selection and bottom-right is end.
    // If LeftX or RightX isn't -1, it will choose left/right border of
    // selection (column).
    procedure PaintSelectionEx(const R: TRect; BlockMode: boolean;
      LeftX: integer = -1; RightX: integer = -1); virtual;

    // Paint selection (whole range from left to right).
    procedure PaintSelection(const R: TRect; BlockMode: boolean); virtual;

    procedure Paint; override;

{$IFDEF VCL}
    // Flip only in VCL mode.
    procedure Flip; inline;
{$ENDIF}

{$IFDEF VCL}
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    // We need WMKillFocus message to correctly handle for example dialog popup.
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;

    procedure KeyDown(var Key: word; Shift: TShiftState); override;
{$ELSE}
    procedure KeyDown(var Key: word; var KeyChar: WideChar; Shift: TShiftState); override;
{$ENDIF}
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: TFmCoord; Y: TFmCoord); override;
    procedure MouseMove(Shift: TShiftState; X: TFmCoord; Y: TFmCoord); override;
{$IFNDEF VCL}
    procedure MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean); override;
{$ENDIF}
    procedure DoMouseWheel(Shift: TShiftState; WheelDelta: integer); reintroduce;

    procedure Resize; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DblClick; override;

    procedure SetGutterWidth(Value: integer);
    procedure SetGutterColor(Value: TAlphaColor);

    // Override this.
    // Must return True if at least one scroll done.
    function DoScroll(dX, dY: integer; var Pos: TPoint): boolean; virtual;

    // When you need scroll, call this function.
    // dX, dY is scroll amount.
    // Pos is position to scroll from.
    procedure PerformScroll(dX, dY: integer; var Pos: TPoint);

    // Guaranteed to be called if scroll occured. dX, dY show how many scrolls
    // done. CaretPos is corrected to be valid position.
    procedure DoAfterScroll(dX, dY: integer); virtual;

    // should be overrided
    procedure DoScrollBarVChanged(Sender: TObject); virtual;
    // override if needed

    // to add raw line internally
    // ret: false if lines already fill the screen and exit, else true
    function AddData(const S: string; const Data: TLineDataObject = nil): boolean;

    // Build lines your own way. Usually you may need to call LinesClear.
    procedure BuildLines; virtual; // abstract;

    function IsDisplayReady: boolean; inline;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: TFmCoord; Y: TFmCoord); override;

    procedure UpdateScrollBarV; virtual;

{$IFNDEF VCL}
  protected
    FCaret: TColorTextCaret;
    function CreateCaret: TColorTextCaret;
    { ICaret }
    function ICaret.GetObject = GetCaret;
    function GetCaret: TCustomCaret;
    procedure SetCaret(const Value: TCaret);
    procedure ShowCaret;
    procedure HideCaret;
{$ENDIF}
  public

    // Mouse coordinates.
    MousePos: TFmPoint;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure ReBuildLines(Force: boolean = False);

    procedure CopyToClipboard; virtual;

    { Selection }

    procedure ClearSelection;
    procedure SelectScreen;

    // called when selection started
    procedure SelectionStart(const CellPos: TPoint); virtual; // to override
    // called when selection changed
    procedure SelectionUpdate(const CellPos: TPoint; BlockMode: boolean); virtual; // to override

    procedure SelectionFirstFixed;
    procedure SelectionLastFixed(BlockMode: boolean);

    procedure SelectionSetRect(const R: TRect);

    procedure SelectionScreen; virtual; // should be overrided

    function LineIsValid(Line: integer): boolean; inline;
    procedure LineToRowRange(Line: integer; out y0, y1: TColRowCoord);
    function LineToRow(Line: integer): integer;

    function RowIsValid(Row: integer): boolean; inline;

    procedure ParseLine(LineNumber: integer; const LI: TLineItem;
      var X: integer; Y: integer; cb: TParseLineCallback; ud: pointer);

    procedure LinesClear; virtual;
    function LineCount: integer; virtual;
    function LineGet(Y: integer): string; virtual;
    function DataGet(Y: integer): TLineDataObject; virtual;

    procedure SetId(Id: integer; const IDRec: TIDRecord);
    procedure SetIdBg(Id: integer; Bg: TAlphaColor); inline;
    procedure SetIdFg(Id: integer; Fg: TAlphaColor); inline;

    // todo: maybe rename to XYAboveField
    // Check if XY (mouse coords) belongs to item and set item accordingly.
    function XYAboveItem(const XY: TFmPoint; out Item: TFieldRecord): boolean;

    // Get text from cells. Either as block or as rows (block=false).
    function CellGetText(x0, y0, x1, y1: integer; Block: boolean)
      : string; overload;
    function CellGetText(const R: TRect; Block: boolean)
      : string; overload;

    // Char position to local screen position.
    function CellColumnToX(Column: TColRowCoord): TFmCoord; inline;
    function CellRowToY(Row: TColRowCoord): TFmCoord; inline;

    function CellCoordsToPoint(x0, y0: TColRowCoord): TFmPoint; overload; inline;
    function CellCoordsToPoint(const Pt: TColRowPoint): TFmPoint; overload; inline;

    function CellCoordsToBoundRect(x0, y0, x1, y1: TColRowCoord): TFmRect; overload;
    function CellCoordsToBoundRect(const p0, p1: TColRowPoint): TFmRect; overload; inline;
    function CellCoordsToBoundRect(const R: TColRowRect): TFmRect; overload; inline;

    // Convert relative screen X/Y position to Column/Row.
    function XToColumn(X: TFmCoord): TColRowCoord; inline;
    function YToRow(Y: TFmCoord): TColRowCoord; inline;

    //
    //
    //
    // Change caret position by char, word, line, page.
    procedure GoCharBegin(var Pos: TPoint); inline;
    procedure GoCharLeft(var Pos: TPoint); inline;
    procedure GoCharRight(var Pos: TPoint); inline;

    procedure GoWordLeft(var Pos: TPoint); inline;
    procedure GoWordRight(var Pos: TPoint); inline;
    procedure GoLineUp(var Pos: TPoint); virtual;
    procedure GoLineDown(var Pos: TPoint); virtual;

    procedure GoPageUp(var Pos: TPoint); virtual;
    procedure GoPageDown(var Pos: TPoint); virtual;

    procedure GoTop(var Pos: TPoint); virtual;
    procedure GoBottom(var Pos: TPoint); virtual;

    procedure GoHome(var Pos: TPoint); virtual;
    procedure GoEnd(var Pos: TPoint); virtual;

    // dir = 1 or -1 (right or left)
    function IsTerm(X, Y, Dir: integer): boolean; // inline;
    function WordFromPos(Pos: TPoint): string;

    // Convert mouse coords to caret coords.
    function MousePosToCaretPos(const MousePos: TFmPoint; var CaretPos: TColRowPoint): boolean;

    // Get caret pos to current mouse position.
    procedure CaretPosFromMouse;

    // Set caret XY (0,0) and deactivate selection.
    procedure ResetCaretPos(Visible: boolean);

    // Make sure caret is shown at valid position.
    procedure RefreshCaretPosition;

    procedure ResetCurrentFieldRec; inline;

    function CellPosValid(const Pos: TPoint): boolean; inline;

    procedure UpdateSelectedTextSingleLine;

    // Wrapper to call Invalidate in VCL and FMX.
    procedure FmInvalidate;

    // Invalidate and request to rebuild all lines.
    // Set FLinesChanged True (i.e. to be rebuilt).
    procedure InvalidateAllRowsWithRebuilding;
    procedure InvalidateCurrentLine;
    procedure InvalidateRows(Row: integer; Cnt: integer = 1);
    procedure InvalidateAllRows; inline;
    procedure InvalidateRowsFromCoordRect(const R: TFmRect);

    //
    // Some fun
  private
{$IFDEF COLORTEXT_RAINBOW}
    FRainbowTimer: TTimer;
    FRainbowX: integer;
    procedure RainbowTimerProc(Sender: TObject);
  protected
    procedure DrawRainbowStripe(X: integer; Color: TAlphaColor);
    procedure DrawRainbow(X: integer);
  public
    procedure AnimateRainbow;
{$ENDIF COLORTEXT_RAINBOW}

    //
    //
    //
    //

    { Properties }
  public
    property RowsAScreen: integer read FRowsAScreen;
    property FullRowsAScreen: integer read FFullRowsAScreen;

    property LineItems: TLineItems read FLineItems;

    property Selection: TSelection read FSelection;

    property CurrentFieldRec: TFieldRecord read FCurrentFieldRec;

  published

    { Inherited }

    property Align;

{$IFDEF VCL}
    property OnKeyPress;
{$ENDIF}
    property OnKeyUp;
    property OnKeyDown;

    property OnCaretMoved: TOnCaretMoved read FOnCaretMoved write FOnCaretMoved;
    property OnTextUnderCaret: TOnTextUnderCaret read FOnTextUnderCaret write FOnTextUnderCaret;
    property OnFieldEvent: TFieldEventProc read FOnFieldEvent write FOnFieldEvent;

    property OnDblClick;
    property OnClick;

    { Own }

    // property Enabled: Boolean read FEnabled write SetEnabled;

    property BackgroundColor: TAlphaColor read GetBkColor write SetBkColor;
    property TextToHighlight: string read FTextToHighlight write SetTextToHighlight;

    property EnableHighlighting: boolean read FEnableHighlighting write SetEnableHighlighting;

    property GutterWidth: integer write SetGutterWidth;
    property GutterColor: TAlphaColor write SetGutterColor;

    property TextRect: TFmRect read FTextRect;

    { Selection }
    // Selected or highlighted text (at caret pos).
    property SelectedTextSingleLine: string read FSelectedTextSingleLine write FSelectedTextSingleLine;

    property EnableLineHighlighting: boolean read FEnableLineHighlighting
      write SetEnableLineHighlighting;

    property LineHighlightColor: TAlphaColor write SetLineHighlightColor;

    property CaretPos: TPoint read FCaretPos write ChangeCaretSimple;
    property CaretX: integer read GetCaretX write ChangeCaretX;
    property CaretY: integer read GetCaretY write ChangeCaretY;
    property CaretWidth: TFmCoord read FCaretWdt write CaretSetWdt;

    property OnScroll: TOnScroll read FOnScroll write FOnScroll;
    property OnIdRecsInit: TNotifyEvent read FOnIdRecsInit write SetOnIdRecsInit;

    property ScrollBarV: TScrollBar read FScrollBarV write SetScrollBarV;

    property FontSize: TFmCoord read FFontSize write SetFontSize;
    property WindowCharsX: integer read FWindowCharsX;
  end;

implementation

{$IFDEF VCL}


uses
  Vcl.Clipbrd;
{$ENDIF}

{$REGION 'utils'}


procedure stop;
asm
  int 3
end;

procedure Swap(var A, B: integer); inline;
var
  t: integer;
begin
  t := A;
  A := B;
  B := t;
end;

procedure Order(var A, B: integer);
begin
  if A > B then
    Swap(A, B);
end;

procedure SwapPoints(var A, B: TPoint); inline;
var
  t: TPoint;
begin
  t := A;
  A := B;
  B := t;
end;

procedure OrderPointsByY(var A, B: TPoint); inline;
begin
  if A.Y > B.Y then
    SwapPoints(A, B);
end;

procedure OrderPointsByX(var A, B: TPoint); inline;
begin
  if A.X > B.X then
    SwapPoints(A, B);
end;

{$ENDREGION 'utils'}

{ TColorText }

function TColorText.MousePosToCaretPos(const MousePos: TFmPoint; var CaretPos: TColRowPoint): boolean;
begin
  result := False;
  if FLineItems.Count <> 0 then
    if MouseInTextRect then
    begin
{$IFDEF VCL}
      CaretPos.X := (MousePos.X - FTextRect.Left) div nCharX;
      CaretPos.Y := (MousePos.Y - FTextRect.Top) div nCharY;
{$ELSE}
      CaretPos.X := Trunc((MousePos.X - FTextRect.Left) / nCharX);
      CaretPos.Y := Trunc((MousePos.Y - FTextRect.Top) / nCharY);
{$ENDIF}
      result := True;
    end;
end;

procedure TColorText.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: TFmCoord);
begin
  inherited;
  // TryToFireMouseFieldEvent(feMouseUp, Shift, X, Y);
end;

procedure TColorText.CaretPosFromMouse;
var
  Pos: TPoint;
begin
  if MousePosToCaretPos(MousePos, Pos) then
    ChangeCaretSimple(Pos);
end;

function TColorText.CellColumnToX(Column: TColRowCoord): TFmCoord;
begin
  result := FTextRect.Left + Column * nCharX;
{$IFNDEF VCL}
  result := Trunc(result);
{$ENDIF}
end;

function TColorText.CellRowToY(Row: TColRowCoord): TFmCoord;
begin
  result := FTextRect.Top + Row * nCharY;
{$IFNDEF VCL}
  result := Trunc(result);
{$ENDIF}
end;

function TColorText.CellCoordsToPoint(x0, y0: TColRowCoord): TFmPoint;
begin
  result.X := CellColumnToX(x0);
  result.Y := CellRowToY(y0);
end;

function TColorText.CellCoordsToPoint(const Pt: TColRowPoint): TFmPoint;
begin
  result := CellCoordsToPoint(Pt.X, Pt.Y);
end;

function TColorText.CellCoordsToBoundRect(const p0, p1: TColRowPoint): TFmRect;
begin
  result := CellCoordsToBoundRect(p0.X, p0.Y, p1.X, p1.Y);
end;

function TColorText.CellCoordsToBoundRect(const R: TColRowRect): TFmRect;
begin
  result := CellCoordsToBoundRect(R.Left, R.Top, R.Right, R.Bottom);
end;

function TColorText.CellCoordsToBoundRect(x0, y0, x1, y1: TColRowCoord): TFmRect;
begin
  Order(x0, x1);
  Order(y0, y1);

  inc(y1);

  result.Left := CellColumnToX(x0);
  result.Top := CellRowToY(y0);
  result.Right := CellColumnToX(x1);
  result.Bottom := CellRowToY(y1);
end;

function TColorText.CellGet(Col, Row: integer): TCell;
begin
  result := FCells[Row * FWindowCharsX + Col];
end;

function TColorText.CellGet(Pos: TPoint): TCell;
begin
  if Pos.Y >= 0 then
    result := FCells[Pos.Y * FWindowCharsX + Pos.X]
  else
  begin
    result.Sym := #0;
    result.WO.Offset := 0;
    result.WO.Width := 0;
  end;
end;

function TColorText.CellGetSym(Col, Row: integer): WideChar;
begin
  result := CellGet(Col, Row).Sym;
end;

function TColorText.CellGetSymSafe(Col, Row: integer): WideChar;
begin
  result := CellGet(Col, Row).Sym;
  if result = #0 then
    result := ' ';
end;

// todo: TAlphaColorText.CellGetSymWidth
function TColorText.CellGetSymWidth(Sym: Char): integer;
{$IFDEF VCL}
var
  cx: TFmCoord;
{$ENDIF}
begin
{$IFDEF VCL}
  result := 0;
  cx := FTargetCanvas.TextWidth(Sym);
  while cx > 0 do
  begin
    inc(result);
    cx := cx - nCharX;
  end;
{$ELSE}
  // warn: CellGetSymWidth is always assumed 1
  result := 1;
{$ENDIF}
end;

function TColorText.CellGetText(const R: TRect; Block: boolean): string;
begin
  result := CellGetText(R.Left, R.Top, R.Right, R.Bottom, Block);
end;

function TColorText.CellGetText(x0, y0, x1, y1: integer; Block: boolean)
  : string;
var
  X, Y: integer;
  RightLimit: integer;
  LeftLimit: integer;
  C: TCell;
  tmp: string;
begin
  result := '';

  Order(x0, x1);
  Order(y0, y1);

  Y := y0;

  if Block then
  begin
    RightLimit := x1;
    LeftLimit := x0;
  end
  else
  begin
    RightLimit := FWindowCharsX;
    LeftLimit := 0;
  end;

  while Y <= y1 do
  begin
    if Y = y0 then
      X := x0
    else
      X := LeftLimit;

    tmp := '';
    while X < RightLimit do
    begin
      // finish after last char in non-block mode
      if (not Block) and (Y = y1) and (X >= x1) then
        break;
      C := CellGet(X, Y);
      // Add char or space in case cell is clear.
      tmp := tmp + C.SafeSym;
      inc(X, C.SafeWidth);
    end;
    result := result + TrimRight(tmp) + sLineBreak;
    inc(Y);
  end;
end;

function TColorText.GetCaretX: integer;
begin
  result := FCaretPos.X;
end;

function TColorText.GetCaretY: integer;
begin
  result := FCaretPos.Y;
end;

function TColorText.GetRowLength(Y: integer): integer;
var
  X: integer;
begin
  result := 0;
  if Y > RowsAScreen then
    exit;
  for X := FWindowCharsX - 1 downto 0 do
    if not CellGet(X, Y).IsWhiteSpace then
      exit(X + 1);
end;

function TColorText.GetTextToBeHighlighed: string;
begin
  if FTextToHighlight <> '' then
    result := FTextToHighlight
  else
    result := FSelectedTextSingleLine;
end;

procedure TColorText.GoBottom(var Pos: TPoint);
begin

end;

procedure TColorText.GoCharBegin(var Pos: TPoint);
var
  C: TCell;
begin
  C := CellGet(Pos);
  if C.WO.Offset <> 0 then
    dec(Pos.X, C.WO.Offset);
end;

procedure TColorText.GoCharLeft(var Pos: TPoint);
begin
  if Pos.X > 0 then
  begin
    dec(Pos.X);
    GoCharBegin(Pos);
  end;
end;

procedure TColorText.GoCharRight(var Pos: TPoint);
var
  C: TCell;
begin
  C := CellGet(Pos);
  inc(Pos.X, C.SafeWidth);
end;

procedure TColorText.GoHome(var Pos: TPoint);
begin
  Pos.X := 0;
end;

procedure TColorText.GoEnd(var Pos: TPoint);
begin
  Pos.X := GetRowLength(Pos.Y);
end;

procedure TColorText.GoLineDown(var Pos: TPoint);
begin
  inc(Pos.Y);
end;

procedure TColorText.GoLineUp(var Pos: TPoint);
begin
  dec(Pos.Y);
end;

procedure TColorText.GoPageDown(var Pos: TPoint);
begin
  inc(Pos.Y, 40);
end;

procedure TColorText.GoPageUp(var Pos: TPoint);
begin
  dec(Pos.Y, 40);
end;

procedure TColorText.GoTop(var Pos: TPoint);
begin

end;

procedure TColorText.GoWordLeft(var Pos: TPoint);
begin
  // skip term
  while (Pos.X > 0) and (IsTerm(Pos.X, Pos.Y, -1)) do
    dec(Pos.X);
  // skip word
  while (Pos.X > 0) and not(IsTerm(Pos.X, Pos.Y, -1)) do
    dec(Pos.X);
end;

procedure TColorText.GoWordRight(var Pos: TPoint);
begin
  // skip word
  while (Pos.X < FWindowCharsX - 1) and not(IsTerm(Pos.X - 1, Pos.Y, 1)) do
    inc(Pos.X);
  // skip terms
  while (Pos.X < FWindowCharsX - 1) and (IsTerm(Pos.X - 1, Pos.Y, 1)) do
    inc(Pos.X);
end;

{$IFDEF VCL}


procedure TColorText.GrabTextCharSize(cv: TCanvas; var cx, cy: TFmCoord);
var
  tm: TTextMetric;
begin
  if not GetTextMetrics(cv.Handle, tm) then
    raise Exception.Create('Failed to get text metrics');

  // if tm.tmAveCharWidth <> tm.tmMaxCharWidth then
  // raise Exception.CreateFmt('Font "%s" has variable char width', [cv.Font.name]);

  cx := tm.tmAveCharWidth;
  cy := tm.tmHeight;
end;
{$ELSE}


procedure TColorText.GrabTextCharSize(cv: TCanvas; var cx, cy: TFmCoord);
begin
  cx := cv.TextWidth('W');
  cy := cv.TextHeight('W');
end;

{$ENDIF}

{$IFDEF VCL}


procedure TColorText.MeasureText(const cv: TCanvas; const Text: string; var cx, cy: TFmCoord);
begin
  raise Exception.Create('MeasureText');
end;
{$ELSE}


procedure TColorText.MeasureText(const cv: TCanvas; const Text: string; var cx, cy: TFmCoord);
var
  R: TFmRect;
begin
  R := RectF(0, 0, 10000, 20);
  cv.MeasureText(R, Text, False, [], TTextAlign.taLeading, TTextAlign.taCenter);
  cx := R.Right;
  cy := R.Bottom;
end;
{$ENDIF}


procedure TColorText.GrabTextSize;
begin
  if FTargetCanvas = nil then
    exit;

  GrabTextCharSize(FTargetCanvas, nCharX, nCharY);

{$IFDEF VCL}
  FWindowCharsX := (FTextRect.Width div nCharX) + 1;
  FWindowCharsY := (FTextRect.Height div nCharY) + 1;
  FFullRowsAScreen := FTextRect.Height div nCharY;
{$ELSE}
  FWindowCharsX := Trunc(FTextRect.Width / nCharX) + 1;
  FWindowCharsY := Trunc(FTextRect.Height / nCharY) + 1;
  FFullRowsAScreen := Trunc(FTextRect.Height / nCharY);
{$ENDIF}
  FRowsAScreen := FWindowCharsY;
end;

procedure TColorText.FmInvalidate;
begin
{$IFDEF VCL}
  self.Invalidate;
{$ELSE}
  self.Repaint;
  // todo: invalidate
{$ENDIF}
end;

procedure TColorText.InvalidateAllRowsWithRebuilding;
begin
  if Visible then
  begin
    FLinesChanged := True;
    InvalidateAllRows;
  end;
end;

procedure TColorText.InvalidateAllRows;
begin
  InvalidateRows(0, LineCount);
  FmInvalidate; // in case RowsUsed = 0
end;

procedure TColorText.InvalidateCurrentLine;
begin
  InvalidateRows(FCaretPos.Y);
end;

procedure TColorText.InvalidateRows(Row, Cnt: integer);
var
  I, y0, y1: integer;
  LineItem: TLineItem;
begin
  // For  test purpose: invalidate all rows
  // Row := 0;
  // Cnt := RowsAScreen;

  if Cnt = 0 then
    exit;

{$IFDEF DIAG_INVALIDATE}
  writeln('InvalidateRows: ', Row, ' cnt = ', Cnt);
{$ENDIF}
  if Row < 0 then
    Row := 0;

  y0 := Row;
  y1 := Row + Cnt;

  if FLineItems.Count >= y1 then
  begin
    for I := y0 to y1 - 1 do
    begin
      LineItem := FLineItems[I];
      Include(LineItem.Flags, TLineItemFlag.NeedPaint);
      FLineItems[I] := LineItem;
    end;
  end;

  FmInvalidate;
end;

procedure TColorText.InvalidateRowsFromCoordRect(const R: TFmRect);
var
  y0, y1: integer;
begin
  y0 := YToRow(R.Top);
  y1 := YToRow(R.Bottom);
  InvalidateRows(y0, y1 - y0 + 1);
end;

function TColorText.IsDisplayReady: boolean;
begin
  result := (nCharX <> 0) and (nCharY <> 0);
end;

function TColorText.IsTerm(X, Y, Dir: integer): boolean;
var
  C: WideChar;
begin
  C := CellGet(X + Dir, Y).Sym;
  if C = #0 then
    exit(True);
  if CharInSet(C, SPLITTER_SET) then
    exit(True);
  exit(False);
end;

procedure TColorText.CellsInit(FirstInvalidatedLineItemIndex: integer);
var
  I, Cnt: integer;
begin
  Cnt := FWindowCharsX * RowsAScreen;
  if Length(FCells) <> Cnt then
  begin
    SetLength(FCells, Cnt);
    FillChar(FCells[0], Cnt * sizeof(TCell), 0);
    exit;
  end;
  for I := FirstInvalidatedLineItemIndex to FLineItems.Count - 1 do
    if TLineItemFlag.NeedPaint in FLineItems[I].Flags then
      FillChar(FCells[I * FWindowCharsX], FWindowCharsX * sizeof(TCell), 0);
end;

function TColorText.CellPosValid(const Pos: TPoint): boolean;
begin
  if (Pos.Y * FWindowCharsX + Pos.X) >= Length(FCells) then
    exit(False);
  result := (Pos.Y >= 0) and (Pos.Y < FWindowCharsY) and (Pos.X >= 0);
end;

procedure TColorText.CellSet(Col, Row: integer; Sym: WideChar; Id: word);
var
  C: TCell;
begin
  C.Sym := Sym;
  C.Id := Id;
  C.WO.Width := 1;
  C.WO.Offset := 0;
  CellSet(Col, Row, C);
end;

procedure TColorText.CellSet(Col, Row: integer; const C: TCell);
begin
  // Test in debug.
  // In realease we omit it.
{$IFDEF DEBUG}
  if (Col < FWindowCharsX) and (Row < FWindowCharsY) then
{$ENDIF}
    FCells[Row * FWindowCharsX + Col] := C;
end;

procedure TColorText.ClearSelection;
begin
  if FSelection.IsActive then
    DoClearSelection;
end;

{$IFDEF NEED_HEADER_CONTROL}


function TColorText.ColumnIdToColumnX(Id: integer; CurX: integer): integer;
var
  tabX: integer;
begin
  // Tabs from FHeaderControl
  if FHeaderControl <> nil then
  begin
    // If Id in FHeaderControl ok, else fallback to default tabs.
    if Id < FHeaderControl.Sections.Count then
    begin
      tabX := FHeaderControl.Sections[Id].Left;
      result := XToColumn(tabX);
      exit;
    end;
  end;

  // Default tabs.
  result := ((CurX + DEF_COLUMN_WIDTH + 1) div DEF_COLUMN_WIDTH) * DEF_COLUMN_WIDTH;
end;

procedure TColorText.SetHeaderControl(const Value: THeaderControl);
begin
  FHeaderControl := Value;
  if FHeaderControl <> nil then
    FHeaderControl.OnSectionResize := DoOnHeaderControlSectionResize;
  FmInvalidate;
end;

procedure TColorText.DoOnHeaderControlSectionResize(HeaderControl
  : THeaderControl; Section: THeaderSection);
begin
  if FHeaderControl_HandleSectionResize then
  begin
    ResetCurrentFieldRec;
    Invalidate;
  end;
end;

procedure TColorText.UpdateHeaderSectionWidth;
var
  I, X: integer;
begin
  if FHeaderControl = nil then
    exit;

  FHeaderControl_HandleSectionResize := False;
  for I := 0 to FHeaderControl.Sections.Count - 1 do
  begin
    X := FHeaderControl.Sections[I].Left + FHeaderControl.Sections[I].Width;
    X := CellColumnToX(XToColumn(X));
    FHeaderControl.Sections[I].Width := X - FHeaderControl.Sections[I].Left;
  end;
  FHeaderControl_HandleSectionResize := True;
end;

procedure TColorText.DrawHeaderSectionLines;
var
  I, x0, x1: integer;
begin
  if FHeaderControl = nil then
    exit; // nothing to draw

  with FTargetCanvas do
  begin
    Pen.Mode := pmCopy;
    Pen.Color := clLtGray;
  end;

  for I := 0 to FHeaderControl.Sections.Count - 1 do
  begin
    x0 := FHeaderControl.Sections[I].Left;
    x1 := x0 + FHeaderControl.Sections[I].Width - 1; // inclusive
    FTargetCanvas.MoveTo(x1, FTextRect.Top);
    FTargetCanvas.LineTo(x1, FTextRect.Bottom);
  end;
end;

{$ENDIF}

{$IFDEF VCL}


procedure TColorText.CopyTextToClipboard(const Text: string);
var
  C: TClipboard;
begin
  C := TClipboard.Create;
  try
    C.AsText := Text;
  finally
    C.Free;
  end;
end;
{$ELSE}


procedure TColorText.CopyTextToClipboard(const Text: string);
var
  ClipService: IFMXClipboardService;
begin
  if FSelection.IsActive and
    TPlatformServices.Current.SupportsPlatformService(IFMXClipboardService, IInterface(ClipService)) then
    ClipService.SetClipboard(Text);
end;
{$ENDIF}


procedure TColorText.CopyToClipboard;
begin
  if FSelection.IsActive then
    CopyTextToClipboard(CellGetText(FSelection.R, FSelection.IsBlockMode))
  else if FSelectedTextSingleLine <> '' then
    CopyTextToClipboard(FSelectedTextSingleLine);
end;

constructor TColorText.Create(AOwner: TComponent);
const
  DEF_WDT = 100;
  DEF_HGT = 100;
begin
  inherited;

  // 1st time need to rebuild lines.
  FLinesChanged := True;

{$IFDEF VCL}
  TabStop := True;
{$ELSE}
  CanFocus := True;
{$ENDIF}

{$IFDEF VCL}
  FBmp := TFmBitmap.Create;
  FTargetCanvas := FBmp.Canvas;
{$ENDIF}
  // Setting Width and Height causes Resize (under FMX). So FBmp must be
  // already created.
  Width := DEF_WDT;
  Height := DEF_HGT;

{$IFDEF NEED_ANTIALIASING}
  // Antialiasing is on by default.
  TextAntiAliasing := DEFAULT_ANTIALIASING;
{$ENDIF}
  nCharX := 0;
  nCharY := 0;
  FWindowCharsX := 0;
  FWindowCharsY := 0;

  FCursorItemId := -1;

  FLineItems := TLineItems.Create;

  // FIdRecs must be created before assigning any colors.
  FIdRecs := TIdRecords.Create;

  BackgroundColor := DEF_BRUSH_COLOR;

  FHighlightColor := VclFmxColor(DEF_HIGHLIGHT_COLOR);
  FTextToHighlight := '';
  FEnableHighlighting := True;

  FGutter.Width := DEF_GUTTER_WIDTH;
  FGutter.Color := VclFmxColor(DEF_GUTTER_COLOR);

  FOnCaretMoved := nil;
  FCaretPos.X := 0;
  FCaretPos.Y := 0;
  FCaretCnt := 0;
  FCaretWdt := DEF_CARET_WIDTH;

{$IFNDEF VCL}
  FCaret := CreateCaret;
{$ENDIF}
  FEnableLineHighlighting := True;
  LineHighlightColor := VclFmxColor(DEF_LINEHL_COLOR);

  FSelectionColor := VclFmxColor(IDR_SELECTION);
  ClearSelection;

{$IFDEF NEED_HEADER_CONTROL}
  FHeaderControl_HandleSectionResize := True;
{$ENDIF}
  // Vertical scrollbar.
  FScrollBarV := TScrollBar.Create(self);
{$IFDEF VCL}
  FScrollBarV.Kind := sbVertical;
  FScrollBarV.Align := alRight;
{$ELSE}
  FScrollBarV.Orientation := TOrientation.orVertical;
  FScrollBarV.Align := TAlignLayout.alRight;
{$ENDIF}
  FScrollBarV.Min := 0;
  FScrollBarV.Max := 1000;
  FScrollBarV.Parent := self;
  SetScrollBarV(FScrollBarV);

  FFontSize := DEFAULT_FONT_SIZE;

{$IFNDEF VCL}
  ClipChildren := True;
{$ENDIF}

{$IFDEF COLORTEXT_RAINBOW}
  // -- Rainbow ----------------------------------------------------------------
  FRainbowTimer := TTimer.Create(self);
  FRainbowTimer.Interval := 100;
  FRainbowTimer.Enabled := False;
  FRainbowTimer.OnTimer := RainbowTimerProc;
{$ENDIF}
end;

function TColorText.DataGet(Y: integer): TLineDataObject;
begin
  if (Y >= 0) and (Y < FLineItems.Count) then
    result := FLineItems[Y].Data
  else
    result := nil;
end;

procedure TColorText.DblClick;
begin
  inherited;
  // TryToFireMouseFieldEvent(feDblClick, [], MousePos.X, MousePos.Y);
end;

destructor TColorText.Destroy;
begin
{$IFNDEF VCL}
  FreeAndNil(FCaret);
{$ELSE VCL}
  if FCaretCreated then
    DestroyCaret;
{$ENDIF}
  FIdRecs.Free;

  LinesClear;
  FLineItems.Free;

{$IFDEF VCL}
  FBmp.Free;
{$ENDIF}
  inherited;
end;

procedure TColorText.DoAfterScroll(dX, dY: integer);
begin

end;

procedure TColorText.DoClearSelection;
var
  y0, y1: integer;
  R: TRect;
begin
  if SelectionGetPaintRect(R) then
  begin
    y0 := R.Top;
    y1 := R.Bottom;
    Order(y0, y1);
    InvalidateRows(y0, y1 - y0 + 1);
    FSelection.Clear;
  end;
end;

procedure TColorText.DoDraw;
var
  LineItem: TLineItem;
  LineData: TLineDataObject;
  I: integer;
  FirstInvalidatedLineItemIndex: integer;
  R: TFmRect;
begin
  // ---------------------------------------------------------------------------
  // Find first changed item.
  FirstInvalidatedLineItemIndex := -1;
  for I := 0 to FLineItems.Count - 1 do
    if TLineItemFlag.NeedPaint in FLineItems[I].Flags then
    begin
      FirstInvalidatedLineItemIndex := I;
      break;
    end;
{$IFDEF DIAG_INVALIDATE}
  writeln('DoDraw first invalidated = ', FirstInvalidatedLineItemIndex);
{$ENDIF}
  // ---------------------------------------------------------------------------
  { clear canvas }

  // Clear changed lines.
  // todo: clearing canvas can be optimized calculating sequential areas
  if FirstInvalidatedLineItemIndex <> -1 then
  begin
    for I := FirstInvalidatedLineItemIndex to FLineItems.Count - 1 do
      if TLineItemFlag.NeedPaint in FLineItems[I].Flags then
      begin
        R := CellCoordsToBoundRect(0, I, FWindowCharsX, I);
        FDraw.DrawClearCanvas(FTargetCanvas, R, BackgroundColor);
      end;
  end;

  if LineCount < RowsAScreen then
  begin
    // Clear rest of lines.
    R := TextRect;
    R.Top := CellRowToY(LineCount);
    FTargetCanvas.Brush.Style := bsSolid;
    FTargetCanvas.Brush.Color := BackgroundColor;
    FTargetCanvas.FillRect(R);
  end;
  // ---------------------------------------------------------------------------
  // Init cells.
  if FirstInvalidatedLineItemIndex <> -1 then
    CellsInit(FirstInvalidatedLineItemIndex);
  // ---------------------------------------------------------------------------
  // Convert lines to cells.
  // And line color overrides.
  if FirstInvalidatedLineItemIndex <> -1 then
    for I := FirstInvalidatedLineItemIndex to FLineItems.Count - 1 do
      if TLineItemFlag.NeedPaint in FLineItems[I].Flags then
        LineToCells(I);
  // ---------------------------------------------------------------------------
  // Gutter.
  if FGutter.Width > 0 then
    FDraw.DrawGutter(FTargetCanvas, FGutter, FTextRect);
  // ---------------------------------------------------------------------------
  DrawOverride;
  // ---------------------------------------------------------------------------
  // Selection.
  DrawSelection;
  // ---------------------------------------------------------------------------
  // ---------------------------------------------------------------------------
  // -- fill line highlight ----------------------------------------------------
  if FEnableLineHighlighting and FCaretVisible then
  begin
    // if there's no line override
    LineData := DataGet(CaretY);
    if (not Assigned(LineData)) or (LineData.LineColorIdRecOverride = -1) then
    begin
      if (not Selection.IsActive) and LineIsValid(CaretY) then
        FillRow(CaretY, @FLineHighlightColor, nil, True);
    end;
  end;
{$IFDEF COLORTEXT_RAINBOW}
  // -- rainbow ----------------------------------------------------------------
  if FRainbowTimer.Enabled then
  begin
    DrawRainbow(FRainbowX);
  end;
{$ENDIF}
  // ---------------------------------------------------------------------------
  // -- draw cells -------------------------------------------------------------
  // new drawing: only if NeedPaint is on
  if FirstInvalidatedLineItemIndex <> -1 then
  begin
    for I := FirstInvalidatedLineItemIndex to FLineItems.Count - 1 do
    begin
      LineItem := FLineItems[I];
      if TLineItemFlag.NeedPaint in LineItem.Flags then
      begin
        DoDrawCells(0, I, FWindowCharsX, 1);
        Exclude(LineItem.Flags, TLineItemFlag.NeedPaint);
        FLineItems[I] := LineItem;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------
  // -- draw highlighted words -------------------------------------------------
  if FLineItems.Count <> 0 then
    if (FEnableHighlighting) and (not FSelection.IsActive) then
      FDraw.DrawHighlighting(FTargetCanvas, GetTextToBeHighlighed, FSelection,
        FHighlightColor, LineCount, FWindowCharsX, self.CellGet,
        self.CellColumnToX, self.CellRowToY);
  // ---------------------------------------------------------------------------

{$IFDEF NEED_HEADER_CONTROL}
  DrawHeaderSectionLines;
{$ENDIF}
  DoDrawFieldRec;
end;

procedure TColorText.DoDrawCells(X, Y, w, h: integer);
begin
  FDraw.DrawCells(
    FTargetCanvas,
    X, Y, w, h,
    self.nCharX, self.nCharY,
    CellCoordsToPoint,
    CellGet,
    FIdRecs);
end;

procedure TColorText.DoDrawFieldRec;
var
  R: TFmRect;
  cv: TCanvas;
  x0, y0, x1: integer;
begin
  if not FCurrentFieldRec.Present then
    exit;

  x0 := FCurrentFieldRec.x0;
  y0 := FCurrentFieldRec.Row;
  x1 := FCurrentFieldRec.x1;

  R := FCurrentFieldRec.R;
{$IFDEF DIAG_INVALIDATE}
  writeln(format('DoDrawFieldRec: %d,%d,%d,%d', [R.Left, R.Top, R.Right, R.Bottom]));
{$ENDIF}
  // FDraw.DrawCurrentItemBorder(FTargetCanvas, FCurrentFieldRec, nCharX, nCharY);
  cv := FTargetCanvas;
  // cv.Brush.Color := VclFmxColor($FFF0F0F0);
  cv.Brush.Color := VclFmxColor(BackgroundColor);

  // R := CellCoordsToBoundRect(x0, Y0, x1, Y0);
  // R.Inflate(nCharX div 2, nCharY div 2);
  cv.Rectangle(R);

  DoDrawCells(x0, y0, x1 - x0, 1);
end;

procedure TColorText.DoEnter;
begin
  inherited;
  SetFocus;
  DoRecreateCaret;
end;

procedure TColorText.DoExit;
begin
  FCaretCreated := not WinAPI.Windows.DestroyCaret();
  inherited;
end;

procedure TColorText.DoIdRecsInitOnce;
begin
  if (not FIdInitDone) and Assigned(FOnIdRecsInit) then
  begin
    FIdRecs.Clear;
    FOnIdRecsInit(self);
    FIdInitDone := True;
  end;
end;

procedure TColorText.DoMouseWheel(Shift: TShiftState; WheelDelta: integer);
var
  newSize: TFmCoord;
  Pos: TPoint;
  t: string;
begin
  if not Enabled then
    exit;

  if WheelDelta = 0 then
    exit;

  // Control + Wheel
  if (ssCtrl in Shift) then
  begin
{$IFDEF VCL}
    newSize := FontSize + (WheelDelta div abs(WheelDelta));
{$ELSE}
    newSize := FontSize + (WheelDelta / abs(WheelDelta));
{$ENDIF}
    if (newSize >= MIN_FONT_SIZE) then
    begin
      self.FontSize := newSize;
    end;
    exit;
  end;

  Pos := FCaretPos;

  t := FSelectedTextSingleLine; // store hl-sel; not very elegant

  if WheelDelta < 0 then
    PerformScroll(0, MOUSE_WHEEL_LINES, Pos)
  else if WheelDelta > 0 then
    PerformScroll(0, -MOUSE_WHEEL_LINES, Pos);

  FSelectedTextSingleLine := t; // restore hl-sel

  ReBuildLines;

  UpdateMousePos;

  UpdateCurrentItemFromCaret;
end;

procedure TColorText.PaintSelection(const R: TRect; BlockMode: boolean);
begin
  PaintSelectionEx(R, BlockMode);
end;

procedure TColorText.PaintSelectionEx(const R: TRect; BlockMode: boolean;
  LeftX: integer = -1; RightX: integer = -1);
const
  NULLS_TO_SPACES = True;
var
  pBG, pFG: PAlphaColor;
  p1, p2: TPoint;
  lx, rx: integer;
begin
  // order rect endpoints
  p1 := R.TopLeft;
  p2 := R.BottomRight;
  OrderPointsByY(p1, p2);

  // choose lx and rx
  if LeftX <> -1 then
    lx := LeftX
  else
    lx := 0;

  if RightX <> -1 then
    rx := RightX
  else
    rx := FWindowCharsX;

  // correct endpoints
  if p1.X < lx then
    p1.X := lx;
  if p2.X > rx then
    p2.X := rx;

  pBG := @FSelectionColor.Bg;
  pFG := @FSelectionColor.Fg;

  // selmode_block
  if FSelection.IsBlockMode then
  begin
    FillCells(R, pBG, pFG, NULLS_TO_SPACES);
  end
  else
  begin
    // selmode_rows

    // Single line.
    if p1.Y = p2.Y then
    begin
      FillCells(TRect.Create(p1.X, p1.Y, p2.X, p1.Y), pBG, pFG, NULLS_TO_SPACES);
    end
    else
    begin
      // Multi-line.
      // top
      FillCells(TRect.Create(p1.X, p1.Y, rx, p1.Y), pBG, pFG, NULLS_TO_SPACES);

      // bottom
      FillCells(TRect.Create(lx, p2.Y, p2.X, p2.Y), pBG, pFG, NULLS_TO_SPACES);

      // body
      if p2.Y - p1.Y > 1 then
        FillCells(TRect.Create(lx, p1.Y + 1, rx, p2.Y - 1), pBG, pFG, NULLS_TO_SPACES);
    end;
  end;
end;

{$IFDEF COLORTEXT_RAINBOW}


procedure TColorText.DrawRainbow(X: integer);
const
  Colors: array [0 .. 6] of TAlphaColor = (
    TAlphaColorRec.Red,
    TAlphaColorRec.Orange,
    TAlphaColorRec.Yellow,
    TAlphaColorRec.Green,
    TAlphaColorRec.Blue,
    TAlphaColorRec.Indigo,
    TAlphaColorRec.Violet);
var
  C, Color: TAlphaColor;
begin
  for C in Colors do
  begin
    Color := VclFmxColor(C);
    DrawRainbowStripe(X + 0, Color);
    DrawRainbowStripe(X + 1, Color);
    DrawRainbowStripe(X + 2, Color);
    DrawRainbowStripe(X + 3, Color);
    inc(X, 4);
  end;
end;

procedure TColorText.AnimateRainbow;
begin
  FRainbowX := -4 * 7;
  FRainbowTimer.Enabled := True;
end;

procedure TColorText.DrawRainbowStripe(X: integer; Color: TAlphaColor);
var
  Y: integer;
  C: TCell;
begin
  if Assigned(FCells) then
  begin
    Y := 0;
    while Y < self.RowsAScreen do
    begin
      if (X >= 0) and (X < self.FWindowCharsX) then
      begin
        C := CellGet(X, Y);
        C.IDRec.Bg := Color;
        CellSet(X, Y, C);
      end;
      dec(X);
      inc(Y);
    end;
  end;
end;

procedure TColorText.RainbowTimerProc(Sender: TObject);
begin
  inc(FRainbowX, 10);
  if FRainbowX > (FWindowCharsX + FWindowCharsY) then
    FRainbowTimer.Enabled := False;
  InvalidateAllRowsWithRebuilding;
end;
{$ENDIF}


procedure TColorText.DrawOverride;
begin

end;

procedure TColorText.DrawSelection;
var
  R: TRect;
begin
  if FSelection.IsActive then
  begin
    if SelectionGetPaintRect(R) then
      PaintSelection(R, FSelection.IsBlockMode);
  end;
end;

function IsBitSet(Value: dword; Mask: dword): boolean; inline;
begin
  result := (Value and Mask) <> 0;
end;

{$IFDEF VCL}


procedure TColorText.KeyDown(var Key: word; Shift: TShiftState);
{$ELSE}


procedure TColorText.KeyDown(var Key: word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
var
  OldPos: TPoint;
  Pos: TPoint;
  dX, dY: integer;
  holdingShift: boolean;
begin
  if not Enabled then
    exit;

  if ssCtrl in Shift then
    case Key of
      ord('C'): // Ctrl+C, Ctrl+Ins
        begin
          CopyToClipboard;
          exit;
        end;
      ord('A'): // Ctrl+A
        begin
          SelectScreen;
          FmInvalidate;
          exit;
        end;
    end;

  inherited;

  OldPos := FCaretPos;
  Pos := FCaretPos;

  case Key of
    vkLeft:
      if ssCtrl in Shift then
        GoWordLeft(Pos)
      else
        GoCharLeft(Pos);
    vkRight:
      if ssCtrl in Shift then
        GoWordRight(Pos)
      else
        GoCharRight(Pos);
    vkUp:
      GoLineUp(Pos);
    vkDown:
      GoLineDown(Pos);
    vkPrior:
      if ssCtrl in Shift then
        GoTop(Pos)
      else
        GoPageUp(Pos);
    vkNext:
      if ssCtrl in Shift then
        GoBottom(Pos)
      else
        GoPageDown(Pos);
    vkHome:
      GoHome(Pos);
    vkEnd:
      GoEnd(Pos);
  end;

  holdingShift := ssShift in Shift;

  // If pos changed, apply pos.
  if (Pos.X <> OldPos.X) or (Pos.Y <> OldPos.Y) then
  begin
    if not holdingShift then
      ClearSelection;

    dX := Pos.X - OldPos.X;
    dY := Pos.Y - OldPos.Y;
    ChangeCaret(Pos, dX, dY);
  end;

  // Now CaretPos should contain valid coords.
  // Handle selection when Shift is pressed.
  if holdingShift then
  begin
    // If not only shift is down.
    if (Key <> vkShift) then
    begin
      // todo: create CONST set for these values
      if (Key in [vkLeft, vkRight, vkUp, vkDown, vkHome, vkEnd, vkPrior, vkNext]) then
        SelectionUpdate(CaretPos, ssAlt in Shift)
{$IFDEF ALLOW_LOCKED_SELECTION}
      else if (Key = ord('1')) then
        SelectionFirstFixed
      else if (Key = ord('2')) then
        SelectionLastFixed(ssAlt in Shift)
{$ENDIF ALLOW_LOCKED_SELECTION}
      else
        SelectionStart(CaretPos);
    end;
  end
  else
  begin
    // If not shift and not alt, start new selection.
    // We mask out ssAlt, because it will corrupt mouse selection.
    if (Shift = []) then
    begin
      SelectionStart(OldPos);
    end;
  end;

  // TryToFireKeyboardFieldEvent(feKeyDown, Shift, Key);
end;

function TColorText.AddData(const S: string; const Data: TLineDataObject): boolean;
var
  L: TLineItem;
begin
  if FLineItems.Count = RowsAScreen then
    exit(False);

  L.Str := S;
  L.Flags := [TLineItemFlag.NeedPaint];
  L.Data := Data;
  FLineItems.Add(L);
  exit(True);
end;

procedure TColorText.LinesClear;
begin
  FLineItems.Clear;
end;

function TColorText.LineCount: integer;
begin
  result := FLineItems.Count;
end;

function TColorText.LineGet(Y: integer): string;
begin
  if (Y >= 0) and (Y < FLineItems.Count) then
    result := FLineItems[Y].Str
  else
    raise Exception.Create('LineGet error');
end;

function TColorText.LineIsValid(Line: integer): boolean;
begin
  result := (FLineItems.Count > Line) and (Line >= 0);
end;

procedure TColorText.ParseLine(LineNumber: integer; const LI: TLineItem;
  var X: integer; Y: integer; cb: TParseLineCallback; ud: pointer);
var
  TagH, TagL, bCnt, SymWdt: byte;
var
  CurId: byte;
var
  Ofs: integer;
  Ctx: TParseLineContext;
  tmpS: string;
  L: string;
{$IFDEF NEED_HEADER_CONTROL}
  ColumnId: integer;
{$ENDIF}
  FieldId: byte;

  procedure JmpToX(src, dst: integer);
  var
    t: integer;
  begin
    if Assigned(cb) then
    begin
      Ctx.LineNumber := LineNumber;
      Ctx.Y := Y;
      Ctx.Tag := TCastTag(TTag.TAG_TEXT_LEN);
      Ctx.Id := 0;
      Ctx.FieldId := TCastTag(TTag.BAD_FIELD_ID);
      tmpS := ' ';
      Ctx.Text := @tmpS;
      Ctx.SymWdt := SymWdt;
      Ctx.ud := ud;
      for t := src to dst - 1 do
      begin
        Ctx.X := t;
        cb(Ctx);
      end;
    end;
    X := dst;
  end;

begin
  L := LI.Str;
  X := 0;
  CurId := TCastTag(TTag.TAGID_NONE);
  Ofs := 1;
{$IFDEF NEED_HEADER_CONTROL}
  ColumnId := 0;
{$ENDIF}
  FieldId := TCastTag(TTag.BAD_FIELD_ID);
  while Ofs <= Length(L) do
  begin

    TagH := word(L[Ofs]) shr 8;
    TagL := byte(L[Ofs]);

    inc(Ofs);
    case TagH of
      TCastTag(TTag.TAG_FIELD_ID):
        begin
          FieldId := TagL;
          continue;
        end;

      TCastTag(TTag.TAG_TAB_COLUMN):
        begin
{$IFDEF NEED_HEADER_CONTROL}
          inc(ColumnId);
          JmpToX(X, ColumnIdToColumnX(ColumnId, X));
{$ELSE}
          X := X + 1;
{$ENDIF}
        end;

      TCastTag(TTag.TAG_SKIP):
        begin
          X := X + TagL;
        end;

      TCastTag(TTag.TAG_ID):
        begin
          CurId := TagL;
        end;

      TCastTag(TTag.TAG_TEXT_LEN):
        begin
          bCnt := TagL and TCastTag(TTag.TAGID_MASK);
          while bCnt > 0 do
          begin
            // #13 and #10 do the same: CRLF
            // if L[Ofs] = #13 then // CR
            // X := x0
            // else if L[Ofs] = #10 then // LF
            if CharInSet(L[Ofs], [#13, #10]) then
            begin
              // CR,LF support is dropped since 2013-06-30
              break;
              // inc(Y);
              // JmpToX(0, X);
            end
            else
            begin
              SymWdt := CellGetSymWidth(L[Ofs]);
              // {$IFDEF DEBUG}
              if SymWdt < 1 then
                SymWdt := 1;
              // {$ENDIF}
              if Assigned(cb) then
              begin
                Ctx.LineNumber := LineNumber;
                Ctx.X := X;
                Ctx.Y := Y;
                Ctx.Tag := TCastTag(TTag.TAG_TEXT_LEN);
                Ctx.FieldId := FieldId;
                Ctx.Id := CurId;
                tmpS := L[Ofs];
                Ctx.Text := @tmpS;
                Ctx.SymWdt := SymWdt;
                Ctx.ud := ud;
                cb(Ctx);
              end;
              inc(X, SymWdt);
            end;
            inc(Ofs);
            dec(bCnt);
          end;

          FieldId := TCastTag(TTag.BAD_FIELD_ID);

          continue;
        end;
    end;

    // CR,LF support is dropped since 2013-06-30
    // case (TagL and TCastTag(TTag.TAGID_MASK)) of
    // TCastTag(TTag.TAG_LF):
    // begin
    // inc(Y);
    // JmpToX(0, X);
    // end;
    // TCastTag(TTag.TAG_CR):
    // X := 0;
    // end;

  end;
end;

procedure TColorText.PerformScroll(dX, dY: integer; var Pos: TPoint);
begin
  if (dX <> 0) or (dY <> 0) then
  begin
    if DoScroll(dX, dY, Pos) then
    begin
      // Correct caret pos.
      if not RowIsValid(FCaretPos.Y) then
        if dY < 0 then
          FCaretPos.Y := 0
        else
          FCaretPos.Y := FFullRowsAScreen;

      // Additional hander.
      DoAfterScroll(dX, dY);
      InvalidateAllRowsWithRebuilding;
    end;
  end;
end;

procedure TColorText.LinesToCells_ParseCB(var Ctx: TParseLineContext);
var
  I: integer;
  C: TCell;
begin
  case Ctx.Tag of
    TCastTag(TTag.TAG_TEXT_LEN):
      begin
        for I := 0 to Ctx.SymWdt - 1 do
        begin
          C.Sym := Ctx.Text^[1];
          C.Id := Ctx.Id;
          C.FieldId := Ctx.FieldId;
          C.WO.Width := Ctx.SymWdt;
          C.WO.Offset := I;
          C.IDRec := FIdRecs.Get(Ctx.Id);
          CellSet(Ctx.X + I, Ctx.Y, C);
        end;
      end;
  end;
end;

procedure TColorText.LineToCells(LineNumber: integer);
var
  X: integer;
  LI: TLineItem;
  idr: TIDRecord;
begin
  X := 0;
  LI := FLineItems[LineNumber];
  ParseLine(LineNumber, LI, X, LineNumber, LinesToCells_ParseCB, nil);

  // Override line color.
  if Assigned(LI.Data) and (LI.Data.LineColorIdRecOverride <> -1) then
  begin
    idr := FIdRecs.Get(LI.Data.LineColorIdRecOverride);
    FillCells(Rect(0, LineNumber, FWindowCharsX, LineNumber), @idr.Bg, nil);
  end;
end;

function TColorText.LineToRow(Line: integer): integer;
var
  y1: integer;
begin
  LineToRowRange(Line, result, y1);
end;

procedure TColorText.LineToRowRange(Line: integer; out y0, y1: TColRowCoord);
begin
  {
    Y0 := 0;
    Y1 := 1;

    if LineIsValid(Line) then
    begin
    Y0 := FLineItems[Line].Y;
    Y1 := Y0 + FLineItems[Line].Hgt;
    end;
  }
  y0 := Line;
  y1 := Line + 1;
end;

procedure TColorText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TFmCoord);
begin
  inherited;

  DoEnter;

  MousePos.X := X;
  MousePos.Y := Y;

  // if not TryToFireMouseFieldEvent(feMouseDown, Shift, X, Y) then
  // exit;

  if ssLeft in Shift then
    ClearSelection;
  CaretPosFromMouse;

  if (ssLeft in Shift) then
  begin
    // if selection isn't locked
    if not FSelection.GetLockState(True) then
    begin
      if Enabled then
      begin
        SelectionStart(CaretPos);
      end;
    end;

  end; // (ssLeft in Shift)
end;

procedure TColorText.MouseMove(Shift: TShiftState; X, Y: TFmCoord);
var
  bIsMouseCapture: boolean;
begin
  inherited;

  if not Enabled then
    exit;

  MousePos := TFmPoint.Create(X, Y);

  UpdateCurrentItem(MousePos, Shift);
  // UpdateCurrentItemFromCaret;

  bIsMouseCapture := {$IFDEF VCL}MouseCapture{$ELSE}True{$ENDIF};

  // Check left button down
  if (ssLeft in Shift) and bIsMouseCapture then
  begin
    CaretPosFromMouse;
    SelectionUpdate(CaretPos, ssAlt in Shift);
  end;
end;

{$IFNDEF VCL}


procedure TColorText.MouseWheel(Shift: TShiftState; WheelDelta: integer; var Handled: boolean);
begin
  inherited;
  DoMouseWheel(Shift, WheelDelta);
end;
{$ENDIF}


function TColorText.XToColumn(X: TFmCoord): TColRowCoord;
begin
  result := Trunc((X - FTextRect.Left) / nCharX);
end;

function TColorText.YToRow(Y: TFmCoord): TColRowCoord;
begin
  result := Trunc((Y - FTextRect.Top) / nCharY);
end;

function TColorText.XYAboveItem(const XY: TFmPoint; out Item: TFieldRecord): boolean;
var
  Row, FieldId: integer;
  x0, x1: integer;
  C: TCell;
begin
  Item.Present := False;
  result := False;

  x0 := XToColumn(XY.X);
  Row := YToRow(XY.Y);

  if not CellPosValid(TPoint.Create(x0, Row)) then
    exit;

  C := CellGet(x0, Row);

  if not C.IsField then
    exit;

  FieldId := C.GetFieldId;

  x1 := x0 + 1;

  // scan left
  while (x0 > 0) and (CellGet(x0 - 1, Row).IsField(FieldId)) do
    dec(x0);

  // scan right
  while (x1 < FWindowCharsX) and (CellGet(x1, Row).IsField(FieldId)) do
    inc(x1);

  Item.Present := True;
  Item.R := CellCoordsToBoundRect(x0, Row, x1, Row);
  // Item.R.Inflate(nCharX div 2, nCharY div 3);
  Item.R.Inflate(2, 2);
  Item.Line := Row;
  Item.FieldId := C.GetFieldId;
  Item.TagId := C.GetPureId;
  Item.x0 := x0;
  Item.x1 := x1;
  Item.Row := Row;

  result := True;
end;

function TColorText.XYInTextRect(const XY: TFmPoint): boolean;
begin
  result := (XY.X >= FTextRect.Left) and (XY.X < FTextRect.Right) and
    (XY.Y >= FTextRect.Top) and (XY.Y < FTextRect.Bottom);
end;

function TColorText.MouseInTextRect: boolean;
begin
  result := XYInTextRect(MousePos);
end;

{$IFDEF VCL}


procedure TColorText.FillCells(const R: TRect; Bg, Fg: PAlphaColor; NullsToSpaces: boolean);
var
  X, Y, xlow, ylow, xhigh, yhigh: integer;
  C: TCell;
begin
  xlow := R.Left;
  xhigh := R.Right;
  if xlow > xhigh then
    Swap(xlow, xhigh);

  ylow := R.Top;
  yhigh := R.Bottom;
  if ylow > yhigh then
    Swap(ylow, yhigh);

  for Y := ylow to yhigh do
    for X := xlow to xhigh - 1 do
    begin
      C := CellGet(X, Y);

      // if (C.Sym = #0) and NullsToSpaces then
      // C.Sym := ' ';

      if Assigned(Bg) then
        C.IDRec.Bg := Bg^;

      if Assigned(Fg) then
        C.IDRec.Fg := Fg^;

      CellSet(X, Y, C);
    end;
end;

procedure TColorText.FillRow(Y: integer; Bg, Fg: PAlphaColor; NullsToSpaces: boolean);
begin
  FillCells(Rect(0, Y, FWindowCharsX, Y), Bg, Fg, NullsToSpaces);
end;

procedure TColorText.Flip;
begin
  // Flip only if drawing not directly on canvas.
  if FTargetCanvas <> Canvas then
    Canvas.Draw(0, 0, FBmp);
end;
{$ELSE}

{$FATAL 'FillCells'}


procedure TColorText.FillCells(const R: TRect; Id: byte);
begin
end;
{$ENDIF}


procedure TColorText.Paint();
var
  // NeedGrabTextSize: boolean;
  FontSizeChanged: boolean;
begin
  inherited;

  DoIdRecsInitOnce;

  FDraw := TDrawer;

{$IFDEF VCL}
  FTargetCanvas := FBmp.Canvas;
{$ELSE}
  FTargetCanvas := Canvas;
{$ENDIF}

{$IFDEF VCL}
  FTargetCanvas.Font.name := DefaultFontName;
  FTargetCanvas.Font.Pitch := fpFixed;
{$ELSE}
  FTargetCanvas.Font.Family := DefaultFontName;
{$ENDIF}
  FTargetCanvas.Font.Style := DEFAULT_FONT_STYLE;

  // NeedGrabTextSize := True;

  if FNewFontSize = 0 then
    FNewFontSize := FFontSize;

  FontSizeChanged := FNewFontSize <> FFontSize;

  // Update font size.
  // On XP FNewFontSize can be 0.
  if FontSizeChanged or (FTargetCanvas.Font.Size <> FFontSize) then
  begin
    FFontSize := FNewFontSize;
    FTargetCanvas.Font.Size := FFontSize;
    GrabTextSize;
    FontSizeChanged := True;
    // NeedGrabTextSize := False;
  end;

  // if NeedGrabTextSize then
  // GrabTextSize;

  if FontSizeChanged or FCaretChanged then
  begin
    FCaretChanged := False;
    DoRecreateCaret;
  end;

{$IFDEF NEED_HEADER_CONTROL}
  // Update sections width now, to make sure added lines will be tabbed right.
  UpdateHeaderSectionWidth;
{$ENDIF}
  // Rebuild lines (not forced).
  ReBuildLines(False);

  DoDraw;
{$IFDEF VCL}
  Flip;
{$ENDIF}
  // end;

{$IFNDEF VCL}
  // FMX
  FTargetCanvas := nil;
{$ENDIF}
end;

procedure TColorText.ReBuildLines(Force: boolean = False);
begin
{$IFDEF DIAG_INVALIDATE}
  writeln(format('ReBuildLines %x', [GetTickCount]));
{$ENDIF}
  if FLinesChanged or Force then
  begin
    if IsDisplayReady then
    begin
      FLinesChanged := False;
      BuildLines;
    end;
  end;
end;

procedure TColorText.RefreshCaretPosition;
begin
  SetCaretPosXY(FCaretPos);
end;

procedure TColorText.ResetCaretPos(Visible: boolean);
begin
  ClearSelection;
  FCaretPos := Point(0, 0);
  CaretVisible := Visible;
end;

procedure TColorText.ResetCurrentFieldRec;
begin
  FCurrentFieldRec.Present := False;
end;

procedure TColorText.Resize;
var
  Right: TFmCoord;
begin
  inherited Resize;

  Right := self.Width;
  if Assigned(FScrollBarV) then
    Right := Right - FScrollBarV.Width;

{$IFDEF VCL}
  FBmp.Width := {$IFNDEF VCL}Trunc{$ENDIF}(Right);
  FBmp.Height := {$IFNDEF VCL}Trunc{$ENDIF}(Height);
{$ENDIF}
  FTextRect.Left := FGutter.Width;
  FTextRect.Top := 0;
  FTextRect.Right := Right;
  FTextRect.Bottom := self.Height;

  // Without it when resize window line count is not updated.
  GrabTextSize;

  InvalidateAllRowsWithRebuilding;
end;

function TColorText.RowIsValid(Row: integer): boolean;
begin
  result := (Row >= 0) and (FFullRowsAScreen > Row);
end;

function TColorText.DoScroll(dX, dY: integer; var Pos: TPoint): boolean;
begin
  if Assigned(FOnScroll) then
    FOnScroll(dX, dY);
  result := False;
end;

procedure TColorText.DoScrollBarVChanged(Sender: TObject);
begin

end;

procedure TColorText.SelectScreen;
begin
  SelectionScreen;
end;

function TColorText.WordFromPos(Pos: TPoint): string;
var
  C: TCell;
begin
  result := '';

  if not CellPosValid(Pos) then
    exit;

  while (Pos.X > 0) and (not IsTerm(Pos.X, Pos.Y, -1)) do
    GoCharLeft(Pos);
  while (Pos.X < FWindowCharsX) and (not IsTerm(Pos.X - 1, Pos.Y, +1)) do
  begin
    C := CellGet(Pos.X, Pos.Y);
    result := result + C.Sym;
    inc(Pos.X, C.SafeWidth);
  end;
end;

procedure TColorText.SelectionFirstFixed;
begin
{$IFDEF ALLOW_LOCKED_SELECTION}
  FSelection.SetLockState(True, False);
  SelectionStart(FCaretPos);
  FSelection.SetLockState(True, True);
  Invalidate;
{$ELSE}
  raise Exception.Create('not implemented');
{$ENDIF}
end;

procedure TColorText.SelectionLastFixed(BlockMode: boolean);
begin
{$IFDEF ALLOW_LOCKED_SELECTION}
  FSelection.SetLockState(False, False);
  SelectionUpdate(FCaretPos, BlockMode);
  FSelection.SetLockState(False, True);
  Invalidate;
{$ELSE}
  raise Exception.Create('not implemented');
{$ENDIF}
end;

procedure TColorText.SelectionScreen;
begin
  FSelection.SetStartPoint(Point(0, 0));
  FSelection.SetEndPoint(Point(FWindowCharsX - 1, FWindowCharsY - 1));
end;

procedure TColorText.SelectionSetRect(const R: TRect);
begin
  FSelection.R := R;
end;

procedure TColorText.SelectionStart(const CellPos: TPoint);
begin
  // can change first pos if not locked
  if not FSelection.GetLockState(True) then
  begin
    FSelectionStartPoint := CellPos;
    ClearSelection;
  end;
end;

procedure TColorText.SelectionUpdate(const CellPos: TPoint; BlockMode: boolean);
var
  y0, y1: integer;
begin
  if CellPos = FSelectionStartPoint then
    exit;

  // can change last pos if not locked
  if not FSelection.GetLockState(False) then
  begin
    if BlockMode then
      FSelection.SetBlockMode
    else
      FSelection.SetRowMode;

    FSelection.SetStartPoint(FSelectionStartPoint);
    FSelection.SetEndPoint(CellPos);

    if (FSelection.ColsSelected > 0) then
      FSelectedTextSingleLine :=
        Trim(CellGetText(FSelection.R.Left, FSelection.R.Top, CellPos.X,
        FSelection.R.Top, False));

    y0 := FSelection.R.Top;
    y1 := FSelection.R.Bottom;
    Order(y0, y1);
    InvalidateRows(y0, y1 - y0 + 1);
  end;
end;

function TColorText.GetBkColor: TAlphaColor;
const
  id_bg = integer(TTag.TAG_NONE);
var
  idr: TIDRecord;
begin
  idr := FIdRecs.Get(id_bg);
  result := idr.Bg;
end;

procedure TColorText.SetBkColor(const Value: TAlphaColor);
const
  id_bg = integer(TTag.TAG_NONE);
var
  idr: TIDRecord;
begin
  idr := FIdRecs.Get(id_bg);
  if idr.Bg <> Value then
  begin
    idr.Bg := Value;
    SetId(id_bg, idr);
    FmInvalidate;
  end;
end;

procedure TColorText.SetCaretPosXY(const XY: TColRowPoint);
var
  Pt: TFmPoint;
begin
{$IFDEF VCL}
  CaretVisible := not(XY.Y < 0);
  if CaretVisible then
  begin
    Pt := CellCoordsToPoint(XY);
    WinAPI.Windows.SetCaretPos(Pt.X, Pt.Y);
  end;
{$ELSE FMX}
  if XY.Y < 0 then
  begin
    FCaret.Visible := False;
    exit;
  end;

  FCaret.BeginUpdate;
  try
    if FCaret.Visible = False then
      FCaret.Visible := True;

    Pt := CellCoordsToPoint(XY);
    FCaret.Pos := Pt;
    FCaret.Size := TSizeF.Create(FCaretWdt, nCharY);
  finally
    FCaret.EndUpdate;
  end;
{$ENDIF}
end;

procedure TColorText.CaretSetWdt(const Value: TFmCoord);
begin
  if Value <> FCaretWdt then
  begin
    FCaretWdt := Value;
    FCaretChanged := True;
  end;
end;

function TColorText.CanCreateCaret: boolean;
begin
  result := True;
end;

procedure TColorText.DoRecreateCaret;
begin
  if CanCreateCaret() then
  begin
    FCaretCreated := WinAPI.Windows.CreateCaret(Handle, 0, FCaretWdt, nCharY);
    if FCaretCreated then
    begin
      FCaretVisible := WinAPI.Windows.ShowCaret(Handle);
      RefreshCaretPosition;
    end;
  end;
{$IFDEF DIAG_CARET}
  writeln(self.ClassName, '.DoRecreateCaret: FCaretCreated=', FCaretCreated, ' FCaretVisible=', FCaretVisible);
{$ENDIF}
end;

procedure TColorText.SetCaretVisible(const Value: boolean);
begin
  if FCaretCreated then
  begin
    if FCaretVisible <> Value then
    begin
      if Value then
        FCaretVisible := WinAPI.Windows.ShowCaret(Handle)
      else
        FCaretVisible := not WinAPI.Windows.HideCaret(Handle);
    end;
  end;
end;

procedure TColorText.SetEnableHighlighting(const Value: boolean);
begin
  if FEnableHighlighting <> Value then
  begin
    FEnableHighlighting := Value;
    InvalidateRows(CaretY, 1);
  end;
end;

procedure TColorText.SetEnableLineHighlighting(const Value: boolean);
begin
  if FEnableLineHighlighting <> Value then
  begin
    FEnableLineHighlighting := Value;
    FmInvalidate;
  end;
end;

procedure TColorText.SetFontSize(Value: TFmCoord);
begin
  if Value > 1000 then
    Value := DEFAULT_FONT_SIZE;

  if Value = FFontSize then
    exit;

  FNewFontSize := Value;
  InvalidateAllRowsWithRebuilding;
end;

procedure TColorText.SetGutterColor(Value: TAlphaColor);
begin
  FGutter.Color := VclFmxColor(Value);
  if FGutter.Width > 0 then
    FmInvalidate;
end;

procedure TColorText.SetGutterWidth(Value: integer);
begin
  FGutter.Width := Value;
  FTextRect.Left := Value;
  FmInvalidate;
end;

procedure TColorText.SetId(Id: integer; const IDRec: TIDRecord);
begin
  if not FIdRecs.ContainIndex(Id) then
    FIdRecs.Count := Id + 1;

  FIdRecs[Id] := VclFmxColor(IDRec);
end;

procedure TColorText.SetIdBg(Id: integer; Bg: TAlphaColor);
var
  idr: TIDRecord;
  I: integer;
begin
  I := integer(Id);
  idr := FIdRecs.Get(I);
  idr.Bg := VclFmxColor(Bg);
  SetId(I, idr);
end;

procedure TColorText.SetIdFg(Id: integer; Fg: TAlphaColor);
var
  idr: TIDRecord;
  I: integer;
begin
  I := integer(Id);
  idr := FIdRecs.Get(I);
  idr.Fg := VclFmxColor(Fg);
  SetId(I, idr);
end;

procedure TColorText.SetLineHighlightColor(const Value: TAlphaColor);
begin
  FLineHighlightColor := Value;
end;

procedure TColorText.SetOnIdRecsInit(const Value: TNotifyEvent);
begin
  FOnIdRecsInit := Value;
  FIdInitDone := False;
  DoIdRecsInitOnce;
end;

procedure TColorText.SetScrollBarV(const Value: TScrollBar);
begin
  FScrollBarV := Value;

  if FScrollBarV <> nil then
    FScrollBarV.OnChange := DoScrollBarVChanged;
end;

procedure TColorText.UpdateScrollBarV;
begin

end;

{$IFDEF NEED_ANTIALIASING}


function TColorText.GetTextAntialiasing: boolean;
begin
  result := FTargetCanvas.Font.Quality = TFontQuality.fqClearTypeNatural;
end;

procedure TColorText.SetTextAntialiasing(const Value: boolean);
begin
  if Value <> TextAntiAliasing then
  begin
    if Value then
      FTargetCanvas.Font.Quality := TFontQuality.fqClearTypeNatural
    else
      FTargetCanvas.Font.Quality := TFontQuality.fqDefault;
    FmInvalidate;
  end;
end;
{$ENDIF}


procedure TColorText.SetTextToHighlight(const Value: string);
begin
  if FTextToHighlight <> Value then
  begin
    FTextToHighlight := Value;
    FmInvalidate;
  end;
end;

// function TColorText.TryToFireKeyboardFieldEvent(Event: TFieldEvent;
// Shift: TShiftState; Key: word): boolean;
// begin
// result := True;
// if Assigned(FOnFieldEvent) and (FCurrentFieldRec.Present) then
// begin
// FCurrentFieldRec.Key := Key;
// result := FOnFieldEvent(Event, FCurrentFieldRec, Shift);
// end;
// end;

// function TColorText.TryToFireMouseFieldEvent(Event: TFieldEvent;
// Shift: TShiftState; X, Y: TFmCoord): boolean;
// begin
// result := True;
// if
// Assigned(FOnFieldEvent) and
// (FCurrentFieldRec.Present) and
// (FCurrentFieldRec.R.Contains(TFmPoint.Create(X, Y))) then
// result := FOnFieldEvent(Event, FCurrentFieldRec, Shift);
// end;

procedure TColorText.UpdateCurrentItem(const XY: TFmPoint; Shift: TShiftState);
  procedure DoInv;
  begin
    InvalidateRows(FCaretPos.Y - 1, 3);
  end;

var
  y0, y1: integer;
begin
  // Currently we skip it.
  // exit;

  // If there is existing rect and xy is in this rect, then nothing changed.
  if (FCurrentFieldRec.Present) then
  begin
    if not(FCurrentFieldRec.R.Contains(XY)) then
    begin
      // Moved out of existing rect.
      FCurrentFieldRec.Present := False;
      // Need to invalidate previous rect.
      y0 := YToRow(FCurrentFieldRec.R.Top);
      y1 := YToRow(FCurrentFieldRec.R.Bottom);
      InvalidateRows(y0, y1 - y0 + 1);

      DoInv;
    end;
    // Otherwise we're still inside existing rect.
    exit;
  end
  else
  begin
    // No existing rect.

    // Check if we're in some rect.
    XYAboveItem(XY, FCurrentFieldRec); // update
    if FCurrentFieldRec.Present then
      DoInv;
  end;
end;

procedure TColorText.UpdateCurrentItemFromCaret;
begin
  UpdateCurrentItem(CellCoordsToPoint(CaretPos), []);
end;

procedure TColorText.UpdateMousePos;
var
  Pt: TPoint;
begin
{$IFDEF VCL}
  if GetCursorPos(Pt) then
    MousePos := ScreenToClient(Pt);
{$ELSE}
  MousePos := FMX.Forms.Screen.MousePos;
{$ENDIF}
end;

procedure TColorText.UpdateSelectedTextSingleLine;
var
  SelectedLast: string;
begin
  SelectedLast := FSelectedTextSingleLine;
  FSelectedTextSingleLine := WordFromPos(FCaretPos);
  if SelectedLast <> FSelectedTextSingleLine then
  begin
    InvalidateAllRows;
    if Assigned(FOnTextUnderCaret) then
      FOnTextUnderCaret(FSelectedTextSingleLine);
  end;
end;

function TColorText.SelectionGetPaintRect(out R: TRect): boolean;
begin
  result := False;
end;

procedure TColorText.CaretMoved;
begin
  if @FOnCaretMoved <> nil then
    FOnCaretMoved(self);
end;

function TColorText.SafeSetPos(var Pos: TPoint; out scrollX, scrollY: integer): boolean;
var
  highRow: integer;
begin
  scrollX := 0;
  scrollY := 0;

  highRow := FFullRowsAScreen - 1;
  // dY := Pos.Y - FCaretPos.Y;

  // Calc scrollY and new (on-screen) Pos.

  // Don't left Y to be less than 0
  if (Pos.Y < 0) then
  begin
    scrollY := Pos.Y;
    Pos.Y := 0;
  end
  // or >= than lines used
  else if (Pos.Y >= LineCount) then
  begin
    Pos.Y := LineCount - 1;
  end
  else if (Pos.Y > highRow) then
  begin
    scrollY := Pos.Y - highRow;
    Pos.Y := highRow;
  end;

  // Correct X.
  if (Pos.X < 0) then
    Pos.X := 0
  else if (Pos.X >= (FWindowCharsX - 1)) then
    Pos.X := FWindowCharsX - 1;

  result := (scrollX <> 0) or (scrollY <> 0);

  // Perform scroll.
  if result then
    PerformScroll(scrollX, scrollY, Pos);
end;

procedure TColorText.ChangeCaret(const Pos: TPoint; dX, dY: integer);
var
  OldPos, NewPos: TPoint;
  bHaveCells: boolean;
  scrolledX, scrolledY: integer;
begin
  OldPos := FCaretPos;

  bHaveCells := FCells <> nil;

  if (FWindowCharsX = 0) or (FWindowCharsY = 0) or (Pos = FCaretPos) then
    exit;

  NewPos := Pos;

  // set pos;
  // if scrolling happened, rebuild lines
  if SafeSetPos(NewPos, scrolledX, scrolledY) then
  begin
    ReBuildLines();
  end;

  // If pos in middle of some char, go to char begin.
  if bHaveCells then
    GoCharBegin(NewPos);

  FCaretPos := NewPos;
  RefreshCaretPosition;

  UpdateSelectedTextSingleLine;

  UpdateCurrentItemFromCaret;

  // Notification: CaretMoved
  if OldPos <> NewPos then
  begin
    CaretMoved;
  end;

  InvalidateRows(OldPos.Y);
  InvalidateRows(NewPos.Y);
end;

procedure TColorText.ChangeCaretOnly(const Value: TPoint);
begin
  FCaretPos := Value;
  RefreshCaretPosition;
end;

procedure TColorText.ChangeCaretSimple(const Pos: TPoint);
begin
  ChangeCaret(Pos, 0, 0);
end;

procedure TColorText.ChangeCaretX(X: integer);
var
  P: TPoint;
begin
  P := FCaretPos;
  P.X := X;
  ChangeCaretSimple(P);
end;

procedure TColorText.ChangeCaretY(Y: integer);
var
  P: TPoint;
begin
  P := FCaretPos;
  P.Y := Y;
  ChangeCaretSimple(P);
end;

{$IFDEF VCL}


procedure TColorText.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  DoEnter;
end;

procedure TColorText.WMKillFocus(var Message: TWMKillFocus);
begin
  DoExit;
  inherited;
end;

procedure TColorText.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.result := 1;
end;

procedure TColorText.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.result := DLGC_WANTARROWS;
end;

procedure TColorText.WMMouseWheel(var Message: TWMMouseWheel);
var
  Shift: TShiftState;
begin
  Shift := [];
  if (GetAsyncKeyState(VK_CONTROL) and $8000) <> 0 then
    Shift := Shift + [ssCtrl];
  DoMouseWheel(Shift, Message.WheelDelta);
end;

{$ENDIF}


procedure TColorText.BuildLines;
begin
end;

{$IFNDEF VCL}


function TColorText.CreateCaret: TColorTextCaret;
begin
  result := TColorTextCaret.Create(self);
  result.Pos := TPointF.Create(Single.MinValue, Single.MinValue);
  result.Visible := True;
  result.Color := TAlphaColorRec.Black;
  result.ReadOnly := True;
end;

function TColorText.GetCaret: TCustomCaret;
begin
  result := FCaret;
end;

procedure TColorText.SetCaret(const Value: TCaret);
begin
  if Assigned(FCaret) then
    FCaret.Assign(Value);
end;

procedure TColorText.ShowCaret;
begin
  TColorTextCaret(FCaret).Show;
end;

procedure TColorText.HideCaret;
begin
  TColorTextCaret(FCaret).Hide;
end;
{$ENDIF}


end.
