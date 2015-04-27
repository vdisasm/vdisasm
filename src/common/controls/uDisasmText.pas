unit uDisasmText;

interface

uses
{$IFDEF VCL}
  WinApi.Windows,
  WinApi.Messages,

  VCL.StdCtrls, // expand
  VCL.Controls,
  VCL.Graphics,
{$ELSE}
  FMX.Graphics,
  FMX.Types,
{$ENDIF}
  System.Classes,
  System.SysUtils,
  System.Math,
  System.Types, // expand Point()
  System.UITypes,
  System.Generics.Collections,

  uColorText.Types,
  uColorText.Selection,
  uColorText,
  uVaColorText,

  uCore.Strings,

  VDAPI,

  VDLib.Math.ValuePairs;

const
  MAX_PREFIX_LINES = 2; // max lines to show, then "..."

  COLUMNID_ADDRESS  = 0;
  COLUMNID_MAINTEXT = 1;
  COLUMNID_REFS     = 2;
  COLUMNID_COMMENTS = 3;

  DEFAULT_COPYTOCLPBRD_TEXT_FLAGS = 0;
  DEFAULT_DECODING_TEXT_FLAGS     = TVDTextFlag.Tags;

  COLOR_BASIC_BLOCK = $FFF8F8F8;

type
  TOnVAChanged = TNotifyEvent;

  { TSelPoint }

  TSelPoint = record
    VA: TVA;
    X: integer;
    YRel: integer;
    procedure Clear; inline;
    class procedure Order(var A, B: TSelPoint); static;
  end;

  { TVDDisasmText }

  TVDCustomDisasmText = class(TVAColorText)
  private
    FHandleScrollBarVChanged: boolean;
    FScrollBarVChanging: boolean;
  private
    FIgnoreMouseDown: integer;
    FTmpSL: TStringList;

    procedure UpdateReadyState; inline;

    // procedure CopyToClipboardVaRange_ParseCB(var ctx: TParseLineContext);

    function GetMaxScrollBarPos: integer; inline;

  protected
    FControlIsReady: boolean;

    FSelPtBegin: TSelPoint;
    FSelPtEnd: TSelPoint;

    FPrevUsrVA: TVA;

    FDumpWidth: integer;

    procedure GrabSelPoint(var P: TSelPoint);

    // Create line to added to lines. VA must be modified to point to next addr.
    // Text must be written into Layout.
    procedure CreateLine(const c: IVDCore; VA: PVA; const Layout: IVDVATextLayout); virtual; abstract;

    procedure BeforeCreateLines; virtual;
    // Create lines.
    procedure CreateLines(const c: IVDCore); virtual;
    // Create lines something changed.
    procedure BuildLines; override;

    function GetScreenVA(out Pos: TVDVAPos): boolean; virtual;
    function GetUserVA(out VA: TVA): boolean; virtual;

    // Map current selection to screen selection (to render).
    function MapSelectionToScreenSelection(out R: TRect;
      { out } CompletelyOnScreen: PBoolean = nil): boolean;

    procedure GetSelectionAddressesLocation(out R: TFmRect; out Text: string);
    procedure DrawSelectionAddresses;

    procedure DblClick; override;

    procedure DoEnter; override;
    procedure CaretMoved; override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: TFmCoord; Y: TFmCoord); override;

    procedure DoClearSelection; override;
    procedure DoDraw; override;

    // < scrollbar
    procedure UpdateScrollBarV; override;
    function SelectionGetPaintRect(out R: TRect): boolean; override;

{$IFDEF VCL}
    procedure WMGetDlgCode(var Message: TWMNoParams); override;
{$ENDIF}
  protected
    procedure DoScrollBarVChanged(Sender: TObject); override;
    procedure DoAfterScroll(dX: integer; dY: integer); override;
    // scrollbar >

    procedure DrawOverride; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GoPageDown(var Pos: TPoint); override;
    procedure GoPageUp(var Pos: TPoint); override;
    procedure GoTop(var Pos: TPoint); override;
    procedure GoBottom(var Pos: TPoint); override;

    // Must return True if at least one scroll done.
    function DoScroll(dX: integer; dY: integer; var Pos: TPoint): boolean; override;

    function DoScrollOneLine(dY: integer; var VA: TVA): boolean; virtual;

    // SetCursorVAForced doesn't check if VA already is set,
    // and can cause deadlocks in some cases.
    procedure SetCursorVAForced(VA: TVA);

    procedure SetCursorVA(VA: TVA); {$IFNDEF debug} inline; {$ENDIF}
    procedure SetVA(VAddr: TVA); override;
    procedure SetDumpWidth(V: integer);

    procedure SelectionStart(const Pos: TPoint); override;
    procedure SelectionUpdate(const Pos: TPoint; BlockMode: boolean); override;
    procedure SelectionScreen; override;

    // Get ordered selection Va.
    function GetSelectionVA(out va0, va1: TVA): boolean;

    procedure CopyToClipboardVaRange(va0, va1: TVA);
    procedure CopyToClipboard; override;

    property CursorVA: TVA read GetCursorVA write SetCursorVA;
    property DumpWidth: integer read FDumpWidth write SetDumpWidth;

  published
    property PopupMenu;
  end;

  TVDDisasmText = class(TVDCustomDisasmText)
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateLine(const c: IVDCore; VA: PVA; const Layout: IVDVATextLayout); override;

    // procedure GoLineUp(var Pos: TPoint); override;
    // procedure GoLineDown(var Pos: TPoint); override;
  end;

implementation

{ TVDDisasmText }

procedure TVDCustomDisasmText.CreateLines(const c: IVDCore);
var
  orgVA, curVA: TVA;
  Layout: IVDVATextLayout;
  i: integer;
  Text: string;
  data: TVADataObject;
  BP: TVDBreakPointRecord;
  dbg: IVDDebuggerPlugin;
var
  session: IVDDebugSession;
  dbgIP: TVA;
  dbgState: TVDDebuggerState;
begin
  BeforeCreateLines;

  Layout := CreateVATextLayout(DEFAULT_DECODING_TEXT_FLAGS);

  session := CoreGet.DebugSession;
  dbgState := session.State;
  dbg := nil;
  session.GetDebugger(false, dbg);
  if assigned(dbg) then
    dbgIP := dbg.GetIP
  else
    dbgIP := 0;

  curVA := FVA;
  while True do
  begin
    orgVA := curVA;

    Layout.Clear;

    // Add line text.
    CreateLine(c, @curVA, Layout);

    // This linebreak to make last line be "line" too.
    Layout.LineBreak;

    for i := 0 to Layout.GetLineCount - 1 do
    begin
      Text := Layout.GetLine(i);

      data := TVADataObject.Create;
      data.VA := orgVA;

      // Override color of lines related to debugger (breakpoints).
      // Only for TVDDisasmText.
      if self is TVDDisasmText then
      begin
        // is breakpoint
        if (dbgState = DBGSTATE_PAUSED) and assigned(dbg) and (dbgIP = orgVA) then
        begin
          data.LineColorIdRecOverride := integer(TTag.TAGID_LINEHIGHLIGHT_DBG);
        end
        else if session.GetBreakpoint(orgVA, @BP) then
        begin
          if (BP.Flags and TVDBreakpointFlag.BPF_ACTIVE) <> 0 then
            data.LineColorIdRecOverride := integer(TTag.TAGID_BREAKPOINT_ACTIVE)
          else
            data.LineColorIdRecOverride := integer(TTag.TAGID_BREAKPOINT_INACTIVE);
        end;
      end;

      if not AddData(Text, data) then
      begin
        data.Free;
        exit;
      end;

      // if not Add(text, orgVA) then
      // exit;
    end;

    if not c.VM.Exists(curVA) then
      if not c.GetDecoder.ItemStep(@curVA, 1) then
        break;
  end;
end;

procedure TVDCustomDisasmText.BeforeCreateLines;
begin
end;

procedure TVDCustomDisasmText.BuildLines;
var
  Pos: TVDVAPos;
  L: integer;
  TmpVA: TVA;
begin
  UpdateReadyState;

  if not FControlIsReady then
  begin
    LinesClear;
    exit;
  end;

  if not GetScreenVA(Pos) then
    raise Exception.Create('Can''t get screen VA');

  FVA := Pos.ScrVA;

  if (FVA = BAD_VA) or (not FCore.VM.Exists(FVA)) then
  begin
    exit;
  end;

  LinesClear;
  FCore.Decoder.SetHexDumpByteCount(FDumpWidth);
  CreateLines(FCore);

  if FindVAInData(Pos.curVA, @L) = fid_equal then
  begin
    // Get VA in line where cursor is now.
    LineToVA(CaretY, TmpVA);
    // Don't spoil cursor Y if VA occupies more than one line.
    if Pos.curVA <> TmpVA then
    begin
      ChangeCaretOnly(TPoint.Create(Pos.X, L));
      // If caret is hidden it will be shown by Changing caret position.
    end;
  end
  else
  begin
    CaretVisible := false; // hide caret
  end;
end;

procedure TVDCustomDisasmText.CaretMoved;
var
  cVA: TVA;
begin
  inherited;

  UpdateCoreVAPos;

  cVA := CursorVA;
  if FPrevUsrVA <> cVA then
  begin
    FPrevUsrVA := cVA;
    DoVaChanged; // todo: redundant
  end;
end;

// -----------------------------------------------------------------------------
// CopyToClipboard
// -----------------------------------------------------------------------------
procedure TVDCustomDisasmText.CopyToClipboard;
var
  R: TRect;
  full: boolean;
begin
  // If nothing selected, copy text under cursor, if any.
  if not Selection.IsActive then
    if SelectedTextSingleLine <> '' then
    begin
      inherited CopyToClipboard;
      exit;
    end
    else
      exit;

  // if selection fit the screen, copy it.
  if MapSelectionToScreenSelection(R, @full) and full then
  begin
    SelectionSetRect(R);
    inherited CopyToClipboard;
  end
  else
  begin
    // multi-screen copy
    CopyToClipboardVaRange(Min(FSelPtBegin.VA, FSelPtEnd.VA),
      Max(FSelPtBegin.VA, FSelPtEnd.VA));
  end;
end;

type
  PCopyToClipboardVaRangeStruc = ^TCopyToClipboardVaRangeStruc;

  TCopyToClipboardVaRangeStruc = record
    X, Y: integer;
    lastY: integer;
    lastStr: string;
    toCopy: string;
    selX0, selX1: integer;
    IsBlock: boolean;
    procedure InitLastStr(wdt: integer; sym: widechar = ' ');
  end;

procedure TCopyToClipboardVaRangeStruc.InitLastStr(wdt: integer;
  sym: widechar = ' ');
var
  i: integer;
begin
  setlength(lastStr, wdt);
  for i := 1 to wdt do
    lastStr[i] := sym;
end;

procedure TVDCustomDisasmText.CopyToClipboardVaRange(va0, va1: TVA);
var
  t: IVDVATextLayout;
  VA: TVA;
  c: IVDCore;
  s: string;
begin
  c := FCore;
  VA := va0;
  t := CreateVATextLayout(DEFAULT_COPYTOCLPBRD_TEXT_FLAGS);
  while VA <= va1 do
  begin
    t.Clear;
    c.GetDecoder.DecodeToText(VA, t);
    s := s + t.Get;
    if s <> '' then
      s := s + #$0A;
    if not FCore.VM.Exists(VA) then
      if not FCore.GetDecoder.ItemStep(@VA, 1) then
        break;
  end;
  CopyTextToClipboard(s);
end;

// -----------------------------------------------------------------------------

constructor TVDCustomDisasmText.Create(AOwner: TComponent);
begin
  inherited;

  FVA := BAD_VA;
  FPrevUsrVA := BAD_VA;

  SetId(TCastTag(TTag.TAGID_NONE), IDR_NONE);
  SetId(TCastTag(TTag.TAGID_VA), IDR_VA);
  SetId(TCastTag(TTag.TAGID_NUMBER), IDR_IMM);
  SetId(TCastTag(TTag.TAGID_STRING), IDR_STRING);
  SetId(TCastTag(TTag.TAGID_CODE), IDR_CODE);

  SetId(TCastTag(TTag.TAGID_LABEL), IDR_LABEL);
  SetId(TCastTag(TTag.TAGID_OFFSET), IDR_LABEL);

  SetId(TCastTag(TTag.TAGID_COMMENT), IDR_COMMENT);

  SetId(TCastTag(TTag.TAGID_REGISTER), IDR_REGISTER);
  SetId(TCastTag(TTag.TAGID_HEXINCODE), IDR_HEXINCODE);

  SetId(TCastTag(TTag.TAGID_REFIN), IDR_REFS);
  SetId(TCastTag(TTag.TAGID_REFOUT), IDR_REFS);

  if self is TVDDisasmText then
  begin
    SetId(TCastTag(TTag.TAGID_BREAKPOINT_ACTIVE), IDR_BREAKPOINT_ACTIVE);
    SetId(TCastTag(TTag.TAGID_BREAKPOINT_INACTIVE), IDR_BREAKPOINT_INACTIVE);
    SetId(TCastTag(TTag.TAGID_LINEHIGHLIGHT_DBG), IDR_LINEHIGHLIGHT_DBG);
  end;

  FSelPtBegin.Clear;
  FSelPtEnd.Clear;

  FTmpSL := TStringList.Create;

  FHandleScrollBarVChanged := True;

  FDumpWidth := 8;

  EnableLineHighlighting := not(self is TVDDisasmText);
end;

procedure TVDCustomDisasmText.DblClick;
begin
  inherited;
  FIgnoreMouseDown := 1;
end;

destructor TVDCustomDisasmText.Destroy;
begin
  FTmpSL.Free;
  inherited;
end;

procedure TVDCustomDisasmText.DoAfterScroll(dX, dY: integer);
begin
  inherited;

  // Screen VA is changed
  // User VA stays same
  UpdateCoreVAPos(false);
end;

procedure TVDCustomDisasmText.DoClearSelection;
var
  R: TFmRect;
  Text: string;
begin
  inherited;
  GetSelectionAddressesLocation(R, Text);
  InvalidateRowsFromCoordRect(R);
end;

procedure TVDCustomDisasmText.DoDraw;
begin
  inherited;
  if Selection.IsActive then
    DrawSelectionAddresses;
end;

procedure TVDCustomDisasmText.DoEnter;
begin
  inherited;
  UpdateReadyState;
end;

function TVDCustomDisasmText.GetMaxScrollBarPos: integer;
begin
  result := Trunc(ScrollBarV.Max);
  if result = 0 then
    result := 1;
end;

function TVDCustomDisasmText.GetScreenVA(out Pos: TVDVAPos): boolean;
var
  tmp: TVDVAPos;
begin
  result := FCore.GetVAPos(@tmp);
  if result then
    Pos := tmp;
end;

function TVDCustomDisasmText.GetSelectionVA(out va0, va1: TVA): boolean;
var
  v0, v1: TVA;
begin
  if Selection.IsActive then
  begin
    v0 := FSelPtBegin.VA;
    v1 := FSelPtEnd.VA;

    TValuePair<TVA>.Order(v0, v1,
      function(const A, B: TVA): integer
      begin
        result := A - B;
      end);

    va0 := v0;
    va1 := v1;
    exit(True);
  end;
  exit(false);
end;

function TVDCustomDisasmText.GetUserVA(out VA: TVA): boolean;
var
  tmp: TVA;
begin
  tmp := FCore.GetUserVA;
  result := tmp <> BAD_VA;
  if result then
    VA := tmp;
end;

procedure TVDCustomDisasmText.GoPageDown(var Pos: TPoint);
var
  curVA: TVA;
begin
  if LineCount = 0 then
    exit;
  if not LineToVA(LineCount - 1, curVA) then
    exit;
  if not FCore.GetDecoder.ItemStep(@curVA, 1) then
    exit;
  self.VA := curVA;
end;

procedure TVDCustomDisasmText.GoPageUp(var Pos: TPoint);
var
  curVA: TVA;
  i: integer;
begin
  curVA := VA;
  for i := 0 to self.RowsAScreen - 1 do
    if not FCore.GetDecoder.ItemStep(@curVA, -1) then
      break;
  self.VA := curVA;
end;

procedure TVDCustomDisasmText.GoTop(var Pos: TPoint);
begin
  NavigateToBegin;
end;

procedure TVDCustomDisasmText.GoBottom(var Pos: TPoint);
begin
  NavigateToEnd;
end;

procedure TVDCustomDisasmText.GrabSelPoint(var P: TSelPoint);
var
  L, LY: integer;
begin
  P.VA := CursorVA;
  P.X := CaretX;

  L := VAToLine(P.VA);
  LY := LineToRow(L);

  P.YRel := CaretY - LY;
end;

function TVDCustomDisasmText.MapSelectionToScreenSelection(out R: TRect;
CompletelyOnScreen: PBoolean): boolean;
var
  sel1, sel2: TSelPoint;
  fbeg, fend: TFindVAInData; // found begin, end
  lBeg, lEnd: integer;       // line begin, end
begin
  if assigned(CompletelyOnScreen) then
    CompletelyOnScreen^ := false;

  { common }
  sel1 := FSelPtBegin;
  sel2 := FSelPtEnd;
  TSelPoint.Order(sel1, sel2);

  fbeg := FindVAInData(sel1.VA, @lBeg);
  fend := FindVAInData(sel2.VA, @lEnd);

  if (fbeg = fid_greater) or (fend = fid_less) then
    exit(false);

{$IFDEF DEBUG}
  if not(Selection.IsRowMode or Selection.IsBlockMode) then
    raise Exception.Create('Selecting mode error');
{$ENDIF DEBUG}
  { block selection }
  if Selection.IsBlockMode then
  begin
    R.Left := sel1.X;
    R.Right := sel2.X;
    if fbeg = fid_equal then
      R.Top := LineToRow(lBeg) + sel1.YRel
    else
      R.Top := 0;
    if fend = fid_equal then
      R.Bottom := LineToRow(lEnd) + sel2.YRel
    else
      R.Bottom := RowsAScreen - 1;

{$IFDEF DEBUG}
    // if IsConsole then
    // writeln(format('%d,%d, %d, %d', [R.Left, R.Top, R.Width, R.Height]));
{$ENDIF}
  end
  else if Selection.IsRowMode then
  begin
    { row selection }
    if fbeg = fid_equal then // selection begin is at screen
    begin
      R.Left := sel1.X;
      R.Top := LineToRow(lBeg) + sel1.YRel;
    end
    else
    begin
      R.Left := 0;
      R.Top := 0;
    end;

    if fend = fid_equal then // selection end is at screen
    begin
      R.Right := sel2.X;
      R.Bottom := LineToRow(lEnd) + sel2.YRel;
    end
    else
    begin
      R.Right := WindowCharsX;
      R.Bottom := RowsAScreen - 1;
    end;
  end;

  { common }
  if assigned(CompletelyOnScreen) then
    CompletelyOnScreen^ := (fbeg = fid_equal) and (fend = fid_equal);

  exit(True);
end;

procedure TVDCustomDisasmText.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: TFmCoord);
begin
  if FIgnoreMouseDown > 0 then
  begin
    dec(FIgnoreMouseDown);
    exit;
  end;

  inherited;
end;

procedure TVDCustomDisasmText.GetSelectionAddressesLocation(out R: TFmRect; out Text: string);
const
  DspMargin = 5;
{$IFDEF VCL}
var
  xt: WinApi.Windows.TSize;
{$ELSE}
type
  TXTSize = record
    cx, cy: TFmCoord;
  end;
var
  xt: TXTSize;
{$ENDIF}
var
  Sel: TSelection;
  va1, va2, tmp: TVA;
begin
  Sel := Selection;

  // get range
  va1 := FSelPtBegin.VA;
  va2 := FSelPtEnd.VA;

  // order
  if va1 > va2 then
  begin
    tmp := va1;
    va1 := va2;
    va2 := tmp;
  end;

  // make fmt string
  if va1 = va2 then
    Text := Format('%x', [va1])
  else
    Text := Format('%x - %x', [va1, va2]);

  // get Text dimensions
{$IFDEF VCL}
  xt := FTargetCanvas.TextExtent(Text);
{$ELSE}
  MeasureText(FTargetCanvas, Text, xt.cx, xt.cy);
{$ENDIF}

{$IFDEF VCL}
  // gfx setup
  // FTargetCanvas.Brush.Color := clWhite;
  // FTargetCanvas.Pen.Color := clBlack;
  // FTargetCanvas.Pen.Mode := pmCopy;
  // FTargetCanvas.Brush.Style := bsSolid;
{$ENDIF}
  // prepare rect
  R.Right := TextRect.Right - DspMargin;
  R.Left := R.Right - xt.cx - DspMargin - DspMargin;
  R.Top := TextRect.Top + DspMargin;
  R.Bottom := R.Top + xt.cy + DspMargin + DspMargin;
end;

procedure TVDCustomDisasmText.DrawOverride;
var
  c: IVDCore;
  block: IVDVARegion;
  Y: integer;
  curVA, TmpVA: TVA;
  color: TAlphaColor;
  bb_begin, bb_end: TVA;
begin
  if not(self is TVDDisasmText) then
    exit;

  c := CoreGet;

  curVA := c.GetUserVA;

  // Override basic block color.
  if curVA <> BAD_VA then
  begin

    block := c.BasicBlocks.Get(curVA);
    if assigned(block) then
    begin
      bb_begin := block.GetStartVA;
      bb_end := block.GetEndVA;

      // Find start of VA block.
      Y := CaretY;
      while (Y > 0) and (LineToVA(Y - 1, TmpVA)) and (TmpVA >= bb_begin) do
        dec(Y);

      // Override.
      color := VclFmxColor(COLOR_BASIC_BLOCK);
      while LineToVA(Y, TmpVA) and (TmpVA < bb_end) do
      begin
        FillRow(Y, @color, nil, True);
        inc(Y);
      end;
    end;
  end;
end;

procedure TVDCustomDisasmText.DrawSelectionAddresses;
var
  Str: string;
  R: TFmRect;
begin
  GetSelectionAddressesLocation(R, Str);

{$IFDEF VCL}
  // gfx setup
  FTargetCanvas.Brush.color := clWhite;
  FTargetCanvas.Pen.color := clBlack;
  FTargetCanvas.Pen.Mode := pmCopy;
  FTargetCanvas.Brush.Style := bsSolid;
{$ENDIF}

{$IFDEF VCL}
  // border
  FTargetCanvas.Rectangle(R);
  // text
  FTargetCanvas.Font.color := clBlack;
  FTargetCanvas.TextRect(R, Str, [tfCenter, tfVerticalCenter, tfSingleLine]);
{$ELSE}
  FTargetCanvas.Fill.Kind := TBrushKind.bkSolid;

  FTargetCanvas.Fill.color := TAlphaColorRec.White;
  FTargetCanvas.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);

  FTargetCanvas.Fill.color := TAlphaColorRec.Black;
  FTargetCanvas.DrawRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);

  FTargetCanvas.FillText(R, Str, false, ABSOLUTE_OPACITY, [], TTextAlign.taCenter);
{$ENDIF}
end;

function TVDCustomDisasmText.DoScrollOneLine(dY: integer; var VA: TVA): boolean;
begin
  result := FCore.GetDecoder.ItemStep(@VA, dY);
end;

function TVDCustomDisasmText.DoScroll(dX, dY: integer; var Pos: TPoint): boolean;
var
  newVA: TVA;
  i, stp: integer;
begin
  inherited;

  newVA := VA;

  if dY > 0 then
    stp := 1
  else
    stp := -1;

  // Scroll items.
  for i := 1 to abs(dY) do
    if not DoScrollOneLine(stp, newVA) then
      break;

  result := VA <> newVA;

  if result then
    FVA := newVA;
end;

procedure TVDCustomDisasmText.SelectionScreen;
begin
  inherited;

  FSelPtBegin.VA := RowToVA(Selection.R.Top);
  FSelPtBegin.X := Selection.R.Left;
  FSelPtBegin.YRel := 0;

  FSelPtEnd.VA := RowToVA(Selection.R.Bottom);
  FSelPtEnd.X := Selection.R.Right;
  FSelPtEnd.YRel := 1;
end;

procedure TVDCustomDisasmText.SelectionStart(const Pos: TPoint);
begin
  inherited;
  GrabSelPoint(FSelPtBegin);
end;

procedure TVDCustomDisasmText.SelectionUpdate(const Pos: TPoint; BlockMode: boolean);
begin
  inherited;
  GrabSelPoint(FSelPtEnd);
end;

procedure TVDCustomDisasmText.SetCursorVA(VA: TVA);
begin
  if GetCursorVA <> VA then
    if FCore.GetVM.Exists(VA) then
      SetCursorVAForced(VA);
end;

procedure TVDCustomDisasmText.SetCursorVAForced(VA: TVA);
var
  L: integer;
begin
  if FindVAInData(VA, @L) = fid_equal then
  begin
    // CaretY := LineToRow(L);
    DoVaChanged;
    RefreshCaretPosition;
  end
  else
    SetVA(VA);
end;

procedure TVDCustomDisasmText.SetDumpWidth(V: integer);
begin
  if FDumpWidth <> V then
  begin
    FDumpWidth := V;
    InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TVDCustomDisasmText.SetVA(VAddr: TVA);
var
  AlignedVA: TVA;
begin
  UpdateReadyState;

  if (VAddr = FVA) then
    exit;

  FVA := VAddr;

  if (VAddr = BAD_VA) then
    exit;

  AlignedVA := FVA;
  if FCore.GetDecoder.ItemStart(@AlignedVA) then
    FVA := AlignedVA;

  UpdateCoreVAPos;

  // don't change order
  ReBuildLines;

  CaretY := 0;

  // DoVaChanged;

  RefreshCaretPosition;
  FmInvalidate;
end;

procedure TVDCustomDisasmText.UpdateReadyState;
begin
  if (FCore <> nil) then
    if (FCore.IsDatabaseOpened) then
      if (FCore.VM.Sections.Count <> 0) then
      begin
        FControlIsReady := True;
        exit;
      end;
  FControlIsReady := false;
end;

// -----------------------------------------------------------------------------
// scrollbar functions
// -----------------------------------------------------------------------------

procedure TVDCustomDisasmText.UpdateScrollBarV;
var
  TmpVA: TVA;
  Pos: TVDVAPos;
  mapped: integer;
begin
  if (ScrollBarV = nil) or (FScrollBarVChanging) then
    exit;
  FHandleScrollBarVChanged := false;
  try
    ScrollBarV.Min := 0;
    ScrollBarV.Max := GetMaxScrollBarPos;

    // if not CoreGet.GetVAPos(@Pos) then
    if not GetScreenVA(Pos) then
      raise Exception.Create('VA is n/a');
    TmpVA := Pos.ScrVA;

    mapped := FCore.GetVM.GetSections.MapVAToContigiousPos(TmpVA, 0, GetMaxScrollBarPos());

{$IFDEF VCL}
    ScrollBarV.Position := mapped;
{$ELSE}
    FScrollBarV.Value := mapped;
{$ENDIF}
  finally
    FHandleScrollBarVChanged := True;
  end;
end;

{$IFDEF VCL}


procedure TVDCustomDisasmText.WMGetDlgCode(var Message: TWMNoParams);
begin
  inherited;
  Message.result := Message.result or DLGC_WANTTAB or DLGC_WANTARROWS;
end;
{$ENDIF}


function TVDCustomDisasmText.SelectionGetPaintRect(out R: TRect): boolean;
begin
  result := MapSelectionToScreenSelection(R);
end;

procedure TVDCustomDisasmText.DoScrollBarVChanged(Sender: TObject);
var
  VA: TVA;
  ScrollbarValue: integer;
  TmpPos: TPoint;
begin
  inherited;

  if not FHandleScrollBarVChanged then
    exit;

  FScrollBarVChanging := True;
  try

    // Changing scrollbar, lines must be rebuilt and redrawn.
    InvalidateAllRowsWithRebuilding;

{$IFDEF VCL}
    ScrollbarValue := ScrollBarV.Position;
{$ELSE}
    ScrollbarValue := Trunc(FScrollBarV.Value);
{$ENDIF}
    // manual correction.
    if ScrollbarValue = ScrollBarV.Min then
    begin
      GoTop(TmpPos);
      exit;
    end;
    if ScrollbarValue = ScrollBarV.Max then
    begin
      GoBottom(TmpPos);
      exit;
    end;

    VA := FCore.GetVM.GetSections.MapContigiousPosToVA(ScrollbarValue, 0, GetMaxScrollBarPos());

    if VA <> BAD_VA then
    begin
      self.VA := VA;
    end;

  finally
    FScrollBarVChanging := false;
  end;
end;

// -----------------------------------------------------------------------------

{ TVDDisasmText }

constructor TVDDisasmText.Create(AOwner: TComponent);
begin
  inherited;
  // FGutter.Width := 32;
end;

procedure TVDDisasmText.CreateLine(const c: IVDCore; VA: PVA;
const Layout: IVDVATextLayout);
begin
  c.GetDecoder.DecodeToText(VA^, Layout);
end;

// procedure TVDDisasmText.GoLineDown(var Pos: TPoint);
// begin
// if (Pos.Y + 1 < RowsUsed) and (LineToVA(Pos.Y) <> LineToVA(Pos.Y + 1)) then
// inc(Pos.Y);
// while (Pos.Y + 1 < RowsUsed) and (LineToVA(Pos.Y) = LineToVA(Pos.Y + 1)) do
// inc(Pos.Y);
// end;
//
// procedure TVDDisasmText.GoLineUp(var Pos: TPoint);
// var
// VA: TVA;
// begin
// // todo: if there's one line on screen it cause problem
// VA := LineToVA(Pos.Y);
// repeat
// dec(Pos.Y);
// until LineToVA(Pos.Y) <> VA;
// end;

{ TSelPoint }

procedure TSelPoint.Clear;
begin
  VA := BAD_VA;
  X := 0;
  YRel := 0;
end;

class procedure TSelPoint.Order(var A, B: TSelPoint);
var
  t: TSelPoint;
begin
  if A.VA > B.VA then
  begin
    t := A;
    A := B;
    B := t;
  end;
end;

end.
