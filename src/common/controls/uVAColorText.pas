{
  Colored text with VA addresses as data
}
unit uVAColorText;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.UITypes,

  uColorText,
  uColorText.Types,

  VDAPI;

type
  TVADataObject = class(TLineDataObject)
  public
    VA: TVA;
  end;

  TFindVAInData = (fid_not_found, fid_less, fid_equal, fid_greater);

  TOnAddLine = function(First: boolean; var VA: TVA; var Text: IVDVATextLayout;
    var Size: integer): boolean of object;

  TVAColorText = class(TColorText)
  protected
    FVA: TVA; // todo: FVA seems to be not needed when start va is taken from CoreData.
    FOnAddLine: TOnAddLine;
    FOnVaChanged: TNotifyEvent;

    // Some derived controls like Hex-edit don't need to update core va pos.
    FNeedUpdateCoreVAPos: boolean;

    function FCore: IVDCore; inline;

    procedure DoVaChanged; virtual;

    // procedure BuildLines; override;
    procedure CaretMoved; override;

    // Set current VA-position (in core) Screen-Top and User VA.
    // UpdateUserVA can disable updating user VA. It can be useful after scroll
    // because only Screen VA changed.
    procedure UpdateCoreVAPos(UpdateUserVA: boolean = True); virtual;
  protected
{$IFDEF VCL}
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
{$ELSE}
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;

    function Add(const S: string; VA: TVA): boolean; reintroduce;
    procedure LinesClear; override;
    function LineToVA(Line: integer; out VA: TVA): boolean; inline;
    function RowToVA(Row: integer): TVA;
    function VAToLine(VA: TVA): integer;

    procedure SetVA(Value: TVA); virtual;
    procedure SetVAEx(VA: TVA; NotifyVaChanged: boolean);

    function GetCursorVA: TVA; virtual;

    procedure SetCursorVa(const Value: TVA);
    procedure SetCursorVaEx(const Value: TVA; NotifyVaChanged: boolean);

    function IsVAOnScreen(const VA: TVA): boolean; inline;

    // Just update VA/Cursor VA w/o notification or redraw.
    procedure UpdateVA(Value: TVA); inline;
    procedure UpdateCursorVA(Value: TVA);

    { Nav }
    function NavigateToVA(VA: TVA; GoNearest: boolean = True; WantsBack: boolean = True): boolean;

    function NavigateBack: boolean;
    function NavigateForward: boolean;

    procedure NavigateToBegin;
    procedure NavigateToEnd;

    function NavigateToHighlightedVA: boolean;
    function NavigateToExpression(const Expr: string): boolean;
    function NavigateToExpressionAtCursor: boolean;

    // Search line data for VA.
    // todo: binary search for FindVAInData
    function FindVAInData(VA: TVA; { out } Line: PInteger): TFindVAInData;

    function GetVaAtCursor(out VA: TVA): boolean;
    function GetVaAtCursorOrCursorVA(out VA: TVA): boolean;

    procedure ForceVAChanged;

    function DataGet(Y: integer): TVADataObject; reintroduce; inline;

    procedure InvalidateVA(VA: TVA);

    function CanCreateCaret: boolean; override;
  published
    property VA: TVA read FVA write SetVA;
    property CursorVa: TVA read GetCursorVA write SetCursorVa;
    property OnAddLine: TOnAddLine read FOnAddLine write FOnAddLine;
    property OnVaChanged: TNotifyEvent read FOnVaChanged write FOnVaChanged;
  end;

implementation

{ TVAColorText }

function TVAColorText.Add(const S: string; VA: TVA): boolean;
var
  o: TVADataObject;
begin
  o := TVADataObject.Create;
  o.VA := VA;
  result := AddData(S, o);
  if not result then
    o.Free;
end;

procedure TVAColorText.LinesClear;
var
  i: integer;
  itm: TLineItem;
begin
  for i := 0 to LineCount - 1 do
  begin
    itm := LineItems[i];
    itm.Str := '';
    if itm.Data <> nil then
      FreeAndNil(itm.Data);
    LineItems[i] := itm;
  end;

  inherited;
end;

procedure TVAColorText.SetCursorVa(const Value: TVA);
begin
  SetCursorVaEx(Value, True);
end;

procedure TVAColorText.SetCursorVaEx(const Value: TVA; NotifyVaChanged: boolean);
var
  fLine: integer;
begin
  if FindVAInData(Value, @fLine) = fid_equal then // VA is on screen
  begin
    RefreshCaretPosition;
    UpdateCoreVAPos;
    FmInvalidate;
    exit;
  end;

  SetVAEx(Value, NotifyVaChanged);
end;

procedure TVAColorText.SetVA(Value: TVA);
begin
  SetVAEx(Value, True);
end;

procedure TVAColorText.SetVAEx(VA: TVA; NotifyVaChanged: boolean);
begin
  if FVA <> VA then
  begin
    FVA := VA;

    UpdateCoreVAPos;
    InvalidateAllRowsWithRebuilding;

    if NotifyVaChanged then
      DoVaChanged;
    RefreshCaretPosition;
    FmInvalidate;
  end;
end;

procedure TVAColorText.UpdateCoreVAPos(UpdateUserVA: boolean);
var
  pos: TVDVAPos;
begin
  if not FNeedUpdateCoreVAPos then
    exit;

  pos.ScrVA := FVA;
  if UpdateUserVA then
    pos.CurVA := GetCursorVA // changed to address at cursor
  else
    pos.CurVA := FCore.GetUserVA; // not changed
  pos.X := CaretX;
  FCore.ChangeVAPos(@pos, False);
end;

procedure TVAColorText.UpdateCursorVA(Value: TVA);
begin
  SetCursorVaEx(Value, False);
end;

procedure TVAColorText.UpdateVA(Value: TVA);
begin
  SetVAEx(Value, False);
end;

// procedure TVAColorText.BuildLines;
// var
// First: boolean;
// VA: TVA;
// Size: integer;
// Text: IVDVATextLayout;
// begin
// LinesClear;
//
// if Assigned(FOnAddLine) then
// begin
// VA := BAD_VA;
// First := True;
// Text := CreateVATextLayout(TVDTextFlag.Plain);
// while True do
// begin
// Text.Clear();
// Size := 0;
// if not FOnAddLine(First, VA, Text, Size) then
// break;
// if Size = 0 then
// break;
// if not Add(Text.Get, VA) then
// break;
// inc(VA, Size);
// First := False;
// end;
// end;
// end;

function TVAColorText.CanCreateCaret: boolean;
var
  c: IVDCore;
begin
  c := FCore;
  if Assigned(c) and (c.IsDatabaseOpened) then
    exit(True);
  exit(False);
end;

procedure TVAColorText.CaretMoved;
begin
  inherited;
  DoVaChanged;
end;

constructor TVAColorText.Create(AOwner: TComponent);
begin
  inherited;
  FNeedUpdateCoreVAPos := True;
end;

function TVAColorText.DataGet(Y: integer): TVADataObject;
begin
  result := TVADataObject(inherited DataGet(Y));
end;

procedure TVAColorText.DoVaChanged;
begin
  if LineCount <> 0 then
  begin
    if Assigned(FOnVaChanged) then
      FOnVaChanged(Self);
    UpdateScrollBarV;
  end;
end;

function TVAColorText.FindVAInData(VA: TVA; { out } Line: PInteger): TFindVAInData;
var
  i, max: integer;
  tmpVA: TVA;
begin
  result := fid_not_found;

  if VA = BAD_VA then
    exit;

  if LineCount = 0 then
    exit;

  // Is before first line?
  LineToVA(0, tmpVA);
  if VA < tmpVA then
    exit(fid_less);

  // Is after last line?
  LineToVA(LineCount - 1, tmpVA);
  if VA > tmpVA then
    exit(fid_greater);

  max := LineCount - 1;
  for i := 0 to max do
  begin
    LineToVA(i, tmpVA);
    if (VA >= tmpVA) then
    begin
      LineToVA(i + 1, tmpVA);
      if (i = max) or (VA < tmpVA) then
      begin
        if Line <> nil then
          Line^ := i;
        exit(fid_equal);
      end;
    end;
  end;
end;

procedure TVAColorText.ForceVAChanged;
begin
  DoVaChanged;
  InvalidateAllRowsWithRebuilding;
end;

function TVAColorText.FCore: IVDCore;
begin
  result := CoreGet;
end;

function TVAColorText.GetCursorVA: TVA;
begin
  if not LineToVA(CaretY, result) then
    result := BAD_VA;
end;

procedure TVAColorText.InvalidateVA(VA: TVA);
begin
  // todo: TVAColorText.InvalidateVA
  // Now it invalidates all lines, but it must invalidate only VA.
  InvalidateAllRowsWithRebuilding;
end;

function TVAColorText.IsVAOnScreen(const VA: TVA): boolean;
begin
  result := FindVAInData(VA, nil) = fid_equal;
end;

{$IFDEF VCL}


procedure TVAColorText.KeyDown(var Key: Word; Shift: TShiftState);
{$ELSE}


procedure TVAColorText.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
{$ENDIF}
begin
  // Alt + Left/Right will navigate prev/next VA (like in browser).
  if ssAlt in Shift then
  begin
    case Key of
      vkLeft:
        begin
          NavigateBack;
          exit;
        end;
      vkRight:
        begin
          NavigateForward;
          exit;
        end;
    end;
  end;

  inherited;
end;

function TVAColorText.VAToLine(VA: TVA): integer;
var
  LineY: integer;
begin
  if FindVAInData(VA, @LineY) = fid_equal then
    result := LineY
  else
    result := -1;
end;

function TVAColorText.RowToVA(Row: integer): TVA;
begin
  if not LineToVA(Row, result) then
    result := BAD_VA;
end;

function TVAColorText.LineToVA(Line: integer; out VA: TVA): boolean;
var
  o: TVADataObject;
begin
  o := TVADataObject(Self.DataGet(Line));
  if not Assigned(o) then
    exit(False);
  VA := o.VA;
  exit(True);
end;

{
  function TVAColorText.NavigateToVA(VA: TVA; GoNearest: boolean; WantsBack: boolean): boolean;
  var
  sCur, sNext: IVDSection;
  begin
  if (FCore = nil) then
  exit(False);
  if (not Enabled) or (VA = BAD_VA) or (CursorVa = VA) then
  exit(False);

  if FCore.Decoder.ItemStart(@VA) then
  result := True
  else
  begin
  FCore.GetVM.GetSections.FindEx(VA, nil, @sCur, @sNext);
  result := sCur <> nil;
  end;

  if (not result) and (GoNearest) and (sNext <> nil) then
  begin
  VA := sNext.GetStartVa;
  result := True;
  end;

  if result then
  begin
  ResetCurrentFieldRec;

  if FNeedUpdateCoreVAPos then
  begin
  FCore.ChangeVA(VA, WantsBack);
  end
  else
  begin
  FVA := VA;
  DoVaChanged;
  InvalidateAllRowsWithRebuilding;
  end;

  SetVAEx(VA, False);
  end;
  end;
}
function TVAColorText.NavigateToVA(VA: TVA; GoNearest: boolean; WantsBack: boolean): boolean;
begin
  SelectedTextSingleLine := '';
  ResetCurrentFieldRec;
  result := FCore.ChangeVA(VA, WantsBack);
end;

function TVAColorText.NavigateBack: boolean;
begin
  SelectedTextSingleLine := '';
  ResetCurrentFieldRec;
  result := FCore.NavigateDirection(-1);
end;

function TVAColorText.NavigateForward: boolean;
begin
  SelectedTextSingleLine := '';
  ResetCurrentFieldRec;
  result := FCore.NavigateDirection(1);
end;

procedure TVAColorText.NavigateToBegin;
var
  CurVA: TVA;
begin
  SelectedTextSingleLine := '';
  if FCore.VM.GetFirstVA(@CurVA) then
    NavigateToVA(CurVA);
end;

procedure TVAColorText.NavigateToEnd;
var
  CurVA: TVA;
begin
  SelectedTextSingleLine := '';
  if FCore.VM.GetLastVa(@CurVA) then
    NavigateToVA(CurVA);
end;

function TVAColorText.NavigateToExpression(const Expr: string): boolean;
var
  newVA: TVA;
begin
  if FCore.EvaluateVA(BSTR_IN(Expr), newVA) then
  begin
    if FCore.GetVM.Exists(newVA) then
      exit(NavigateToVA(newVA, True));
  end;
  exit(False);
end;

function TVAColorText.NavigateToExpressionAtCursor: boolean;
var
  w: string;
begin
  w := WordFromPos(CaretPos);
  if w <> '' then
    result := NavigateToExpression(w)
  else
    result := False;
end;

function TVAColorText.NavigateToHighlightedVA: boolean;
begin
  if (SelectedTextSingleLine <> '') then
    result := NavigateToExpression(SelectedTextSingleLine)
  else
    result := False;
end;

function TVAColorText.GetVaAtCursor(out VA: TVA): boolean;
var
  w: string;
begin
  w := WordFromPos(CaretPos);
  if w <> '' then
    exit(FCore.EvaluateVA(BSTR_IN(w), VA));
  exit(False);
end;

function TVAColorText.GetVaAtCursorOrCursorVA(out VA: TVA): boolean;
begin
  result := GetVaAtCursor(VA);
  if not result then
  begin
    VA := GetCursorVA;
    result := VA <> BAD_VA;
  end;
end;

end.
