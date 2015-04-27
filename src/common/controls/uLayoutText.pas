unit uLayoutText;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,

  Vcl.StdCtrls,

  uColorText.Types,
  uColorText,

  VDAPI;

type
  TLayoutText = class(TColorText)
  private
    procedure SetY0(const Value: integer);
    // procedure LogLinesChange(Sender: TObject);
  protected
    FLayout: IVDVATextLayout;
    FY0: integer; // # of 1st line on the screen
    FScrollOnAdd: boolean;
    FHandleScrollBarVChanged: boolean;

    procedure BuildLines; override;

    function DoScroll(dX, dY: integer; var Pos: TPoint): boolean; override;
    procedure DoScrollBarVChanged(Sender: TObject); override;

    function ValidateY0(Y0: integer): integer;
    procedure UpdateLinePositions;
    procedure UpdateScrollBarV; reintroduce;

    procedure CaretMoved; override;
    function SelectionGetPaintRect(out R: TRect): boolean; override;

  public
    constructor Create(AOwner: TComponent); override;

//    procedure Clear;

    procedure GoPageDown(var Pos: TPoint); override;
    procedure GoPageUp(var Pos: TPoint); override;

  published
    property Y0: integer read FY0 write SetY0;
    property Layout: IVDVATextLayout read FLayout;
  end;

implementation

const
  IDR_NORMAL: TIDRecord  = (Bg: TColors.White; Fg: TColors.Black);
  IDR_CODE: TIDRecord    = (Bg: TColors.White; Fg: TColors.Navy);
  IDR_COMMENT: TIDRecord = (Bg: TColors.White; Fg: TColors.Green);

function Min(a, b: integer): integer; inline;
begin
  if a < b then
    exit(a);
  exit(b);
end;

procedure TLayoutText.BuildLines;
var
  y: integer;
begin
  LinesClear;

  y := FY0;
  while ((y - FY0) < RowsAScreen) and (y < FLayout.GetLineCount) do
  begin
    self.AddData(FLayout.GetLine(y));
    inc(y);
  end;
end;

procedure TLayoutText.CaretMoved;
begin
  inherited;
  UpdateScrollBarV;
end;

//procedure TLayoutText.Clear;
//begin
//  FLayout.Clear;
//  ResetCaretPos;
//  FY0 := 0;
//  FmInvalidate;
//end;

constructor TLayoutText.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLayout := CreateVATextLayout(TVDTextFlag.Tags);

  FScrollOnAdd := true;

  SetId(integer(TTag.TAGID_NONE), IDR_NORMAL);
  SetId(integer(TTag.TAGID_CODE), IDR_CODE);
  SetId(integer(TTag.TAGID_COMMENT), IDR_COMMENT);
end;

// procedure TLayoutText.LogLinesChange(Sender: TObject);
// begin
// UpdateLinePositions;
// UpdateScrollBarV;
// Invalidate;
// end;

function TLayoutText.SelectionGetPaintRect(out R: TRect): boolean;
begin
  R := Selection.R;
  Result := true;
end;

procedure TLayoutText.SetY0(const Value: integer);
var
  new: integer;
begin
  new := ValidateY0(Value);
  if new <> FY0 then
  begin
    FY0 := new;
    FmInvalidate;
  end;
end;

function TLayoutText.DoScroll(dX, dY: integer; var Pos: TPoint): boolean;
var
  NewY0: integer;
begin
  inherited;
  NewY0 := ValidateY0(FY0 + dY);
  FY0 := NewY0;
  UpdateScrollBarV;
  InvalidateAllRowsWithRebuilding;
  Result := NewY0 <> FY0;
end;

procedure TLayoutText.DoScrollBarVChanged(Sender: TObject);
begin
  inherited;
  if FHandleScrollBarVChanged then
    self.Y0 := (Sender as TScrollBar).Position;
end;

procedure TLayoutText.GoPageDown(var Pos: TPoint);
begin
  CaretY := CaretY + RowsAScreen;
end;

procedure TLayoutText.GoPageUp(var Pos: TPoint);
begin
  CaretY := CaretY - RowsAScreen;
end;

procedure TLayoutText.UpdateLinePositions;
var
  LayoutLineCount: integer;
begin
  if FScrollOnAdd then
  begin
    LayoutLineCount := FLayout.GetLineCount;
    if LayoutLineCount > RowsAScreen then
      FY0 := LayoutLineCount - RowsAScreen + 1
    else
      FY0 := 0;
  end;
end;

procedure TLayoutText.UpdateScrollBarV;
var
  LayoutLineCount: integer;
begin
  if ScrollBarV = nil then
    exit;

  FHandleScrollBarVChanged := False;
  ScrollBarV.Min := 0;

  LayoutLineCount := FLayout.GetLineCount;
  if LayoutLineCount > 0 then
    ScrollBarV.Max := LayoutLineCount - 1
  else
    ScrollBarV.Max := 0;
  ScrollBarV.Position := FY0 + CaretY;
  FHandleScrollBarVChanged := true;
end;

function TLayoutText.ValidateY0(Y0: integer): integer;
var
  LayoutLineCount: integer;
begin
  Result := Y0;
  LayoutLineCount := FLayout.GetLineCount;
  if (Y0 + RowsAScreen) > LayoutLineCount then
    Result := LayoutLineCount - RowsAScreen + 1;
  if (Y0 < 0) or (Result < 0) then
    Result := 0;
end;

end.
