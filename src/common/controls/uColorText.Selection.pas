unit uColorText.Selection;

interface

uses
  System.Types; // TPoint

type
{$SCOPEDENUMS ON}
  TSelectionFlag =
    (
    rows,         // row-mode selection
    block,        // block-mode selection
    locked_first, //
    locked_last   //
    );
{$SCOPEDENUMS OFF}
  TSelectionFlags = set of TSelectionFlag;

  TSelection = record
  strict private
    Flags: TSelectionFlags;
  public
    // Selection rectangle.
    // Use SelectionGetPaintRect to get actual selected rect in overriden class.
    R: TRect;

    // Selection is Active when:
    // - there is Row/Block mode
    // - selected area isn't null
    function IsActive: Boolean; inline;

    // Manage Row/Block mode.
    function IsRowMode: Boolean; inline;
    function IsBlockMode: Boolean; inline;
    procedure SetRowMode; inline;
    procedure SetBlockMode;

    // Manage Start/End points.
    procedure SetStartPoint(const Pt: TPoint); inline;
    procedure SetEndPoint(const Pt: TPoint); inline;

    function RowsSelected: Integer; inline;
    function ColsSelected: Integer; inline;

    // Order Recatnage points.
    procedure OrderR;

    procedure SetLockState(IsFirst, Locked: Boolean);
    function GetLockState(IsFirst: Boolean): Boolean;

    // Clear selection.
    procedure Clear;
  end;

implementation

{ TSelection }

function TSelection.IsActive: Boolean;
begin
  Result := ((TSelectionFlag.rows in Flags) or (TSelectionFlag.block in Flags));
end;

function TSelection.IsBlockMode: Boolean;
begin
  Result := TSelectionFlag.block in Flags;
end;

function TSelection.IsRowMode: Boolean;
begin
  Result := TSelectionFlag.rows in Flags;
end;

procedure TSelection.OrderR;
var
  t: TPoint;
begin
  if R.Top > R.Bottom then
  begin
    t := R.TopLeft;
    R.TopLeft := R.BottomRight;
    R.BottomRight := t;
  end;
end;

procedure TSelection.Clear;
begin
  // Exclude(Flags, selflag_fixed);
  // Exclude(Flags, selflag_rows);
  // Exclude(Flags, selflag_block);
  Flags := [];
  R := TRect.Empty;
end;

function TSelection.ColsSelected: Integer;
begin
  if not IsActive then
    exit(0);
  Result := abs(R.Right - R.Left);
end;

function TSelection.GetLockState(IsFirst: Boolean): Boolean;
begin
  if IsFirst then
    Result := TSelectionFlag.locked_first in Flags
  else
    Result := TSelectionFlag.locked_last in Flags;
end;

function TSelection.RowsSelected: Integer;
begin
  if not IsActive then
    exit(0);
  Result := abs(R.Bottom - R.Top);
end;

procedure TSelection.SetBlockMode;
begin
  include(Flags, TSelectionFlag.block);
  Exclude(Flags, TSelectionFlag.rows);
end;

procedure TSelection.SetRowMode;
begin
  include(Flags, TSelectionFlag.rows);
  Exclude(Flags, TSelectionFlag.block);
end;

procedure TSelection.SetStartPoint(const Pt: TPoint);
begin
  R.TopLeft := Pt;
end;

procedure TSelection.SetEndPoint(const Pt: TPoint);
begin
  R.BottomRight := Pt;
end;

procedure TSelection.SetLockState(IsFirst, Locked: Boolean);
begin
  if IsFirst then
  begin
    if Locked then
      include(Flags, TSelectionFlag.locked_first)
    else
      Exclude(Flags, TSelectionFlag.locked_first);
  end
  else
  begin
    if Locked then
      include(Flags, TSelectionFlag.locked_last)
    else
      Exclude(Flags, TSelectionFlag.locked_last);
  end;
end;

end.
