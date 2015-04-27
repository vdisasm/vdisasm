unit uColorText.Draw;

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Generics.Collections,

{$IFDEF VCL}
  Vcl.Graphics,
{$ELSE}
  FMX.Graphics,
  FMX.Types,
{$ENDIF}
  uColorText.Selection,
  uColorText.Types;

const
  MAX_SCR_WDT = 10000;
  MAX_SCR_HGT = 10000;

type
  TCellCoordsToPoint = function(x0, Y0: TColRowCoord): TFmPoint of object;
  TCellGet = function(Col, Row: integer): TCell of object;
  TLineIsValid = function(Line: integer): Boolean of object;
  TCellColumnToX = function(Column: TColRowCoord): TFmCoord of object;
  TLineToRowRange = procedure(Line: integer; out Y0, Y1: TColRowCoord) of object;

  TDrawer = class
  public
    class procedure DrawClearCanvas(const cv: TCanvas; const R: TFmRect; Color: TAlphaColor);

    class procedure DrawCells(
      const cv: TCanvas;
      x, y, w, h: integer;
      CharWidth, CharHeight: TFmCoord;
      // RowsAScreen, WindowCharsX: TColRowCoord;
      CellCoordsToPoint: TCellCoordsToPoint;
      CellGet: TCellGet;
      IdRecs: TIdRecords
      );

    // Draw gutter. It's already checked to be > 0.
    class procedure DrawGutter(const cv: TCanvas; const Gutter: TGutter; const TextRect: TFmRect);

    class procedure DrawHighlighting(
      const cv: TCanvas;
      const Text: string;
      const Selection: TSelection;
      const Colors: TIDRecord;
      RowsUsed, WindowCharsX: TColRowCoord;
      CellGet: TCellGet;
      CellColumnToX, CellRowToY: TCellColumnToX
      );

    // class procedure DrawLineHighlight(
    // const cv: TCanvas;
    // Line: integer;
    // Color: TAlphaColor;
    // const Selection: TSelection;
    // LineIsValid: TLineIsValid;
    // CellColumnToX, CellRowToY: TCellColumnToX;
    // const TextRect: TFmRect;
    // LineToRowRange: TLineToRowRange
    // );

    class procedure DrawCurrentItemBorder(
      const cv: TCanvas;
      const Rec: TFieldRecord;
      nCharX, nCharY: TFmCoord);

    class procedure DrawVLine(const cv: TCanvas; const Col: integer;
      nCharX, ClientHeight: TFmCoord;
      CellCoordsToPoint: TCellCoordsToPoint);

    class procedure Rectangle(const cv: TCanvas; const R: TFmRect);
  end;

  TDrawerClass = class of TDrawer;

implementation

procedure DrawText(
  const cv: TCanvas;
  const Pt: TFmPoint; const idr: TIDRecord; const Text: string); {$IFNDEF DEBUG}inline; {$ENDIF}
{$IFNDEF VCL}
var
  ARect: TFmRect;
{$ENDIF}
begin
  if Text = '' then
    Exit;

{$IFDEF VCL}
  cv.Brush.Color := idr.Bg;
  cv.Font.Color := idr.Fg;
  cv.TextOut(Pt.x, Pt.y, Text);
{$ELSE}
  ARect := TFmRect.Create(0, 0, 10000, 50);
  cv.MeasureText(ARect, Text, false, [], TTextAlign.Leading);
  ARect.Offset(Pt);

  cv.Fill.Color := idr.BgColor;
  cv.FillRect(ARect, 0, 0, AllCorners, ABSOLUTE_OPACITY);

  cv.Fill.Color := idr.FgColor;
  cv.FillText(ARect, Text, false, ABSOLUTE_OPACITY, [], TTextAlign.Leading);
{$ENDIF}
end;

// Slow char-by-char printing.
// It can be used to debug-check if cells are filled correctly.
procedure DrawCellsDummy(
  const cv: TCanvas;
  x, y, w, h: integer;
  CharWidth, CharHeight: TFmCoord;
  // RowsAScreen, WindowCharsX: TColRowCoord;
  CellCoordsToPoint: TCellCoordsToPoint;
  CellGet: TCellGet;
  IdRecs: TIdRecords
  );
var
  CurY, CurX: integer;
  c: TCell;
  Pt: TFmPoint;
begin
{$IFDEF VCL}
  with cv do
  begin
    Pen.Mode := pmCopy;
    Brush.Style := bsClear;
  end;
{$ELSE}
{$FATAL 'DrawCells'}
{$ENDIF}
  for CurY := y to y + h - 1 do
    for CurX := x to x + w - 1 do
    begin
      c := CellGet(CurX, CurY);

      // if c.Sym = #0 then
      // c.Sym := '*';

      if c.Sym <> #0 then
      begin
        Pt := CellCoordsToPoint(CurX, CurY);
        cv.Brush.Color := c.IdRec.Bg;
        cv.Font.Color := c.IdRec.Fg;
        cv.TextOut(Pt.x, Pt.y, c.Sym);
      end;
    end;
end;

class procedure TDrawer.DrawCells;
var
  CurX, CurY: integer;
  cur, prev: TCell;
  s: string;
  PresentX: integer;
  LastColumn: integer;
  IsLineEnd: Boolean;
begin
  // DrawCellsDummy(cv, x, y, w, h, CharWidth, CharHeight, CellCoordsToPoint, CellGet, IdRecs);
  // Exit;

{$IFDEF VCL}
  with cv do
  begin
    Pen.Mode := pmCopy;
    Brush.Style := bsClear;
  end;
{$ELSE}
{$FATAL 'DrawCells'}
{$ENDIF}
  LastColumn := x + w - 1;
  for CurY := y to y + h - 1 do
  begin
    CurX := x;
    s := '';
    PresentX := x;
    IsLineEnd := false;
    while not IsLineEnd do
    begin
      IsLineEnd := CurX > LastColumn;

      cur := CellGet(CurX, CurY);

      if (cur.Sym = #0) and (cur.IdRec.IsNull) then
      begin
        cur.IdRec := IdRecs.Get(0);
      end;

      if IsLineEnd or
        ((cur.Sym = #0) and (prev.Sym <> #0)) or
        ((cur.Sym <> #0) and (prev.Sym = #0)) or
        ((CurX <> x) and (prev.IdRec <> cur.IdRec)) then
      begin
        if (s <> '') then
        begin
          DrawText(cv, CellCoordsToPoint(PresentX, CurY), prev.IdRec, s);
          s := '';
        end;
        PresentX := CurX;
      end;

      prev := cur;
      inc(CurX, cur.SafeWidth);

      if cur.Sym <> #0 then
        s := s + cur.Sym
      else
        s := s + ' ';
    end;
  end;
end;

class procedure TDrawer.DrawClearCanvas(const cv: TCanvas; const R: TFmRect; Color: TAlphaColor);
begin
{$IFDEF VCL}
  cv.Brush.Style := bsSolid;
  cv.Brush.Color := Color;
  cv.FillRect(R);
{$ELSE}
  cv.Fill.Kind := TBrushKind.bkSolid;
  cv.Fill.Color := Color;
  cv.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);
{$ENDIF}
end;

class procedure TDrawer.DrawCurrentItemBorder;
begin
end;

class procedure TDrawer.DrawGutter(const cv: TCanvas; const Gutter: TGutter; const TextRect: TFmRect);
var
  R: TFmRect;
begin
  R := TextRect;
  R.Left := 0;
  R.Right := Gutter.Width;
{$IFDEF VCL}
  cv.Brush.Color := Gutter.Color;
  cv.FillRect(R);
{$ELSE}
  cv.Fill.Color := Gutter.Color;
  cv.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);
{$ENDIF}
end;

procedure PreprocessHighlightText(var Text: string);
begin
  if (copy(Text, 1, 2) = '0x') or (copy(Text, 1, 2) = '0b') then
    Delete(Text, 1, 2);
end;

class procedure TDrawer.DrawHighlighting;
var
  x, y, Xs, j: integer;
  found: Boolean;
  LText: string;
  c: TCell;
{$IFNDEF VCL}
  R: TFmRect;
{$ENDIF}
begin
  LText := Text;
  if LText = '' then
    Exit;
  if Selection.RowsSelected > 0 then
    Exit;

  PreprocessHighlightText(LText);

{$IFDEF VCL}
  cv.Brush.Style := bsSolid;
  cv.Brush.Color := Colors.Bg;
  cv.Font.Color := Colors.Fg;
{$ELSE}
  cv.Fill.Kind := TBrushKind.bkSolid;
  cv.Fill.Color := Colors.Bg;
{$ENDIF}
  for y := 0 to RowsUsed - 1 do
  begin
    x := 0;
    while x < WindowCharsX do
    begin

      // Search text to highlight.
      found := True;
      Xs := x;
      for j := 1 to length(LText) do
      begin
        c := CellGet(Xs, y);
        if LText[j] <> c.Sym then
        begin
          found := false;
          break;
        end;
        inc(Xs, c.SafeWidth);
      end;

      // If found, draw it.
      if found then
      begin
{$IFDEF VCL}
        cv.TextOut(CellColumnToX(x), CellRowToY(y), LText);
{$ELSE}
        R.Left := CellColumnToX(x);
        R.Top := CellRowToY(y);
        R.Width := cv.TextWidth(LText);
        R.Height := cv.TextHeight(LText);

        cv.Fill.Color := Colors.BgColor;
        cv.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);

        cv.Fill.Color := Colors.FgColor;
        cv.FillText(R, LText, false, ABSOLUTE_OPACITY, [], TTextAlign.taCenter);
{$ENDIF}
      end;

      x := Xs + 1;
    end;
  end;

end;

// class procedure TDrawer.DrawLineHighlight;
// var
// R: TFmRect;
// Y0, Y1: integer;
// begin
// if Selection.IsActive then
// Exit;
// if not LineIsValid(Line) then
// Exit;
//
// R.Left := CellColumnToX(0);
// R.Right := TextRect.Right;
//
// LineToRowRange(Line, Y0, Y1);
//
// R.Top := CellRowToY(Y0);
// R.Bottom := CellRowToY(Y1);
//
// {$IFDEF VCL}
// cv.Brush.Color := Color;
// cv.Pen.Mode := pmNotXor;
// cv.Pen.Color := Color;
// cv.Rectangle(R);
// {$ELSE}
// cv.Fill.Color := Color;
// cv.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);
// {$ENDIF}
// end;

{$IFDEF VCL}


class procedure TDrawer.DrawVLine(const cv: TCanvas; const Col: integer;
  nCharX, ClientHeight: integer; CellCoordsToPoint: TCellCoordsToPoint);
var
  Pt: TFmPoint;
begin
  Pt := CellCoordsToPoint(Col, 0);
  dec(Pt.x, nCharX div 2);
  cv.MoveTo(Pt.x, Pt.y);
  Pt.y := ClientHeight;
  cv.LineTo(Pt.x, Pt.y);
end;
{$ELSE}


class procedure TDrawer.DrawVLine(const cv: TCanvas; const Col: integer;
  nCharX, ClientHeight: TFmCoord; CellCoordsToPoint: TCellCoordsToPoint);
begin
  // todo: DrawVLine
end;
{$ENDIF}


class procedure TDrawer.Rectangle(const cv: TCanvas; const R: TFmRect);
begin
{$IFDEF VCL}
  cv.Rectangle(R);
{$ELSE}
  cv.FillRect(R, 0, 0, AllCorners, ABSOLUTE_OPACITY);
{$ENDIF}
end;

end.
