unit GraphControl;

// {$DEFINE PRINT_GRAPH_NODE_LEVEL}

interface

uses
  System.Classes,
  System.Math,
  System.SysUtils,
  System.Types,

  Vcl.Graphics,
  Vcl.Controls,

  WinApi.Messages,
  WinApi.Windows,

  uGraph,
  uGraph.Geometric;

type
  TWorldPoint = TPoint;

  TNodeSelection = record
    Node: TGeometricGraphNode;
    GrabbedAt: TPoint; // relative to node left/top
  end;

  TGraphControlOption = (
    gco_SelectNode,
    gco_MoveNodeHorizontally,
    gco_MoveNodeVertically,
    gco_MoveCanvas
    );

  TGraphControlOptions = set of TGraphControlOption;

const
  DEFAULT_OPTIONS = [
    gco_SelectNode,
  // gco_MoveNodeHorizontally,
  // gco_MoveNodeVertically,
  gco_MoveCanvas];

type
  TGraphControl = class(TCustomControl)
  private
    FBmp: Vcl.Graphics.TBitmap;
    FGraph: TGeometricGraph;
    FOwnsGraph: boolean;
    FProbableConnection: TNodeSelection;
    FCanAdjustNodeBounds: boolean;
    FCanDrawEdges: boolean;
    FOptions: TGraphControlOptions;
    procedure SetGraph(const Value: TGeometricGraph);
    procedure SelectNode(const Value: TGeometricGraphNode; const Pt: TWorldPoint);
    procedure SetCanDrawEdges(const Value: boolean);
  private
    procedure FillNodeContentSize;
  protected
    FBackBuffer: TCanvas;
  protected
    FNodeSelection: TNodeSelection;
  protected
    FBackgroundColor: TColor;
    FArrowColor: TColor;
    FBackArrowColor: TColor;
    FNodeColor: TColor;
    FSelectedNodeColor: TColor;
    FInvisibleNodeColor: TColor;
  protected
    function FindRectAndLineIntersectionPoint(
      const R: TRect;
      const LinePt: TPoint;
      out Pt: TPoint): boolean;
  protected
    procedure MeasureNodeContentSize(cv: TCanvas; Node: TGeometricGraphNode; out W, H: integer); virtual;
    procedure DrawNodeContent(cv: TCanvas; Node: TGeometricGraphNode; R: TRect); virtual;
  protected
    procedure DrawArrow(X1, Y1, X2, Y2: single; Inner: boolean = True);
    procedure DrawTriangle(X1, Y1, X2, Y2, X3, Y3: single);
    procedure DrawLine(X1, Y1, X2, Y2: single);

    procedure DrawBackground;
    procedure DrawNode(Node: TGeometricGraphNode);
    procedure DrawEdge(Source, Target: TGeometricGraphNode; Edge: TGeometricGraphEdge);
    procedure DrawNodeOutEdges(Source: TGeometricGraphNode);
    procedure DrawNodes;
    procedure DrawLevel(Level: TGraphLevel; Y: integer);
    procedure DrawLevels;
  protected
    procedure Paint; override;
    procedure Resize; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: integer; Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: integer; Y: integer); override;
    procedure MouseMove(Shift: TShiftState; X: integer; Y: integer); override;
    procedure DoEnter; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: integer; MousePos: TPoint): boolean; override;
  protected
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    FWorldOffset: TPoint;
    FMouseGrabPoint: TPoint;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function ScreenToWorld(const Pt: TPoint): TWorldPoint;
    function GetNodeAtPoint(const Pt: TWorldPoint): TGeometricGraphNode;

    procedure Layout(Kind: integer); virtual;

    procedure GoToNode(Node: TGeometricGraphNode);

    property Graph: TGeometricGraph read FGraph write SetGraph;
    property OwnsGraph: boolean read FOwnsGraph write FOwnsGraph;
    property CanDrawEdges: boolean read FCanDrawEdges write SetCanDrawEdges;
    property CanAdjustNodeBounds: boolean read FCanAdjustNodeBounds write FCanAdjustNodeBounds;
    property BackBuffer: TCanvas read FBackBuffer;
    property Options: TGraphControlOptions read FOptions write FOptions;
  end;

implementation

{ TGraphControl }

constructor TGraphControl.Create(AOwner: TComponent);
begin
  inherited;

  Self.TabStop := True;

  FBmp := Vcl.Graphics.TBitmap.Create;
  FBackBuffer := FBmp.Canvas;

  FBackgroundColor := clLtGray;
  FArrowColor := clBlack;
  FBackArrowColor := clRed;
  FNodeColor := clWhite;
  FSelectedNodeColor := clSkyBlue;
  FInvisibleNodeColor := clGrayText;
end;

destructor TGraphControl.Destroy;
begin
  SetGraph(nil);
  FBmp.Free;
  inherited;
end;

procedure TGraphControl.DoEnter;
begin
  inherited;
  SetFocus;
end;

function TGraphControl.DoMouseWheel(Shift: TShiftState; WheelDelta: integer;
  MousePos: TPoint): boolean;
begin
  WheelDelta := (WheelDelta div abs(WheelDelta)) * 100;

  if ssAlt in Shift then
    inc(FWorldOffset.X, WheelDelta)
  else
    inc(FWorldOffset.Y, WheelDelta);

  Invalidate;

  Result := True;
end;

procedure TGraphControl.Paint;
begin
  inherited;
  DrawBackground;

  if not Assigned(FGraph) then
    Exit;

  if FGraph.LevelCount = 0 then
    DrawNodes
  else
    DrawLevels;
  if Assigned(FNodeSelection.Node) and Assigned(FProbableConnection.Node) then
    DrawEdge(FNodeSelection.Node, FProbableConnection.Node, nil);
  Canvas.Draw(0, 0, FBmp);
end;

procedure TGraphControl.DrawArrow(X1, Y1, X2, Y2: single; Inner: boolean);
const
  ArrowLen  = 8;
  SideAngle = Pi / 8;
var
  A: single;
  tx1, ty1, tx2, ty2: single;
begin
  A := ArcTan2((Y2 - Y1), (X2 - X1)); // get angle

  if not Inner then
  begin
    X2 := X2 + ArrowLen * cos(A);
    Y2 := Y2 + ArrowLen * sin(A);
  end;

  tx1 := X2 + (-ArrowLen * cos(A - SideAngle));
  ty1 := Y2 + (-ArrowLen * sin(A - SideAngle));
  tx2 := X2 + (-ArrowLen * cos(A + SideAngle));
  ty2 := Y2 + (-ArrowLen * sin(A + SideAngle));

  DrawLine(X1, Y1, X2, Y2); // line w/o arrow
  DrawTriangle(tx1, ty1, X2, Y2, tx2, ty2);
end;

procedure TGraphControl.DrawBackground;
begin
  FBackBuffer.Brush.Color := FBackgroundColor;
  FBackBuffer.FillRect(Rect(0, 0, FBmp.Width, FBmp.Height));
end;

procedure TGraphControl.DrawTriangle(X1, Y1, X2, Y2, X3, Y3: single);
var
  p1, p2, p3: TPoint;
begin
  p1.X := trunc(X1);
  p1.Y := trunc(Y1);
  p2.X := trunc(X2);
  p2.Y := trunc(Y2);
  p3.X := trunc(X3);
  p3.Y := trunc(Y3);
  FBackBuffer.Polygon([p1, p2, p3]);
end;

procedure TGraphControl.FillNodeContentSize;
var
  n: TBasicGraphNode;
  Node: TGeometricGraphNode;
  W, H: integer;
begin
  for n in Graph.Nodes.Values do
  begin
    Node := TGeometricGraphNode(n);
    MeasureNodeContentSize(FBackBuffer, Node, W, H);
    Node.X := 0;
    Node.Y := 0;
    Node.Width := W;
    Node.Height := H;
  end;
end;

function TGraphControl.FindRectAndLineIntersectionPoint(
  const R: TRect;
  const LinePt: TPoint;
  out Pt: TPoint): boolean;
var
  delta: TPoint;
  A: Extended;
  CPt: TPoint;
  wx, wy: single;
  BoxAngle, Angle: single;
  a0, a1, a2, a3: single;
begin
  // box angle
  BoxAngle := RadToDeg(ArcTan2(R.Height, R.Width));

  a0 := BoxAngle;
  a1 := 180 - BoxAngle;
  a2 := -a0;
  a3 := -a1;

  delta := LinePt - R.CenterPoint;
  A := ArcTan2(delta.Y, delta.X);

  CPt := R.CenterPoint;

  Angle := RadToDeg(A);

  Pt := R.CenterPoint;

  wx := R.Width / 2;
  wy := R.Height / 2;

  if (Angle >= a3) and (Angle < a2) then
  begin
    // top
    Pt.Y := R.Top;
    Pt.X := CPt.X + trunc(wy * cos(A));
  end
  else if (Angle >= -BoxAngle) and (Angle < BoxAngle) then
  begin
    // right
    Pt.X := R.right;
    Pt.Y := CPt.Y + trunc(wx * sin(A));
  end
  else if (Angle >= a0) and (Angle < a1) then
  begin
    // bottom
    Pt.Y := R.Bottom;
    Pt.X := CPt.X + trunc(wy * cos(A));
  end
  else
  begin
    // left
    Pt.X := R.Left;
    Pt.Y := CPt.Y + trunc(wx * sin(A));
  end;
  Exit(True);
end;

procedure TGraphControl.DrawLevel(Level: TGraphLevel; Y: integer);
const
  W = 25;
  H = 25;
var
  NodeIndex: integer;
  Node: TBasicGraphNode;
  GNode: TGeometricGraphNode;
  R: TRect;
begin
  R := Rect(0, Y - (H div 2), 0 + W, Y + (H div 2));

  for NodeIndex := 0 to Level.Nodes.Count - 1 do
  begin
    Node := Level.Nodes[NodeIndex];
    GNode := TGeometricGraphNode(Node);

    if FCanAdjustNodeBounds then
      GNode.SetBoundRect(R);

    DrawNode(GNode);

    if FCanDrawEdges then
      DrawNodeOutEdges(GNode);

    // DrawNode(nil, @R, Format('%s', [GNode.Caption]));
    R.Offset(W * 2, 0);
  end;
end;

procedure TGraphControl.DrawLevels;
var
  LevelIndex: integer;
  Level: TGraphLevel;
  Y: integer;
  OldPenStyle: TPenStyle;
begin
  Y := 50;

  for LevelIndex := 0 to FGraph.LevelCount - 1 do
  begin
    Level := FGraph.Levels[LevelIndex];

    OldPenStyle := FBackBuffer.Pen.Style;
    FBackBuffer.Pen.Style := psDash;
    FBackBuffer.Pen.Color := clBlack;
    FBackBuffer.Brush.Style := bsSolid;
    FBackBuffer.MoveTo(0, Y);
    FBackBuffer.LineTo(Width, Y);
    FBackBuffer.Pen.Style := OldPenStyle;

    DrawLevel(Level, Y);

    inc(Y, 50);
  end;

  if FCanAdjustNodeBounds then
    FCanAdjustNodeBounds := False;
end;

procedure TGraphControl.DrawLine(X1, Y1, X2, Y2: single);
begin
  FBackBuffer.MoveTo(trunc(X1), trunc(Y1));
  FBackBuffer.LineTo(trunc(X2), trunc(Y2));
end;

procedure TGraphControl.DrawNode(Node: TGeometricGraphNode);
var
  R: TRect;
begin
  if Assigned(Node) then
  begin
    R := Node.GetBoundRect;
    R.Offset(FWorldOffset);
    DrawNodeContent(FBackBuffer, Node, R);
  end;
end;

procedure TGraphControl.MeasureNodeContentSize(cv: TCanvas; Node: TGeometricGraphNode; out W, H: integer);
var
  Extent: TSize;
begin
  if Node.Caption <> '' then
  begin
    Extent := cv.TextExtent(Node.Caption);
    W := Extent.cx + 10;
    H := Extent.cy + 10;
  end
  else
  begin
    W := 25;
    H := 25;
  end;
end;

procedure TGraphControl.DrawNodeContent(cv: TCanvas; Node: TGeometricGraphNode; R: TRect);
var
  X, Y: integer;
  Extent: TSize;
begin
  cv.Pen.Color := clBlack;
  cv.Brush.Color := FNodeColor;

  if Node.Selected then
  begin
    cv.Pen.Color := clBlack;
    cv.Brush.Color := FSelectedNodeColor;
  end
  else if not Node.Visible then
  begin
    cv.Pen.Color := clBlack;
    cv.Brush.Color := FInvisibleNodeColor;
  end;

  cv.Rectangle(R);

  if Node.Caption <> '' then
  begin
    Extent := cv.TextExtent(Node.Caption);
    X := R.Left + (R.Width - Extent.cx) div 2;
    Y := R.Top + (R.Height - Extent.cy) div 2;
    cv.TextOut(X, Y, Node.Caption);
  end;
end;

procedure TGraphControl.DrawEdge(Source, Target: TGeometricGraphNode; Edge: TGeometricGraphEdge);
var
  SourceCenter, TargetCenter, Pt, Pt1, Pt2: TPoint;
  i: integer;
  tmpPt1, tmpPt2: TPoint;
begin
  SourceCenter := Source.GetCenter;
  TargetCenter := Target.GetCenter;

  FBackBuffer.Brush.Color := FArrowColor;
  FBackBuffer.Pen.Color := FArrowColor;

  Pt := Source.GetCenter;
  tmpPt1 := Pt + FWorldOffset;
  FBackBuffer.MoveTo(tmpPt1.X, tmpPt1.Y);

  if Assigned(Edge) then
  begin
    if Edge.BackEdge then
    begin
      FBackBuffer.Brush.Color := FBackArrowColor;
      FBackBuffer.Pen.Color := FBackArrowColor;
    end;

    for i := 0 to High(Edge.Points) do
    begin
      Pt := Edge.Points[i];
      tmpPt1 := Pt + FWorldOffset;
      FBackBuffer.LineTo(tmpPt1.X, tmpPt1.Y);
    end;
  end;

  if not FindRectAndLineIntersectionPoint(Target.GetBoundRect, Pt, Pt2) then
    raise Exception.Create('Arrow coords not found');

  Pt1 := Pt;
  tmpPt1 := Pt1 + FWorldOffset;
  tmpPt2 := Pt2 + FWorldOffset;
  DrawArrow(tmpPt1.X, tmpPt1.Y, tmpPt2.X, tmpPt2.Y);
end;

procedure TGraphControl.DrawNodeOutEdges(Source: TGeometricGraphNode);
var
  Edge: TBasicGraphEdge;
begin
  for Edge in Source.OutEdges do
    DrawEdge(TGeometricGraphNode(Edge.Source), TGeometricGraphNode(Edge.Target),
      TGeometricGraphEdge(Edge));
end;

procedure TGraphControl.DrawNodes;
var
  Node: TBasicGraphNode;
begin
  if FCanDrawEdges then
    for Node in FGraph.Nodes.Values do
      DrawNodeOutEdges(TGeometricGraphNode(Node));

  for Node in FGraph.Nodes.Values do
    DrawNode(TGeometricGraphNode(Node));
end;

function TGraphControl.GetNodeAtPoint(const Pt: TWorldPoint): TGeometricGraphNode;
var
  n: TBasicGraphNode;
begin
  if Assigned(FGraph) then
    for n in FGraph.Nodes.Values do
      if TGeometricGraphNode(n).GetBoundRect.Contains(Pt) then
        Exit(TGeometricGraphNode(n));
  Exit(nil);
end;

procedure TGraphControl.GoToNode(Node: TGeometricGraphNode);
begin
  FWorldOffset.X := -Node.X + (Self.Width div 2);
  FWorldOffset.Y := -Node.Y + (Self.Height div 20) + (Node.Height div 2);

  Invalidate;
end;

procedure TGraphControl.Layout(Kind: integer);
begin
  if Assigned(Graph) then
  begin
    FillNodeContentSize;
    Invalidate;
  end;
end;

procedure TGraphControl.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  WorldPt: TWorldPoint;
  n: TGeometricGraphNode;
begin
  inherited;

  if not Focused then
    DoEnter;

  FMouseGrabPoint := Point(X, Y);

  if gco_SelectNode in FOptions then
  begin
    WorldPt := ScreenToWorld(FMouseGrabPoint);
    n := GetNodeAtPoint(WorldPt);
    SelectNode(n, WorldPt);
  end;
end;

procedure TGraphControl.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  WorldPt: TWorldPoint;
  n: TGeometricGraphNode;
begin
  inherited;

  WorldPt := ScreenToWorld(Point(X, Y));
  n := GetNodeAtPoint(WorldPt);

  if (ssCtrl in Shift) and Assigned(n) and Assigned(FNodeSelection.Node) and (n <> FNodeSelection.Node) then
  begin
    FProbableConnection.Node := nil;
    FGraph.EdgeAdd(FNodeSelection.Node, n);
    Invalidate;
    Exit;
  end;
end;

procedure TGraphControl.MouseMove(Shift: TShiftState; X, Y: integer);
var
  WorldPt, NewNodePos: TWorldPoint;
  ProbableConnectionOld, ProbableConnectionNew: boolean;
  MouseMoveDelta: TPoint;
var
  CanMoveH, CanMoveV: boolean;
begin
  inherited;

  ProbableConnectionOld := Assigned(FProbableConnection.Node);
  FProbableConnection.Node := nil;

  WorldPt := ScreenToWorld(Point(X, Y));

  // Moving and Left button down
  if (ssLeft in Shift) and not(ssCtrl in Shift) then
  begin
    CanMoveH := gco_MoveNodeHorizontally in FOptions;
    CanMoveV := gco_MoveNodeVertically in FOptions;

    // If some node selected and mouse under this node -> move this node.
    if Assigned(FNodeSelection.Node) and
      (FNodeSelection.Node.GetBoundRect.Contains(WorldPt)) and
      (CanMoveH or CanMoveV) then
    begin
      NewNodePos := WorldPt - FNodeSelection.GrabbedAt;

      if not CanMoveH then
        NewNodePos.X := FNodeSelection.Node.X;
      if not CanMoveV then
        NewNodePos.Y := FNodeSelection.Node.Y;

      FNodeSelection.Node.Position := NewNodePos;
      Invalidate;
    end
    else
    begin
      // If not moving node, move canvas.
      if gco_MoveCanvas in FOptions then
      begin
        MouseMoveDelta := Point(X, Y) - FMouseGrabPoint;
        FMouseGrabPoint := Point(X, Y);
        FWorldOffset := FWorldOffset + MouseMoveDelta;
        Invalidate;
      end;
    end;
    Exit;
  end;

  // Moving and Left and Ctrl down
  if (ssLeft in Shift) and (ssCtrl in Shift) then
  begin
    FProbableConnection.Node := nil;
    if Assigned(FNodeSelection.Node) then
    begin
      FProbableConnection.Node := GetNodeAtPoint(WorldPt);
      if Assigned(FProbableConnection.Node) then
      begin
        FProbableConnection.GrabbedAt := Point(0, 0);
      end;
      Invalidate;
      Exit;
    end;
  end;

  ProbableConnectionNew := Assigned(FProbableConnection.Node);

  // Check for modifications
  if (ProbableConnectionOld <> ProbableConnectionNew) then
    Invalidate;
end;

procedure TGraphControl.Resize;
begin
  inherited;
  FBmp.SetSize(ClientWidth, ClientHeight);
end;

function TGraphControl.ScreenToWorld(const Pt: TPoint): TWorldPoint;
begin
  Result := Pt - FWorldOffset;
end;

procedure TGraphControl.SelectNode(const Value: TGeometricGraphNode; const Pt: TWorldPoint);
begin
  if Value = nil then
    Exit;

  if Assigned(FNodeSelection.Node) then
    FNodeSelection.Node.Selected := False;

  FNodeSelection.Node := Value;
  if Assigned(Value) then
  begin
    Value.Selected := True;
    FNodeSelection.GrabbedAt := Pt - Value.Position;
  end;

  Invalidate;
end;

procedure TGraphControl.SetCanDrawEdges(const Value: boolean);
begin
  if Value <> FCanDrawEdges then
  begin
    FCanDrawEdges := Value;
    Invalidate;
  end;
end;

procedure TGraphControl.SetGraph(const Value: TGeometricGraph);
begin
  FNodeSelection.Node := nil;
  FNodeSelection.GrabbedAt := Point(0, 0);

  if FGraph <> Value then
  begin
    FCanAdjustNodeBounds := True;
    FOptions := DEFAULT_OPTIONS;
    FCanDrawEdges := True;

    if FGraph <> nil then
    begin
      if FOwnsGraph then
        FGraph.Free;
    end;
    FGraph := Value;
    Invalidate;
  end;
end;

procedure TGraphControl.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

end.
