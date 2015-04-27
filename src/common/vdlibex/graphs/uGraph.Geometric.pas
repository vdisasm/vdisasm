unit uGraph.Geometric;

interface

uses
  System.Classes,
  System.Generics.Collections,
  System.Math,
  System.SysUtils,
  System.Types,
  uGraph,

  ogdf;

const
  DEF_NODE_WIDTH  = 25;
  DEF_NODE_HEIGHT = 25;

type
  TGeometricGraph = class;

  TGraphLevel = class
  private
    FGraph: TGeometricGraph;
    FIndex: integer;
    FNodes: TList<TBasicGraphNode>;
    function GetNodeCount: integer;
  public
    constructor Create(Graph: TGeometricGraph; Index: integer);
    destructor Destroy; override;

    property Graph: TGeometricGraph read FGraph;
    property Index: integer read FIndex;
    property Count: integer read GetNodeCount;
    property Nodes: TList<TBasicGraphNode> read FNodes;
  end;

  TGeometricGraphNode = class(TBasicGraphNode)
  private
    FLevel: TGraphLevel;
    FGraph: TGraph;
    procedure SetLevel(const Value: TGraphLevel);
    function GetHeight: integer;
    function GetPosition: TPoint;
    function GetWidth: integer;
    procedure SetHeight(const Value: integer);
    procedure SetPosition(const Value: TPoint);
    procedure SetWidth(const Value: integer);
    function GetX: integer;
    function GetY: integer;
    procedure SetX(const Value: integer);
    procedure SetY(const Value: integer);
  protected
    OGDFNode: ogdf.POGDFNode;
  public
    Caption: string;
    Selected: boolean;
    Visible: boolean;
  public
    function GetBoundRect: TRect;
    procedure SetBoundRect(const Value: TRect);

    function GetCenter: TPoint;

    procedure UnbindLevel;
    property Level: TGraphLevel read FLevel write SetLevel;

    property X: integer read GetX write SetX;
    property Y: integer read GetY write SetY;
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    property Position: TPoint read GetPosition write SetPosition;
  end;

  TGeometricGraphEdge = class(TBasicGraphEdge)
  private
    FWeight: single;
  public
    OGDFEdge: ogdf.POGDFEdge;
    Points: TArray<TPoint>;

    function IsBackEdge: boolean;
    function IsLongLevelNode: boolean;
    property Weight: single read FWeight;
  end;

  TGraphLevels = TObjectList<TGraphLevel>;

  TLvlGraphEdgeSplitMode = (
    lgesNone,
    lgesSeparate,    // create for each edge separate hidden nodes, this creates a lot of hidden nodes
    lgesMergeSource, // combine hidden nodes at source (outgoing edge)
    lgesMergeTarget, // combine hidden nodes at target (incoming edge)
    lgesMergeHighest // combine hidden nodes at source or target, whichever has more edges
    );

  TGeometricGraph = class(TGraph)
  private
    FOGDFGraph: POGDFGraph;
    FOGDFGraphAttr: POGDFGraphAttr;
  private
    FLevels: TGraphLevels;
    procedure SetLevelCount(const Value: integer);
    function GetLevelCount: integer;
  protected
    function GetNodeClass: TBasicGraphNodeClass; override;
    function GetEdgeClass: TBasicGraphEdgeClass; override;
    procedure NodeInit(Node: TBasicGraphNode); override;
    procedure EdgeInit(Edge: TBasicGraphEdge); override;
  public
    procedure SaveToStream(const Stream: TStream);
    procedure SaveToFile(const FileName: string);

    procedure LoadFromStream(const Stream: TStream);
    procedure LoadFromFile(const FileName: string);
  protected
    // procedure ConsistencyCheck(WithBackEdge: boolean);
    // procedure CreateTopologicalLevels;
    // procedure SplitLongEdges(SplitMode: TLvlGraphEdgeSplitMode);
    // function CreateHiddenNode(Level: integer = 0): TGeometricGraphNode;
  public
    constructor Create;
    destructor Destroy; override;

    // Reverse all edges with BackEdge = True.
    procedure ReverseAllBackEdges;

    procedure Layout;

    procedure Clear; override;

    procedure OGDFLayout(Kind: TGraphLayoutKind = glk_fast_simple_hierarchy);

    property LevelCount: integer read GetLevelCount write SetLevelCount;
    property Levels: TGraphLevels read FLevels;

    property OGDFGraph: ogdf.POGDFGraph read FOGDFGraph;
  end;

implementation

{ TGeometricGraph }

procedure TGeometricGraph.SetLevelCount(const Value: integer);
begin
  if Value < 1 then
    raise Exception.Create('Invalid level count')
  else if Value = LevelCount then
    exit;
  while LevelCount < Value do
    TGraphLevel.Create(self, LevelCount);
  if LevelCount > Value then
    FLevels.Count := Value;
end;

procedure CopyEdgePoints([ref] pt: DPoint; i: integer; ud: pointer); cdecl;
begin
  TGeometricGraphEdge(ud).Points[i] := Point(Round(pt.X), Round(pt.Y));
end;

procedure TGeometricGraph.OGDFLayout(Kind: TGraphLayoutKind);
var
  e: TBasicGraphEdge;
  ge: TGeometricGraphEdge;
  bends: DPolyline;
  cnt: integer;
  LayerDist, NodeDist: Double;
begin
  LayerDist := 30;
  NodeDist := 30;
  ogdf.Layout(FOGDFGraph, FOGDFGraphAttr, Kind, LayerDist, NodeDist);

{$IFDEF DEBUG}
  // ogdf.graph_to_svg(FOGDFGraphAttr, 'graph.svg');
{$ENDIF}
  // Copy edge data.
  for e in FEdges do
  begin
    ge := TGeometricGraphEdge(e);
    bends := edge_bends(FOGDFGraphAttr, ge.OGDFEdge);
    cnt := DPolyline_cnt(bends);
    SetLength(ge.Points, cnt); // start + bends + end
    if cnt <> 0 then
      DPolyline_iterate(bends, CopyEdgePoints, ge);
  end;
end;

procedure TGeometricGraph.ReverseAllBackEdges;
var
  e: TBasicGraphEdge;
begin
  for e in FEdges do
    if e.BackEdge then
    begin
      EdgeReverse(e);
    end;
end;

procedure TGeometricGraph.SaveToFile(const FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TGeometricGraph.SaveToStream(const Stream: TStream);
var
  bw: TBinaryWriter;
var
  NodePair: TPair<TNodeId, TBasicGraphNode>;
  gn: TGeometricGraphNode;
var
  e: TBasicGraphEdge;
begin
  bw := TBinaryWriter.Create(Stream, TEncoding.UTF8);
  try
    bw.Write(Nodes.Count);
    for NodePair in Nodes do
    begin
      bw.Write(NodePair.Key);

      gn := TGeometricGraphNode(NodePair.Value);
      bw.Write(gn.Caption);
      bw.Write(gn.X);
      bw.Write(gn.Y);
      bw.Write(gn.Width);
      bw.Write(gn.Height);
    end;

    bw.Write(Edges.Count);
    for e in Edges do
    begin
      bw.Write(e.EdgeId);
      bw.Write(e.Source.NodeId);
      bw.Write(e.Target.NodeId);
    end;
  finally
    bw.Free;
  end;
end;

// procedure TGeometricGraph.SplitLongEdges(SplitMode: TLvlGraphEdgeSplitMode);
// type
// TNodeInfo = record
// HiddenNodes: TArray<TBasicGraphNode>;
// LongInEdges, LongOutEdges: integer;
// end;
//
// PNodeInfo = ^TNodeInfo;
//
// TNodeToInfo = TDictionary<TBasicGraphNode, PNodeInfo>;
// var
// NodeToInfo: TNodeToInfo; // node to TNodeInfo
// BasicSourceNode: TBasicGraphNode;
// SourceNode, TargetNode, LastNode, NextNode: TGeometricGraphNode;
// l: integer;
// Edge: TBasicGraphEdge;
// EdgeWeight: single;
// EdgeData: pointer;
// HiddenNodes: TArray<TBasicGraphNode>;
// // AVLNode: TAvgLvlTreeNode;
// // P2PItem: PPointerToPointerItem;
// MergeAtSourceNode: boolean;
// SourceInfo, TargetInfo: PNodeInfo;
// begin
// if SplitMode = lgesNone then
// exit;
//
// NodeToInfo := TNodeToInfo.Create;
// try
// // create node infos
// for BasicSourceNode in FNodes.Values do
// begin
// SourceNode := TGeometricGraphNode(BasicSourceNode);
//
// New(SourceInfo);
// FillChar(SourceInfo^, SizeOf(TNodeInfo), 0);
// SetLength(SourceInfo^.HiddenNodes, LevelCount);
// for Edge in SourceNode.OutEdges do
// begin
// if TGeometricGraphEdge(Edge).IsLongLevelNode then
// inc(SourceInfo^.LongOutEdges, 1);
// end;
// for Edge in SourceNode.InEdges do
// begin
// if TGeometricGraphEdge(Edge).IsLongLevelNode then
// inc(SourceInfo^.LongInEdges, 1);
// end;
// // NodeToInfo[SourceNode] := SourceInfo;
// NodeToInfo.Add(SourceNode, SourceInfo);
// end;
//
// // split long edges
// for BasicSourceNode in FNodes.Values do
// begin
// SourceNode := TGeometricGraphNode(BasicSourceNode);
//
// for Edge in SourceNode.OutEdges.ReverseEnumerator do
// begin // Note: run downwards, because edges will be deleted
// TargetNode := TGeometricGraphNode(Edge.Target);
// if not TGeometricGraphEdge(Edge).IsLongLevelNode then
// continue;
//
// // Edge is long.
//
// EdgeWeight := TGeometricGraphEdge(Edge).Weight;
// // >> EdgeData := Edge.Data;
// // remove long edge
// // Edge.Free; replaced with EdgeDelete(Edge)
// EdgeDelete(Edge);
//
// // create merged hidden nodes
// if SplitMode in [lgesMergeSource, lgesMergeTarget, lgesMergeHighest] then
// begin
// SourceInfo := PNodeInfo(NodeToInfo[SourceNode]);
// TargetInfo := PNodeInfo(NodeToInfo[TargetNode]);
// MergeAtSourceNode := True;
// case SplitMode of
// lgesMergeTarget:
// MergeAtSourceNode := false;
// lgesMergeHighest:
// MergeAtSourceNode := SourceInfo^.LongOutEdges >= TargetInfo^.LongInEdges;
// end;
// // debugln(['TLvlGraph.SplitLongEdges ',SourceNode.Caption,'=',SourceInfo^.LongOutEdges,' ',TargetNode.Caption,'=',TargetInfo^.LongInEdges,' MergeAtSourceNode=',MergeAtSourceNode]);
// if MergeAtSourceNode then
// HiddenNodes := SourceInfo^.HiddenNodes
// else
// HiddenNodes := TargetInfo^.HiddenNodes;
// // create hidden nodes
// for l := SourceNode.Level.Index + 1 to TargetNode.Level.Index - 1 do
// if HiddenNodes[l] = nil then
// HiddenNodes[l] := CreateHiddenNode(l);
// end;
// // create edges
// LastNode := SourceNode;
// for l := SourceNode.Level.Index + 1 to TargetNode.Level.Index do
// begin
// if l < TargetNode.Level.Index then
// begin
// if SplitMode = lgesSeparate then
// NextNode := CreateHiddenNode(l)
// else
// NextNode := TGeometricGraphNode(HiddenNodes[l]);
// end
// else
// NextNode := TargetNode;
//
// GetEdge(LastNode, NextNode, True);
//
// LastNode := NextNode;
// end;
// end;
// end;
// finally
// for SourceInfo in NodeToInfo.Values do
// Dispose(SourceInfo);
// NodeToInfo.Free;
// end;
// end;

procedure TGeometricGraph.LoadFromStream(const Stream: TStream);
var
  br: TBinaryReader;
  i, cnt: integer;
  n: TGeometricGraphNode;
  ns, nt: TBasicGraphNode;
  s, t: integer;
begin
  br := TBinaryReader.Create(Stream, TEncoding.UTF8);
  try
    // nodes
    cnt := br.ReadInteger;
    for i := 0 to cnt - 1 do
    begin
      n := TGeometricGraphNode(NodeAdd);
      n.NodeId := br.ReadInteger;
      n.Caption := br.ReadString;
      n.X := br.ReadInteger;
      n.Y := br.ReadInteger;
      n.Width := br.ReadInteger;
      n.Height := br.ReadInteger;
      FNodes.Add(n.NodeId, n);
      if n.NodeId >= FNodeLastId then
        FNodeLastId := n.NodeId + 1;
    end;

    cnt := br.ReadInteger;
    for i := 0 to cnt - 1 do
    begin
      br.ReadInteger; // id
      s := br.ReadInteger;
      t := br.ReadInteger;

      if FNodes.TryGetValue(s, ns) and FNodes.TryGetValue(t, nt) then
        EdgeAdd(ns, nt)
      else
        raise Exception.Create('Failed to add edge');
    end;
  finally
    br.Free;
  end;
end;

procedure TGeometricGraph.Layout;
begin
  // CreateTopologicalLevels;
  // ReverseAllBackEdges;
  // SplitLongEdges(lgesSeparate);

  // MinimizeCrossings;
  // AutoLayoutLevels;
  // MinimizeOverlappings
end;

procedure TGeometricGraph.LoadFromFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TGeometricGraph.GetLevelCount: integer;
begin
  Result := FLevels.Count;
end;

function TGeometricGraph.GetNodeClass: TBasicGraphNodeClass;
begin
  Result := TGeometricGraphNode;
end;

function TGeometricGraph.GetEdgeClass: TBasicGraphEdgeClass;
begin
  Result := TGeometricGraphEdge;
end;

procedure TGeometricGraph.NodeInit(Node: TBasicGraphNode);
var
  n: TGeometricGraphNode;
begin
  inherited;

  n := TGeometricGraphNode(Node);

  // Create node and get pointers to coords.
  n.OGDFNode := ogdf.node_new(FOGDFGraph, FOGDFGraphAttr);

  // ogdf.node_coords(FOGDFGraph, FOGDFGraphAttr, n, n.FX, n.FY, n.FW, n.FH);

  n.FGraph := self;
  n.Width := DEF_NODE_WIDTH;
  n.Height := DEF_NODE_HEIGHT;
  n.Visible := True;
end;

procedure TGeometricGraph.EdgeInit(Edge: TBasicGraphEdge);
var
  e: TGeometricGraphEdge;
begin
  inherited;

  e := TGeometricGraphEdge(Edge);

{$IFDEF DEBUG}
  if (e.Source = nil) or (e.Target = nil) then
    raise Exception.Create('Nodes must be not nil');
{$ENDIF}
  e.OGDFEdge := ogdf.edge_new(FOGDFGraph,
    TGeometricGraphNode(e.Source).OGDFNode,
    TGeometricGraphNode(e.Target).OGDFNode);
end;

{ TGeometricGraphNode }

function TGeometricGraphNode.GetBoundRect: TRect;
var
  w, h: integer;
begin
  w := Width;
  h := Height;
  Result := Rect(0, 0, w, h);
  Result.Offset(X - (w div 2), Y - (h div 2));
end;

function TGeometricGraphNode.GetCenter: TPoint;
begin
  Result := Point(X, Y);
end;

procedure TGeometricGraphNode.SetBoundRect(const Value: TRect);
begin
  Width := Value.Width;
  Height := Value.Height;
  X := Value.Left + (Value.Width div 2);
  Y := Value.Top + (Value.Height div 2);
end;

function TGeometricGraphNode.GetPosition: TPoint;
begin
  Result := Point(X, Y);
end;

procedure TGeometricGraphNode.SetPosition(const Value: TPoint);
begin
  X := Value.X;
  Y := Value.Y;
end;

function TGeometricGraphNode.GetX: integer;
var
  d: Double;
begin
  ogdf.node_get_x(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
  Result := Round(d);
end;

function TGeometricGraphNode.GetY: integer;
var
  d: Double;
begin
  ogdf.node_get_y(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
  Result := Round(d);
end;

function TGeometricGraphNode.GetWidth: integer;
var
  d: Double;
begin
  ogdf.node_get_w(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
  Result := Round(d);
end;

function TGeometricGraphNode.GetHeight: integer;
var
  d: Double;
begin
  ogdf.node_get_h(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
  Result := Round(d);
end;

procedure TGeometricGraphNode.SetX(const Value: integer);
var
  d: Double;
begin
  d := Value;
  ogdf.node_set_x(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
end;

procedure TGeometricGraphNode.SetY(const Value: integer);
var
  d: Double;
begin
  d := Value;
  ogdf.node_set_y(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
end;

procedure TGeometricGraphNode.SetWidth(const Value: integer);
var
  d: Double;
begin
  d := Value;
  ogdf.node_set_w(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
end;

procedure TGeometricGraphNode.SetHeight(const Value: integer);
var
  d: Double;
begin
  d := Value;
  ogdf.node_set_h(TGeometricGraph(FGraph).FOGDFGraphAttr, OGDFNode, d);
end;

procedure TGeometricGraphNode.SetLevel(const Value: TGraphLevel);
begin
  if Value = nil then
    raise Exception.Create('node needs a level');
  if Value.Graph <> self.FGraph then
    raise Exception.Create('wrong graph');
  if FLevel = Value then
    exit;
  if FLevel <> nil then
    UnbindLevel;
  FLevel := Value;
  FLevel.FNodes.Add(self);
end;

procedure TGeometricGraphNode.UnbindLevel;
begin
  if FLevel <> nil then
    FLevel.FNodes.Remove(self);
end;

{ TGeometricGraphEdge }

function TGeometricGraphEdge.IsBackEdge: boolean;
begin
  Result := TGeometricGraphNode(Source).Level.Index >= TGeometricGraphNode(Target).Level.Index;
end;

function TGeometricGraphEdge.IsLongLevelNode: boolean;
begin
  Result := (TGeometricGraphNode(Target).Level.Index - TGeometricGraphNode(Source).Level.Index) > 1;
end;

type
  TGraphLevelerNode = class
  public
    Node: TBasicGraphNode;
    Level: integer;
    Visited: boolean;
    InPath: boolean; // = node on stack
  end;

  TExtNodes = TObjectDictionary<TBasicGraphNode, TGraphLevelerNode>;

function TGraphLevel_IndexOf(self: TGraphLevel; Node: TBasicGraphNode): integer;
var
  i: integer;
begin
  for i := 0 to self.Nodes.Count - 1 do
    if self.Nodes[i] = Node then
      exit(i);
  exit(-1);
end;

procedure TGeometricGraph.Clear;
begin
  inherited;
  FLevels.Clear;
end;

// procedure TGeometricGraph.ConsistencyCheck(WithBackEdge: boolean);
// var
// i: integer;
// BasicNode: TBasicGraphNode;
// Node: TGeometricGraphNode;
// j: integer;
// BasicEdge: TBasicGraphEdge;
// Edge: TGeometricGraphEdge;
// Level: TGraphLevel;
// begin
// for i := 0 to LevelCount - 1 do
// begin
// Level := Levels[i];
// if Level.Index <> i then
// raise Exception.Create('');
// for j := 0 to Level.Count - 1 do
// begin
// Node := TGeometricGraphNode(Level.Nodes[j]);
// if Node.Level <> Level then
// raise Exception.Create('');
// if TGraphLevel_IndexOf(Level, Node) < j then
// raise Exception.Create('');
// end;
// end;
//
// for BasicNode in FNodes.Values do
// begin
// Node := TGeometricGraphNode(BasicNode);
// for BasicEdge in Node.OutEdges do
// begin
// Edge := TGeometricGraphEdge(BasicEdge);
// if Edge.Source <> Node then
// raise Exception.Create('');
// if not Edge.Target.InEdges.ContainsKey(Edge) then
// raise Exception.Create('');
// if WithBackEdge and (Edge.BackEdge <> Edge.IsBackEdge) then
// begin
// raise Exception.Create('');
// // raise Exception.Create('Edge.BackEdge ' + Edge.AsString + ' Edge.BackEdge=' + dbgs(Edge.BackEdge) + ' Edge.IsBackEdge=' + dbgs(Edge.IsBackEdge) + ' Source.Index=' +
// // dbgs(Edge.Source.Level.Index) + ' Target.Index=' + dbgs(Edge.Target.Level.Index));
// end;
// end;
//
// for BasicEdge in Node.InEdges do
// begin
// Edge := TGeometricGraphEdge(BasicEdge);
// if Edge.Target <> Node then
// raise Exception.Create('');
// if not Edge.Source.OutEdges.ContainsKey(Edge) then
// raise Exception.Create('');
// end;
// if Assigned(Node.Level) then
// if Node.Level.Nodes.IndexOf(Node) < 0 then
// raise Exception.Create('');
// end;
// end;

constructor TGeometricGraph.Create;
begin
  inherited Create;
  FLevels := TGraphLevels.Create(True);

  FOGDFGraph := new_g;
  if FOGDFGraph = nil then
    raise Exception.Create('Graph is nil');

  FOGDFGraphAttr := new_ga(FOGDFGraph);
  if FOGDFGraphAttr = nil then
    raise Exception.Create('Graph attr is nil');
end;

destructor TGeometricGraph.Destroy;
begin
  ogdf.graph_free(FOGDFGraph, FOGDFGraphAttr);
  FOGDFGraph := nil;
  FOGDFGraphAttr := nil;

  FLevels.Free;
  inherited;
end;

// function TGeometricGraph.CreateHiddenNode(Level: integer): TGeometricGraphNode;
// begin
// Result := TGeometricGraphNode(NodeAdd);
// Result.Level := FLevels[Level];
// Result.Visible := false;
// end;

// procedure TraverseCreateTopologicalLevels(
// const ExtNodes: TExtNodes;
// var MaxLevel: integer;
// ExtNode: TGraphLevelerNode);
// var
// Node: TBasicGraphNode;
// Edge: TBasicGraphEdge;
// ExtNextNode: TGraphLevelerNode;
// begin
// if ExtNode.Visited then
// exit;
// ExtNode.InPath := True;
// ExtNode.Visited := True;
// Node := ExtNode.Node;
// for Edge in Node.OutEdges do
// begin
// ExtNextNode := ExtNodes[Edge.Target];
// if ExtNextNode.InPath then
// Edge.BackEdge := True // edge is part of the cycle
// else
// begin
// TraverseCreateTopologicalLevels(ExtNodes, MaxLevel, ExtNextNode);
// ExtNode.Level := Max(ExtNode.Level, ExtNextNode.Level + 1);
// end;
// end;
// MaxLevel := Max(MaxLevel, ExtNode.Level);
// ExtNode.InPath := false; // backtrack
// end;

// procedure TGeometricGraph.CreateTopologicalLevels;
// var
// ExtNodes: TExtNodes;
// MaxLevel: integer;
// var
// Node: TBasicGraphNode;
// Edge: TBasicGraphEdge;
// ExtNode: TGraphLevelerNode;
// begin
// {$IFDEF DEBUG}
// ConsistencyCheck(false);
// {$ENDIF}
// UnvisitAllNodes;
//
// ExtNodes := TExtNodes.Create([doOwnsValues]);
// try
// // Init ExtNodes.
// // Clear BackEdge flags.
// for Node in FNodes.Values do
// begin
// ExtNode := TGraphLevelerNode.Create;
// ExtNode.Node := Node;
// ExtNodes.Add(Node, ExtNode);
// for Edge in Node.OutEdges do
// Edge.BackEdge := false;
// end;
// // Traverse all nodes.
// MaxLevel := 0;
// for Node in FNodes.Values do
// TraverseCreateTopologicalLevels(ExtNodes, MaxLevel, ExtNodes[Node]);
// // Set levels.
// LevelCount := Max(LevelCount, MaxLevel + 1);
// for Node in FNodes.Values do
// begin
// ExtNode := ExtNodes[Node];
// TGeometricGraphNode(Node).Level := Levels[MaxLevel - ExtNode.Level];
// end;
// // Delete unneeded levels.
// LevelCount := MaxLevel + 1;
// finally
// ExtNodes.Free;
// end;
// {$IFDEF DEBUG}
// ConsistencyCheck(True);
// {$ENDIF}
// end;

{ TGraphLevel }

constructor TGraphLevel.Create(Graph: TGeometricGraph; Index: integer);
begin
  inherited Create;
  self.FGraph := Graph;
  self.FGraph.FLevels.Add(self);
  self.FIndex := Index;
  self.FNodes := TList<TBasicGraphNode>.Create;
end;

destructor TGraphLevel.Destroy;
begin
  FNodes.Free;
  inherited;
end;

function TGraphLevel.GetNodeCount: integer;
begin
  Result := FNodes.Count;
end;

end.
