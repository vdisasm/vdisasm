{
  https://www.cs.rochester.edu/u/nelson/courses/csc_173/graphs/search.html
  http://www.cs.bu.edu/fac/snyder/cs112/CourseMaterials/GraphAlgorithms.html
}
unit ugraph;

interface

uses
  System.Math,
{$IFDEF DEBUG}
  System.SysUtils, // PrintEdge
{$ENDIF}
  System.Generics.Collections,
  gmap,
  gset;

type
  TBasicGraphNode = class;
  TBasicGraphEdge = class;

  TNodeKind = (
    nk_in,
    nk_out,
    nk_self
    );
  TNodeKindSet = set of TNodeKind;

  TNodeVisitKind = (
    nvk_null,
    nvk_discovered,
    nvk_back_edge
    );

  TVisitedStatus = (NotVisited, Visited, InStack);

  TNodeEdgeInfo = record
    Kind: TNodeKind;
    Node: TBasicGraphNode;
  end;

  TNodeId = integer;

  TNodeList = TList<TBasicGraphNode>;
  TNodeSet = TSet<TBasicGraphNode>;
  TNodeEnumerator = TEnumerator<TBasicGraphNode>;
  TNodeMap = TMap<TNodeId, TBasicGraphNode>; // id-node

  TEdgeSet = TSet<TBasicGraphEdge>;
  TEdgeEnumerator = TEnumerator<TBasicGraphEdge>;
  TEdgeStack = TStack<TBasicGraphEdge>;

  TPrintNodeFunc = reference to function(n: TBasicGraphNode): string;

  TNodeDataObject = TObject;
  TEdgeDataObject = TObject;

  TBasicGraphNode = class
  private
    FStatus: TVisitedStatus;
  public
    NodeId: TNodeId;
    InEdges: TEdgeSet;
    OutEdges: TEdgeSet;
  end;

  TBasicGraphEdge = class
  public
    EdgeId: integer;
    Source: TBasicGraphNode;
    Target: TBasicGraphNode;
    BackEdge: boolean;
    Reversed: boolean;
  end;

  TBasicGraphNodeClass = class of TBasicGraphNode;
  TBasicGraphEdgeClass = class of TBasicGraphEdge;

  TGraph = class
  private
    // class function CreateDefaultNodeSet: TNodeSet;
    class function CreateDefaultEdgeSet: TEdgeSet;
  protected
    FNodes: TNodeMap;
    FEdges: TEdgeSet;
    FEdgeLastId: integer;
    FNodeLastId: integer;
    procedure UnvisitAllNodes;
  private
    FPrintNodeFunc: TPrintNodeFunc;
    function VisitedStatusToString(Value: TVisitedStatus): string;
    function PrintNode(n: TBasicGraphNode; PrintVisitedState: boolean): string;
{$IFDEF DEBUG}
    function PrintEdge(e: TBasicGraphEdge; PrintVisitedState: boolean): string;
{$ENDIF}
  private type
    TFeedbackSetItem = record
      Node: TBasicGraphNode;
      OutEdges: TEdgeEnumerator;
    end;
  protected
    // procedure DoNodeNotification(Node: TBasicGraphNode; Action: TCollectionNotification); virtual;

    // Must override these 2 methods to create your sub-classes.
    function GetNodeClass: TBasicGraphNodeClass; virtual; abstract;
    function GetEdgeClass: TBasicGraphEdgeClass; virtual; abstract;

    // Optionally you can init classes after creation.
    procedure NodeInit(Node: TBasicGraphNode); virtual;
    procedure EdgeInit(Edge: TBasicGraphEdge); virtual;

    procedure NodeFree(Node: TBasicGraphNode);
  public
    constructor Create;
    destructor Destroy; override;

    function NodeAdd: TBasicGraphNode;

    // Self-edges allowed.
    function EdgeAdd(Src, Dst: TBasicGraphNode): TBasicGraphEdge;
    procedure EdgeReverse(Edge: TBasicGraphEdge);
    function GetEdge(Source, Target: TBasicGraphNode; CreateIfNotExists: boolean): TBasicGraphEdge;
    procedure EdgeDelete(Edge: TBasicGraphEdge);

    // Depth-first.
    // procedure DepthFirstSearchNode(v: TBasicGraphNode; VisitFunc: TNodeVisitFunc; UnvisitNodesFirst: boolean = True);
    // procedure DepthFirstSearchGraph(VisitFunc: TNodeVisitFunc);

    // Bredth-first.
    // procedure BreadthFirstSearchNode(v: TBasicGraphNode; VisitFunc: TNodeVisitFunc; UnvisitNodesFirst: boolean = True);

    // Get all nodes with 2 or more edges (either in or out).
    // Node can be Start or Termination node.
    function GetForkNodes: TNodeList;
    experimental;

    // Get all nodes which have no in edges.
    // There can be (back) input nodes from inside of the graph.
    function GetStartNodes: TNodeList;

    // Get all nodes which have no out edges.
    // Sink nodes.
    function GetSinkNodes: TNodeList;

    // function GetFeedbackSetExperimental: TEdgeSet;

    // Return edges that when reversed will make graph acyclic.
    // function GetFeedbackSet: TEdgeSet;

    procedure Clear; virtual;

    property Nodes: TNodeMap read FNodes;
    property Edges: TEdgeSet read FEdges;

    // property LevelLow: integer read FLevelLow;
    // property LevelHigh: integer read FLevelHigh;
    // property GraphLevelCount: integer read NodeLevelsGetCount;

    property PrintNodeFunc: TPrintNodeFunc read FPrintNodeFunc write FPrintNodeFunc;
  end;

implementation

function NodeIdComparerLess(const a, b: TNodeId): boolean;
begin
  result := a < b;
end;

function NodeComparerLess(const a, b: TBasicGraphNode): boolean;
begin
  result := a.NodeId < b.NodeId;
end;

function EdgeComparerLess(const a, b: TBasicGraphEdge): boolean;
begin
  result := a.EdgeId < b.EdgeId;
end;

procedure TGraph.NodeFree(Node: TBasicGraphNode);
begin
  Node.InEdges.Free;
  Node.OutEdges.Free;
  Node.Free;
end;

procedure TGraph.NodeInit(Node: TBasicGraphNode);
begin
end;

procedure TGraph.EdgeInit(Edge: TBasicGraphEdge);
begin
end;

procedure TGraph.EdgeReverse(Edge: TBasicGraphEdge);
var
  Src, Tgt, Tmp: TBasicGraphNode;
begin
  Src := Edge.Source;
  Tgt := Edge.Target;

  // Disconnect edge from source and target.
  Src.OutEdges.Remove(Edge);
  Tgt.InEdges.Remove(Edge);

  // Connect reversed edge.
  Src.InEdges.Insert(Edge);
  Tgt.OutEdges.Insert(Edge);

  // Swap nodes in edge and toggle Reversed state.
  Tmp := Edge.Source;
  Edge.Source := Edge.Target;
  Edge.Target := Tmp;
  Edge.Reversed := not Edge.Reversed;

{$IFDEF DEBUG}
  if IsConsole then
    writeln('Edge reversed to: ', PrintEdge(Edge, False));
{$ENDIF}
end;

class function TGraph.CreateDefaultEdgeSet: TEdgeSet;
begin
  result := TEdgeSet.Create(EdgeComparerLess);
end;

{$IFDEF DEBUG}
function TGraph.PrintEdge(e: TBasicGraphEdge; PrintVisitedState: boolean): string;
begin
  result := format('%s->%s', [PrintNode(e.Source, PrintVisitedState), PrintNode(e.Target, PrintVisitedState)]);
end;
{$ENDIF}

function TGraph.PrintNode(n: TBasicGraphNode; PrintVisitedState: boolean): string;
begin
  if Assigned(FPrintNodeFunc) then
    result := '"' + FPrintNodeFunc(n) + '"'
  else
    result := '?';
  if PrintVisitedState then
    result := result + ' ' + VisitedStatusToString(n.FStatus);
end;

procedure TGraph.UnvisitAllNodes;
var
  n: TBasicGraphNode;
begin
  for n in FNodes.Values do
  begin
    n.FStatus := TVisitedStatus.NotVisited;
  end;
end;

function TGraph.VisitedStatusToString(
  Value: TVisitedStatus): string;
begin
  case Value of
    TVisitedStatus.NotVisited:
      result := 'not visited';
    TVisitedStatus.Visited:
      result := 'visited';
    TVisitedStatus.InStack:
      result := 'in stack';
  else
    result := 'unknown';
  end;
end;

constructor TGraph.Create;
begin
  inherited;
  FNodes := TNodeMap.Create(NodeIdComparerLess);
  FEdges := CreateDefaultEdgeSet;
end;

destructor TGraph.Destroy;
begin
  Clear;
  FEdges.Free;
  FNodes.Free;
  inherited;
end;

procedure TGraph.Clear;
var
  Node: TBasicGraphNode;
  Edge: TBasicGraphEdge;
begin
  FNodeLastId := 0;
  FEdgeLastId := 0;

  for Edge in FEdges do
    Edge.Free;
  FEdges.Clear;

  for Node in FNodes.Values do
    NodeFree(Node);
  FNodes.Clear;
end;

// procedure TGraph.DepthFirstSearchGraph(VisitFunc: TNodeVisitFunc);
// var
// n: TBasicGraphNode;
// begin
// UnvisitAllNodes;
// for n in FNodes do
// if n.FStatus <> TVisitedStatus.Visited then
// DepthFirstSearchNode(n, VisitFunc, False);
// end;

// procedure TGraph.DepthFirstSearchNode(v: TBasicGraphNode; VisitFunc: TNodeVisitFunc; UnvisitNodesFirst: boolean);
// var
// s: TStack<TBasicGraphNode>;
// e: TNodeEdgeInfo;
// begin
// if UnvisitNodesFirst then
// UnvisitAllNodes;
// s := TStack<TBasicGraphNode>.Create;
// try
// s.Push(v);
// while s.Count <> 0 do
// begin
// v := s.Pop;
// case v.FStatus of
// TVisitedStatus.Visited:
// begin
// VisitFunc(v, nvk_back_edge);
// end;
// TVisitedStatus.NotVisited:
// begin
// VisitFunc(v, nvk_discovered);
// v.FStatus := TVisitedStatus.Visited;
// for e in v.GetAdjacentEdgeInfo([nk_out]) do
// begin
// s.Push(e.Node);
// end;
// end;
// end;
// end;
// finally
// s.Free;
// end;
// end;

// procedure TGraph.BreadthFirstSearchNode(v: TBasicGraphNode; VisitFunc: TNodeVisitFunc; UnvisitNodesFirst: boolean);
// var
// q: TQueue<TBasicGraphNode>;
// t, u: TBasicGraphNode;
// e: TNodeEdgeInfo;
// begin
// if UnvisitNodesFirst then
// UnvisitAllNodes;
// q := TQueue<TBasicGraphNode>.Create;
// try
// VisitFunc(v, nvk_discovered);
// v.FStatus := TVisitedStatus.Visited;
//
// q.Enqueue(v);
//
// while q.Count <> 0 do
// begin
// t := q.Dequeue;
// for e in t.GetAdjacentEdgeInfo([nk_out]) do
// begin
// u := e.Node;
// if u.FStatus <> TVisitedStatus.Visited then
// begin
// VisitFunc(u, nvk_discovered);
// u.FStatus := TVisitedStatus.Visited;
//
// q.Enqueue(u);
// end;
// end;
// end;
// finally
// q.Free;
// end;
// end;

// function TGraph.GetFeedbackSet: TEdgeSet;
// type
// TSrcTgtPair = TPair<TBasicGraphNode, TBasicGraphNode>;
// var
// EdgeStack: TEdgeStack;
// Edge, OutEdge: TBasicGraphEdge;
// GlobalNode, CurrentNode: TBasicGraphNode;
// begin
// result := CreateDefaultEdgeSet;
//
// // Do depth-first scan in all components.
// EdgeStack := TEdgeStack.Create;
// try
// UnvisitAllNodes;
// for GlobalNode in FNodes.Values do
// begin
// if GlobalNode.FStatus <> TVisitedStatus.Visited then
// begin
// repeat
// if EdgeStack.Count = 0 then
// begin
// Edge := nil;
// CurrentNode := GlobalNode
// end
// else
// begin
// Edge := EdgeStack.Pop;
// CurrentNode := Edge.Target;
// end;
//
// {$IFDEF DEBUG}
// if IsConsole then
// begin
// if Edge <> nil then
// writeln(PrintEdge(Edge, True));
// end;
// {$ENDIF}
// case CurrentNode.FStatus of
// TVisitedStatus.NotVisited:
// begin
// CurrentNode.FStatus := TVisitedStatus.Visited;
// for Edge in CurrentNode.OutEdges do
// EdgeStack.Push(Edge);
// end;
// TVisitedStatus.Visited:
// begin
// result.Insert(Edge);
// {$IFDEF DEBUG}
// if IsConsole then
// writeln('  back edge: ' + PrintEdge(Edge, False));
// {$ENDIF}
// end;
// end;
//
// until EdgeStack.Count = 0;
// end;
// end;
// finally
// EdgeStack.Free;
// end;
// end;

// function TGraph.GetFeedbackSetExperimental: TEdgeSet;
// var
// s: TStack<TFeedbackSetItem>;
// Current: TBasicGraphEdge;
// n: TBasicGraphNode;
// item: TFeedbackSetItem;
// NodeSet: TNodeSet;
// begin
// result := CreateDefaultEdgeSet;
// s := TStack<TFeedbackSetItem>.Create;
//
// try
// UnvisitAllNodes;
//
// for n in FNodes.Values do
// begin
// if n.FStatus <> TVisitedStatus.Visited then
// begin
// // push
// item.Node := n;
// item.Node.FStatus := TVisitedStatus.InStack;
// item.OutEdges := n.OutEdges.GetEnumerator;
// s.Push(item);
//
// while s.Count <> 0 do
// begin
// // pop
// item := s.Pop;
// item.Node.FStatus := TVisitedStatus.Visited;
//
// while item.OutEdges.MoveNext do
// begin
// Current := item.OutEdges.Current;
// if Current.Source <> Current.Target then
// begin
// case Current.Target.FStatus of
// TVisitedStatus.InStack:
// result.Insert(Current);
// TVisitedStatus.NotVisited:
// begin
// // push
// item.Node.FStatus := TVisitedStatus.InStack;
// s.Push(item);
//
// // switch
// item.Node := Current.Target;
// item.Node.FStatus := TVisitedStatus.Visited;
// item.OutEdges := item.Node.OutEdges.GetEnumerator;
// end;
// end;
// end;
// end;
// FreeAndNil(item.OutEdges); // free enumerator
//
// end;
// end;
// end;
// finally
// s.Free;
// end;
// end;

function TGraph.GetForkNodes: TNodeList;
var
  n: TBasicGraphNode;
begin
  result := TNodeList.Create;
  for n in FNodes.Values do
    if (n.InEdges.Count >= 2) or (n.OutEdges.Count >= 2) then
      result.Add(n);
end;

function TGraph.GetStartNodes: TNodeList;
var
  n: TBasicGraphNode;
begin
  result := TNodeList.Create;
  for n in FNodes.Values do
    if n.InEdges.Count = 0 then
      result.Add(n);
end;

function TGraph.GetSinkNodes: TNodeList;
var
  n: TBasicGraphNode;
begin
  result := TNodeList.Create;
  for n in FNodes.Values do
    if n.OutEdges.Count = 0 then
      result.Add(n);
end;

function TGraph.EdgeAdd(Src, Dst: TBasicGraphNode): TBasicGraphEdge;
begin
  if (Src = nil) or (Dst = nil) then
    exit(nil);

  result := GetEdgeClass.Create;

  result.Source := Src;
  result.Target := Dst;

  inc(FEdgeLastId);
  result.EdgeId := FEdgeLastId;

  Src.OutEdges.Insert(result);
  Dst.InEdges.Insert(result);

  FEdges.Insert(result);

  EdgeInit(result);
end;

procedure TGraph.EdgeDelete(Edge: TBasicGraphEdge);
begin
  Edge.Source.OutEdges.Remove(Edge);
  Edge.Target.InEdges.Remove(Edge);
  FEdges.Remove(Edge);
  Edge.Free;
end;

function TGraph.GetEdge(Source, Target: TBasicGraphNode; CreateIfNotExists: boolean): TBasicGraphEdge;
var
  e: TBasicGraphEdge;
begin
  // todo: it's a very slow scan; need to optimize it
  for e in FEdges do
    if (e.Source = Source) and (e.Target = Target) then
      exit(e); // edge found

  // Edge not found
  if CreateIfNotExists then
    result := EdgeAdd(Source, Target)
  else
    result := nil;
end;

function TGraph.NodeAdd: TBasicGraphNode;
begin
  result := GetNodeClass.Create;
  result.InEdges := TEdgeSet.Create(EdgeComparerLess);
  result.OutEdges := TEdgeSet.Create(EdgeComparerLess);
  result.NodeId := FNodeLastId;
  FNodes.Add(FNodeLastId, result);
  inc(FNodeLastId);

  NodeInit(result);
end;

end.
