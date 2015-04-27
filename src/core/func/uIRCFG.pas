unit uIrCfg;

interface

uses
  System.Generics.Collections,
  System.SysUtils,

  VDAPI,

  uIrBasicBlock,
  uGraph;

type
  TIrBlockList = TList<IIRBasicBlock>;

  TIrCFG = class(TInterfacedObject, IIRCfg)
  private
    FEntryBlock: IIRBasicBlockHandle;
    FGraph: TIRBasicBlockGraph;
  public
    constructor Create;
    destructor Destroy; override;
  public
    // Add new empty block with start address and size.
    function BlockAdd(VA: TVA; Size: integer): TIRBasicBlockNode; stdcall;
    // Add edge.
    function EdgeAdd(Source, Target: TIRBasicBlockNode): TIRBasicBlockEdge; stdcall;
  public
    { VDAPI }

    function GetBlock(Handle: IIRBasicBlockHandle): IIRBasicBlock; stdcall;

    function BlockNew(VA: TVA; Size: int): IIRBasicBlockHandle; stdcall;
    procedure BlockConnect(Source, Target: IIRBasicBlockHandle); stdcall;
    function BlockGetEntry: IIRBasicBlockHandle; stdcall;

    // Block iteration.
    function BlockGetFirst: PIterator; stdcall;
    function BlockMoveNext(it: PIterator): BOOL; stdcall;
    function BlockGetCurrent(it: PIterator): IIRBasicBlock; stdcall;

    // Edge iteration.
    function EdgeGetFirst: PIterator; stdcall;
    function EdgeMoveNext(it: PIterator): BOOL; stdcall;
    function EdgeGetCurrent(it: PIterator): IIRBasicBlockEdge; stdcall;

    // Free iterator (block/edge).
    procedure FreeIterator(it: PIterator); stdcall;
  end;

implementation

uses
  uCore.Strings;

{ TIRCFG }

constructor TIrCFG.Create;
begin
  inherited;
  FGraph := TIRBasicBlockGraph.Create;
end;

destructor TIrCFG.Destroy;
begin
  FGraph.Free;
  inherited;
end;

function TIrCFG.BlockAdd(VA: TVA; Size: integer): TIRBasicBlockNode; stdcall;
begin
  result := TIRBasicBlockNode(FGraph.NodeAdd);
  result.block := TIRBasicBlock.Create;
  result.block.VA := VA;
  result.block.Size := Size;
end;

function TIrCFG.EdgeAdd(Source, Target: TIRBasicBlockNode): TIRBasicBlockEdge;
begin
  result := TIRBasicBlockEdge(FGraph.EdgeAdd(TBasicGraphNode(Source), TBasicGraphNode(Target)));
  result.Edge := TIRBasicEdge.Create;
  result.Edge.Source := Source.block;
  result.Edge.Target := Target.block;
end;

function TIrCFG.GetBlock(Handle: IIRBasicBlockHandle): IIRBasicBlock;
begin
  if Assigned(Handle) then
    result := TIRBasicBlockNode(Handle).block
  else
    result := nil;
end;

function TIrCFG.BlockNew(VA: TVA; Size: int): IIRBasicBlockHandle;
var
  Node: TIRBasicBlockNode;
begin
  Node := BlockAdd(VA, Size);
  result := Node;
end;

procedure TIrCFG.BlockConnect(Source, Target: IIRBasicBlockHandle);
begin
  // If Src is nil, add entry block.
  if not Assigned(Source) then
  begin
    if Assigned(FEntryBlock) then
      raise Exception.Create('Entry basic block already exists.');

    // As we have Target handle block is already created.
    // We just mark entry.
    FEntryBlock := Target;
    exit;
  end;

  // Connect.
  FGraph.EdgeAdd(TIRBasicBlockNode(Source), TIRBasicBlockNode(Target));
end;

function TIrCFG.BlockGetEntry: IIRBasicBlockHandle;
begin
  result := FEntryBlock;
end;

function TIrCFG.BlockGetFirst: PIterator;
begin
  result := FGraph.Nodes.Values.GetEnumerator;
end;

function TIrCFG.BlockMoveNext(it: PIterator): BOOL;
begin
  result := TIRBasicBlockEnumerator(it).MoveNext;
end;

function TIrCFG.BlockGetCurrent(it: PIterator): IIRBasicBlock;
begin
  result := TIRBasicBlockEnumerator(it).Current.block;
end;

function TIrCFG.EdgeGetFirst: PIterator;
begin
  result := FGraph.Edges.GetEnumerator;
end;

function TIrCFG.EdgeMoveNext(it: PIterator): BOOL;
begin
  result := TIRBasicBlockEdgeEnumerator(it).MoveNext;
end;

function TIrCFG.EdgeGetCurrent(it: PIterator): IIRBasicBlockEdge;
begin
  result := TIRBasicBlockEdgeEnumerator(it).Current.Edge;
end;

procedure TIrCFG.FreeIterator(it: PIterator);
begin
  TObject(it).Free;
end;

end.
