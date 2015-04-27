unit uIRBasicBlock;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  VDAPI,
  glinkedlist,
  ugraph;

type
  TIRBasicBlock = class(TInterfacedObject, IIRBasicBlock)
  private type
    TSlots = TLinkedList<IRExprHandle>;
    PSlotItem = TSlots.PItem;
  private
    FSlots: TSlots;
  private
    FVA: TVA;
    FSize: integer;
  public
    constructor Create;
    destructor Destroy; override;
  public
    { VDAPI }

    function GetVA: TVA; stdcall;
    procedure SetVA(VA: TVA); stdcall;

    function GetSize: int; stdcall;
    procedure SetSize(Value: int); stdcall;

    function InsertFirst(X: IRExprHandle): PIRSlot; stdcall;
    function InsertLast(X: IRExprHandle): PIRSlot; stdcall;
    function InsertBefore(Slot: PIRSlot; X: IRExprHandle): PIRSlot; stdcall;
    function InsertAfter(Slot: PIRSlot; X: IRExprHandle): PIRSlot; stdcall;

    function GetFirstSlot: PIRSlot; stdcall;
    function GetLastSlot: PIRSlot; stdcall;
    function GetNextSlot(Slot: PIRSlot): PIRSlot; stdcall;
    function GetPrevSlot(Slot: PIRSlot): PIRSlot; stdcall;

    function GetSlotExpression(Slot: PIRSlot): IRExprHandle; stdcall;
  end;

  TIRBasicEdge = class(TInterfacedObject, IIRBasicBlockEdge)
  public
    Source: IIRBasicBlock;
    Target: IIRBasicBlock;
    function GetSource: IIRBasicBlock; stdcall;
    function GetTarget: IIRBasicBlock; stdcall;
  end;

  TIRBasicBlockEdge = class(TBasicGraphEdge)
  public
    Edge: TIRBasicEdge;
  end;

  // TIRBasicBlockNode = IIRBasicBlockHandle
  TIRBasicBlockNode = class(TBasicGraphNode)
  public
    Block: IIRBasicBlock; // don't need Free?
  end;

  TIRBasicBlockGraph = class(TGraph)
  protected
    function GetNodeClass: TBasicGraphNodeClass; override;
    function GetEdgeClass: TBasicGraphEdgeClass; override;
  end;

  TIRBasicBlockEnumerator = TEnumerator<TIRBasicBlockNode>;
  TIRBasicBlockEdgeEnumerator = TEnumerator<TIRBasicBlockEdge>;

implementation

{ TIRBasicBlock }

constructor TIRBasicBlock.Create;
begin
  inherited Create;
  FSlots := TSlots.Create;
end;

destructor TIRBasicBlock.Destroy;
begin
  FSlots.Free;
  inherited;
end;

function TIRBasicBlock.GetVA: TVA;
begin
  Result := FVA;
end;

procedure TIRBasicBlock.SetVA(VA: TVA);
begin
  FVA := VA;
end;

function TIRBasicBlock.GetSize: int;
begin
  Result := self.FSize;
end;

procedure TIRBasicBlock.SetSize(Value: int);
begin
  self.FSize := Value;
end;

function TIRBasicBlock.InsertFirst(X: IRExprHandle): PIRSlot;
begin
  Result := FSlots.InsertFirst(X);
end;

function TIRBasicBlock.InsertLast(X: IRExprHandle): PIRSlot;
begin
  Result := FSlots.InsertLast(X);
end;

function TIRBasicBlock.InsertBefore(Slot: PIRSlot; X: IRExprHandle): PIRSlot;
begin
  Result := FSlots.InsertBefore(Slot, X);
end;

function TIRBasicBlock.InsertAfter(Slot: PIRSlot; X: IRExprHandle): PIRSlot;
begin
  Result := FSlots.InsertAfter(Slot, X);
end;

function TIRBasicBlock.GetFirstSlot: PIRSlot;
begin
  Result := FSlots.First;
end;

function TIRBasicBlock.GetLastSlot: PIRSlot;
begin
  Result := FSlots.Last;
end;

function TIRBasicBlock.GetNextSlot(Slot: PIRSlot): PIRSlot;
begin
  Result := PSlotItem(Slot).Next;
end;

function TIRBasicBlock.GetPrevSlot(Slot: PIRSlot): PIRSlot;
begin
  Result := PSlotItem(Slot).Prev;
end;

function TIRBasicBlock.GetSlotExpression(Slot: PIRSlot): IRExprHandle; stdcall;
begin
  if Assigned(Slot) then
    Result := PSlotItem(Slot).Data
  else
    Result := nil;
end;

{ TIRBasicBlockGraph }

function TIRBasicBlockGraph.GetEdgeClass: TBasicGraphEdgeClass;
begin
  Result := TIRBasicBlockEdge;
end;

function TIRBasicBlockGraph.GetNodeClass: TBasicGraphNodeClass;
begin
  Result := TIRBasicBlockNode;
end;

{ TIRBasicEdge }

function TIRBasicEdge.GetSource: IIRBasicBlock;
begin
  Result := Source;
end;

function TIRBasicEdge.GetTarget: IIRBasicBlock;
begin
  Result := Target;
end;

end.
