unit uCpuInstrBasicInfo;

interface

uses
  System.Generics.Collections,
  VDAPI;

type
  TBranchList = TList<TBranchRec>;

  TMemAcc = record
    Kind: TMemoryAccessKind;
    VA: TVA;
    Size: SIZE_T;
  end;

  TMemAccList = TList<TMemAcc>;

  TVDCpuInstrBasicInfo = class(TInterfacedObject, IVDCpuInstrBasicInfo)
  protected
    FBranchList: TBranchList;
    FMemAccList: TMemAccList;
    FConditional: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  public

    { VDAPI }

    // Clear all internal states.
    procedure Clear; stdcall;

    // If instruction changes control flow, call this function. Set kind of
    // branch. If you know branch address set VA to it, or set it BAD_VA
    // otherwise.
    // Kind: TCpuInstrBranchKind;
    procedure AddBranch(Kind: TCpuInstrBranchKind; VA: TVA; IsPtr: boolean = False); stdcall;
    function GetBranchCount: UInt; stdcall;
    function GetBranch(Index: UInt; out Rec: TBranchRec): BOOL; stdcall;

    // If instr. read or write memory, call this function.
    procedure AddMemoryAccess(VA: TVA; Size: SSIZE_T; Kind: TMemoryAccessKind); stdcall;
    function GetMemoryAccessCount: UInt; stdcall;
    function GetMemoryAccess(Index: UInt;
      out VA: TVA;
      out Size: SIZE_T;
      out Kind: TMemoryAccessKind): BOOL; stdcall;

    procedure AddSwitchTableJump(JmpVA, TableVA: TVA; Count: int); stdcall;

    function GetConditional: BOOL; stdcall;
    procedure SetConditional(Value: BOOL); stdcall;
  end;

implementation

{ TVDCpuInstrBasicInfo }

procedure TVDCpuInstrBasicInfo.AddBranch(Kind: TCpuInstrBranchKind; VA: TVA; IsPtr: boolean);
var
  r: TBranchRec;
begin
  r.Kind := Kind;
  r.DstVA := VA;
  r.IsPtr := IsPtr;
  FBranchList.Add(r);
end;

procedure TVDCpuInstrBasicInfo.AddMemoryAccess(VA: TVA; Size: SSIZE_T;
  Kind: TMemoryAccessKind);
var
  r: TMemAcc;
begin
  r.Kind := Kind;
  r.VA := VA;
  r.Size := Size;
  FMemAccList.Add(r);
end;

procedure TVDCpuInstrBasicInfo.AddSwitchTableJump(JmpVA, TableVA: TVA; Count: int);
var
  c: IVDCore;
  addrSize: UInt32;
  branchVA: TVA;
  io: IVDStreamIO;
begin
  c := CoreGet;
  if not c.VM.Exists(TableVA) then
    exit;
  if Count = 0 then
    Count := 255;

  addrSize := c.GetData.AddressSize;

  io := c.VM.CreateStreamIO(TableVA, BAD_VA, True);

  while Count > 0 do
  begin
    branchVA := io.ReadWord(addrSize);
    if not c.VM.Exists(branchVA) then
      break;

    c.MakePointer(TableVA);
    inc(TableVA, addrSize);

    AddBranch(TCpuInstrBranchKind.SwitchTableJump, branchVA);

    dec(Count);
  end;
end;

procedure TVDCpuInstrBasicInfo.Clear;
begin
  FBranchList.Clear;
  FMemAccList.Clear;
end;

constructor TVDCpuInstrBasicInfo.Create;
begin
  inherited;
  FBranchList := TBranchList.Create;
  FMemAccList := TMemAccList.Create;
end;

destructor TVDCpuInstrBasicInfo.Destroy;
begin
  FBranchList.Free;
  FMemAccList.Free;
  inherited;
end;

function TVDCpuInstrBasicInfo.GetBranch(Index: UInt; out Rec: TBranchRec): BOOL; stdcall;
begin
  if Index >= FBranchList.Count then
    exit(False);
  Rec := FBranchList[Index];
  exit(True);
end;

function TVDCpuInstrBasicInfo.GetBranchCount: UInt;
begin
  Result := FBranchList.Count;
end;

function TVDCpuInstrBasicInfo.GetConditional: BOOL;
begin
  Result := FConditional;
end;

function TVDCpuInstrBasicInfo.GetMemoryAccess(Index: UInt; out VA: TVA;
  out Size: SIZE_T; out Kind: TMemoryAccessKind): BOOL;
begin
  if Index >= FMemAccList.Count then
    exit(False);
  VA := FMemAccList[Index].VA;
  Size := FMemAccList[Index].Size;
  Kind := FMemAccList[Index].Kind;
  exit(True);
end;

function TVDCpuInstrBasicInfo.GetMemoryAccessCount: UInt;
begin
  Result := FMemAccList.Count;
end;

procedure TVDCpuInstrBasicInfo.SetConditional(Value: BOOL);
begin
  FConditional := Value;
end;

end.
