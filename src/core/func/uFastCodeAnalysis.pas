{
  Simple code flow analysis.
  Not performing data flow analysis.
}
unit uFastCodeAnalysis;

interface

uses
  System.Generics.Collections,
  grbtree,
  VDAPI,
  uIRBasicBlock,
  VDLib.VARegions,
  uRegions.GlobalBasicBlocks;

{$UNDEF ANALYSIS_LOG}


type
  TRefInfo = record
    FromVA: TVA; // if BAD_VA, means Unknown
    Kind: TVDReferenceKind;
    constructor Create(FromVA: TVA; Kind: TVDReferenceKind);
  end;

  TRefInfoList = TList<TRefInfo>;

  TVARegionInfo = class

    // Where this block is referenced from (i.e. in-edges).
    Refs: TRefInfoList;

    // Node is set on copy-to-graph stage.
    Node: TIRBasicBlockNode;

    constructor Create(const InitialRef: TRefInfo);
    destructor Destroy; override;
  end;

  // todo: maybe to replace with VA-range tree (less memory consumption)
  // TVATree = TRBTree<TVA>;
  TVATree = TVRegions<TVARegionInfo>;

  TAnalysisStats = record
    SubFunc: uint32;
    AnalysisRecursionLevel: uint32;
    procedure Clear;
  end;

  TDoneListContainer = class
  public
    class procedure DoneListNotify(Sender: TObject; const Item: TVARegionInfo;
      Action: TCollectionNotification);
  end;

  // Done is list of already processed.
  // Func is optional.
  // Cpu is mandatory.
procedure DoCodeAnalysis(
  const Func: IVDFunction;
  const Cpu: IVDCpu;
  VA: TVA;
  const RefInfo: TRefInfo;
  CodeType: BSTR_IN;
  Flags: TVDCodeAnalysisFlags;
  DoneList: TVATree;
  var Stats: TAnalysisStats;
  CalledViaRecursion: boolean = False);

implementation

{ .$DEFINE LOG_ALREADY_DONE }

uses
  System.AnsiStrings,
  System.SysUtils,
  uCore,
  uCpuInstrBasicInfo,
  uFunction,
  uIrCfg;

const
  MAKE_TYPE_FLAGS = 0;
  RECURSIVE_CALL  = True; // Code analysis called recursively (for stats)

function VALess(const a, b: TVA): boolean; inline;
begin
  result := a < b;
end;

procedure DoMemAccess(const c: IVDCore; VA: TVA; Size: Integer); inline;
begin
  case Size of
    1:
      c.MakeType(VA, TVDStdTypeName.u8, MAKE_TYPE_FLAGS);
    2:
      c.MakeType(VA, TVDStdTypeName.u16, MAKE_TYPE_FLAGS);
    4:
      c.MakeType(VA, TVDStdTypeName.u32, MAKE_TYPE_FLAGS);
    8:
      c.MakeType(VA, TVDStdTypeName.u64, MAKE_TYPE_FLAGS);
  end;
end;

function DoHandleAddress(
  const c: IVDCore;
  VAToHandle: TVA;
  CodeType: BSTR_IN;
  const inf: IVDCpuInstrBasicInfo;
  Flags: TVDCodeAnalysisFlags): boolean;
var
  i: Integer;
  memVA: TVA;
  memSize: SIZE_T;
  memKind: TMemoryAccessKind;
  RefKind: TVDReferenceKind;
begin
  if (Flags and TVDCodeAnalysis.CA_MARK_DATA_REFS) <> 0 then
  begin
    if inf.MemoryAccessCount <> 0 then
      for i := 0 to inf.MemoryAccessCount - 1 do
      begin
        inf.GetMemoryAccess(i, memVA, memSize, memKind);

        // if not sure in mem.access and it's not allowed, then skip it
        if ((memKind and TMemoryAccessKinds.MEMACC_NOT_SURE) <> 0) and
          ((Flags and TVDCodeAnalysis.CA_CONST_AS_ADDR) = 0) then
          continue;

        // have to check mem access is valid, because it can be
        // non-valid VAToHandle, like 0 in fs:[0]
        if c.VM.Exists(memVA) then
        begin
          DoMemAccess(c, memVA, memSize);

          if ((memKind and TMemoryAccessKinds.MEMACC_READ) <> 0) then
            RefKind := REFKIND_READ
          else if ((memKind and TMemoryAccessKinds.MEMACC_WRITE) <> 0) then
            RefKind := REFKIND_WRITE
          else
            RefKind := REFKIND_READORWRITE;

          // referencing memVA from VAToHandle
          if (Flags and TVDCodeAnalysis.CA_MARK_DATA_REFS) <> 0 then
            c.Refs.Put(memVA, VAToHandle, RefKind)
          else if (Flags and TVDCodeAnalysis.CA_UNDF_DATA_REFS) <> 0 then
            c.Refs.Del(memVA, VAToHandle);
        end;
      end;
  end;


  // code

  // define code
  if (Flags and TVDCodeAnalysis.CA_MAKE_CODE) <> 0 then
  begin
    result := c.MakeType(VAToHandle, CodeType, MAKE_TYPE_FLAGS) <> 0;
    exit;
  end
  // undefine code
  else if (Flags and TVDCodeAnalysis.CA_UNDF_CODE) <> 0 then
  begin
    result := c.Undefine(VAToHandle) <> 0;
    exit;
  end;

  exit(True);
end;

type
  TBlockRange = record
    Start, &End: TVA;
  end;

procedure CopyVARegionsIntoGlobalBlocks(const c: IVDCore; const Regions: TVATree);
var
  Region: TVATree.TVRegion;
  Blocks: TVDGlobalBasicBlocks;
begin
  Blocks := c.BasicBlocks as TVDGlobalBasicBlocks;
  for Region in Regions.Items do
  begin
    Blocks.Put(Region.VA, Region.Size, 0, nil);
  end;
end;

procedure CopyVARegionsIntoGraph(const Regions: TVATree; const Cfg: TIRCFG);
var
  Region, SrcRegion: TVATree.TVRegion;
  RefInfo: TRefInfo;
  SrcNode, TgtNode: TIRBasicBlockNode;
  NodeCount: Integer;
begin
  // Add all ranges as blocks.
  NodeCount := 0;
  for Region in Regions.Items do
  begin
    if (Region.Data.Refs.Count = 1) then
    begin
      RefInfo := Region.Data.Refs[0];
      if RefInfo.Kind = REFKIND_FALLTHROUGH then
      begin
        // The only reference is fallthrough -> regions can be merged.
      end;
    end;

    Region.Data.Node := Cfg.BlockAdd(Region.VA, Region.Size);
    inc(NodeCount);
  end;

{$IFDEF DEBUG}
  CoreGet.Log.WriteLn(Format('%d node(s)', [NodeCount]));
{$ENDIF}
  // Add all edges.
  for Region in Regions.Items do
    for RefInfo in Region.Data.Refs do
      if RefInfo.FromVA <> BAD_VA then
        if Regions.FindRegion(RefInfo.FromVA, SrcRegion) then
        begin
          SrcNode := SrcRegion.Data.Node;
          TgtNode := Region.Data.Node;
          Cfg.EdgeAdd(SrcNode, TgtNode);
        end;
end;

function IsOnlyCallBranch(const inf: IVDCpuInstrBasicInfo): boolean;
var
  br: TBranchRec;
begin
  if (inf.BranchCount = 1) then
  begin
    inf.GetBranch(0, br);
    exit(br.Kind = Call);
  end;
  exit(False);
end;

procedure DoCodeAnalysis(
  const Func: IVDFunction;
  const Cpu: IVDCpu;
  VA: TVA;
  const RefInfo: TRefInfo;
  CodeType: BSTR_IN;
  Flags: TVDCodeAnalysisFlags;
  DoneList: TVATree;
  var Stats: TAnalysisStats;
  CalledViaRecursion: boolean);
var
  c: IVDCore;
  splitLeft, splitRight, splitCause: TVATree.TVRegionRBTree.TRBNodePtr;
  stumpedBlock, currentBlock: TVATree.TVRegionRBTree.TRBNodePtr;
var
  sCodeType: string;
  ownDoneList: boolean;
  parsedSize, blockSize: SIZE_T;
  inf: IVDCpuInstrBasicInfo;
  i: Integer;
  br: TBranchRec;
  FallThrough: boolean;
  pFallThrough: PBoolean;
  bIsSwitchTableJump: boolean;
  brKindIsReturn: boolean;
var
  io: IVDStreamIO;
  addrSize: uint32;
  bSkipProblemBadVa: boolean;
var
  // basic block
  BB_va0: TVA;             // first instr. of block
  BB_va1: TVA;             // last instr. of block
  BlockRange: TBlockRange; // start/end of block
  TmpRgn: TVATree.TVRegion;
  IntersectRgn: TVATree.TVRegionRBTree.TRBNodePtr;
begin
  c := CoreGet();

  if not c.VM.Exists(VA) then
  begin
    // c.Log.WriteLn(Format('0x%x does not exist', [VA]));
    exit;
  end;

  // CurNode := nil;
  io := nil;
  addrSize := c.GetData.AddressSize;

  // if know addr where came from,
  // add or del ref
  if RefInfo.FromVA <> BAD_VA then
  begin
    // don't mark fallthrough
    if RefInfo.Kind <> REFKIND_FALLTHROUGH then
    begin
      if (Flags and TVDCodeAnalysis.CA_MARK_CODE_REFS) <> 0 then
        c.Refs.Put(VA, RefInfo.FromVA, RefInfo.Kind)
      else if (Flags and TVDCodeAnalysis.CA_UNDF_CODE_REFS) <> 0 then
        c.Refs.Del(VA, RefInfo.FromVA);
    end;
  end;

  // if this address was done before, exit
  if (assigned(DoneList)) then
  begin
{$IFDEF DEBUG}
    // c.Log.WriteLn(DoneList.Print);
{$ENDIF}
    currentBlock := DoneList.Find(VA);
    if (assigned(currentBlock)) then
    begin
      // Already done.

      // If it's jump or fall from some block into middle (not start) of already
      // existing block, current block must be splitted and references corrected.
      if RefInfo.Kind in [REFKIND_JUMP, REFKIND_FALLTHROUGH] then
      begin
        // Try to split existing region by VA.
        // If it returns false there's either no region or VA is region first/last.
        // If true, VA is in the middle of some region.
        if DoneList.Split(VA, splitLeft, splitRight) then
        begin
          // Splitted part (splitRight) must have reference corrected.
          // It is referenced from splitLeft as fallthrough.
          splitRight^.K.Data := TVARegionInfo.Create(TRefInfo.Create(splitLeft^.K.VA, REFKIND_FALLTHROUGH));
          // One more reference which caused split.
          splitCause := DoneList.Find(RefInfo.FromVA);
          splitRight^.K.Data.Refs.Add(TRefInfo.Create(splitCause^.K.VA, RefInfo.Kind));
        end
        else
        begin
          // Block where it was found was not splitted.
          // It can be jump to block created earlier.
          // Don't need to analyse this block.
          // But need to add reference for this block.

          // get block where it referenced from
          splitLeft := DoneList.Find(RefInfo.FromVA);

          // update references of current block
          currentBlock^.K.Data.Refs.Add(TRefInfo.Create(splitLeft^.K.VA, RefInfo.Kind));
        end;
      end;

      exit;
    end;
  end;

  // ---------------------------------------------------------------------------
  // Stats
  // ---------------------------------------------------------------------------
  // It's another sub-function.
  if RefInfo.Kind = REFKIND_CALL then
    inc(Stats.SubFunc);
  // Recrusions
  if CalledViaRecursion then
    inc(Stats.AnalysisRecursionLevel);
  // ---------------------------------------------------------------------------

  // skip trace loop?
  if (Flags and TVDCodeAnalysis.CA_SKIP_TRACELOOP) <> 0 then
    exit;

  // notify on start of block analysis
{$IFDEF ANALYSIS_LOG}
  c.Log.WriteLn(Format('Analysis of 0x%x', [VA]));
{$ENDIF}
  (c as TVDCore).AnalysisVA := VA;

  if CodeType = nil then
  begin
    sCodeType := string(c.GetData.CodeType);
    CodeType := BSTR_IN(sCodeType);
  end;

  // fetch done list
  // it can be nil only once
  ownDoneList := DoneList = nil;
  if ownDoneList then
  begin
    // DoneList := TVATree.Create(VALess);
    DoneList := TVATree.Create;
    DoneList.OnNotify := TDoneListContainer.DoneListNotify;
  end;

  // create info
  // one intf is used for each instruction
  inf := TVDCpuInstrBasicInfo.Create;

  // by default can fall to next instruction
  // it's used after block is defined
  FallThrough := True;
  pFallThrough := @FallThrough;

  try
    BB_va0 := VA;
    BB_va1 := VA;
    parsedSize := 0;
    blockSize := 0;
    stumpedBlock := nil;

    // analysis loop
    // find basic block (i.e. until some branch found)
    // then make addtional code analysis for branch
    while True do // -----------------------------------------------------------
    begin
      // mandatory init
      inf.Clear;

      // it's the main part :)
      parsedSize := Cpu.ParseBasic(VA, inf);

      // either failed to parse
      // or basic parsing is not supported
      if parsedSize = 0 then
      begin
        c.Problems.Put(VA, TProblemKinds.PROBLEM_DECODE_BASIC_ERROR);
        // {$IFDEF ANALYSIS_LOG}
        c.Log.WriteLn(Format('0x%x ParseBasic failed', [VA]));
        // {$ENDIF}
        break;
      end;

      // if we stopped at already defined block
      stumpedBlock := DoneList.Find(VA);
      if stumpedBlock <> nil then
      begin
        pFallThrough := nil; // don't allow use later
        break;
      end;

      // update block end
      inc(blockSize, parsedSize);
      BB_va1 := VA;

      if not DoHandleAddress(c, VA, CodeType, inf, Flags) then
        exit; // failed

      // simplest case if there's no branches
      if inf.BranchCount = 0 then
      begin
        inc(VA, parsedSize);
        continue;
      end;

      // if only one call branch and it's ignored
      if IsOnlyCallBranch(inf) then
      begin
        if (Flags and TVDCodeAnalysis.CA_CALLS_AS_JUMPS) <> 0 then
        begin
          break; // pretend it's jump
        end;

        // if not step into calls
        if (Flags and TVDCodeAnalysis.CA_STEP_INTO_CALL) = 0 then
        begin
          // treat call as non-branch
          inc(VA, parsedSize);
          continue;
        end;
      end;

      // branch(es)
      break;
    end; // --------------------------------------------------------------------

    // log this block
    BlockRange.Start := BB_va0;
    BlockRange.&End := BB_va0 + blockSize;

    // register this block
    if blockSize <> 0 then
    begin
      TmpRgn := TVATree.TVRegion.Create(BB_va0, blockSize);
      IntersectRgn := DoneList.Intersects(TmpRgn);

      if IntersectRgn = nil then
      begin
        TmpRgn.Data := TVARegionInfo.Create(RefInfo);
        DoneList.AddNotJoin(TmpRgn);
      end
      else
      begin
        // Region intersected.
        c.Log.WriteLn(Format('intersection: 0x%x-0x%x from 0x%x', [BlockRange.Start, BlockRange.&End, RefInfo.FromVA]));
        exit;
      end;
    end;

    // if block discovering finished by stumping on some block stumped block
    // need reference
    if stumpedBlock <> nil then
    begin
      // Fallthrough because we stumped on it.
      stumpedBlock^.K.Data.Refs.Add(TRefInfo.Create(BB_va0, REFKIND_FALLTHROUGH));
    end;

    // if block discovering ended with decoding problem we must exit
    if parsedSize = 0 then
    begin
      exit;
    end;

    // handle branch instructions

    // process other branches

    for i := 1 to inf.BranchCount do
    begin
      if inf.GetBranch(i - 1, br) then
      begin

        bSkipProblemBadVa := False; // don't skip

        if br.IsPtr and c.VM.Exists(br.DstVA) then
        begin
          // If this pointer is import, no branched address can be read.
          if not c.ImportSymbols.Exists(br.DstVA) then
          begin
            // Read branch pointed.
            io := c.VM.CreateStreamIO(br.DstVA, BAD_VA, True);
            br.DstVA := io.ReadWord(addrSize);
          end
          else
          begin
            // It's import.

            // Destination address is unknown.
            br.DstVA := BAD_VA;
            bSkipProblemBadVa := True; // skip problem if BAD_VA

            // Else if it's imports and it CALLs function which is NO-RETURN,
            // no fallthrough needed. But we can't implement it until no-ret and
            // other import info is available.
          end;
        end;

        // uncond. branch leads into outer space, probably it's trash branch
        // undefine it (current branch)
        if
          (not inf.Conditional) and
          (not br.IsPtr) and
          (br.DstVA <> BAD_VA) and
          (not c.VM.Exists(br.DstVA)) then
        begin
          c.Undefine(BB_va0, (BB_va1 + parsedSize) - BB_va0);
          // {$IFDEF ANALYSIS_LOG}
          c.Log.WriteLn(Format('Bad branch detected and undefined: 0x%x .. 0x%x', [BB_va0, BB_va1]));
          // {$ENDIF}
          continue;
        end;

        brKindIsReturn := br.Kind = TCpuInstrBranchKind.Return;

        if not bSkipProblemBadVa then
        begin
          // if it's branch and address of branch is unknown, put it into problems
          if (br.DstVA = BAD_VA) and (not brKindIsReturn) then
          begin
            c.Problems.Put(VA, TProblemKinds.PROBLEM_UNKNOWN_BRANCH_ADDR);
          end;
        end;

        //
        // process branch kind
        //

        // if br.Kind = TCpuInstrBranchKind.Null then
        // raise Exception.Create('Unhandled branch kind (null)');

        // call with override
        if (br.Kind = TCpuInstrBranchKind.Call) and
          ((Flags and TVDCodeAnalysis.CA_CALLS_AS_JUMPS) <> 0) then
        begin
          br.Kind := TCpuInstrBranchKind.Jump;
        end;

        // call
        if br.Kind = TCpuInstrBranchKind.Call then
        begin
          if assigned(pFallThrough) then
            pFallThrough^ := True; // need check for no-return

          if (br.DstVA <> BAD_VA) then
          begin
            // if options say visit calls
            if (Flags and TVDCodeAnalysis.CA_STEP_INTO_CALL) <> 0 then
            begin
              // normally call allow fallthrough, except when called function
              // is no-return also obfuscation can use calls as noret jumps

              // TODO: recursion
              DoCodeAnalysis(
                nil,
                Cpu,
                br.DstVA,
                TRefInfo.Create(VA, REFKIND_CALL),
                CodeType,
                Flags,
                DoneList,
                Stats,
                RECURSIVE_CALL);
            end
            else
            begin
              // Update code refs only

              // TODO: recursion
              DoCodeAnalysis(
                nil,
                Cpu,
                br.DstVA,
                TRefInfo.Create(VA, REFKIND_CALL),
                CodeType,
                Flags or TVDCodeAnalysis.CA_SKIP_TRACELOOP,
                DoneList,
                Stats,
                RECURSIVE_CALL);
            end;
          end;
        end;

        // If at least one switch table jump (i.e. it's jump by switch table)
        // no fallthrough possible.
        bIsSwitchTableJump := (br.Kind = TCpuInstrBranchKind.SwitchTableJump);
        if bIsSwitchTableJump then
        begin
          pFallThrough := nil;
          br.Kind := TCpuInstrBranchKind.Jump;
        end;

        // jump
        if br.Kind = TCpuInstrBranchKind.Jump then
        begin
          // Unconditional jump has no fallthrough.
          if not inf.Conditional then
            pFallThrough := nil;
          if (br.DstVA <> BAD_VA) then
          begin
            // Jump is part of function.

            // TODO: recursion
            DoCodeAnalysis(
              Func,
              Cpu,
              br.DstVA,
              TRefInfo.Create(VA, REFKIND_JUMP),
              CodeType,
              Flags,
              DoneList,
              Stats,
              RECURSIVE_CALL);
          end;
        end;

        // return
        // don't need fallthrough
        if brKindIsReturn then
        begin
          // Return means end of function, i.e. no fallthrough.
          pFallThrough := nil;
        end;

      end;
    end;

    // after branches done, process fallthrough
    // BAD_VA means don't add reference for fall-through branch
    if assigned(pFallThrough) and (pFallThrough^) then
    begin
      // Fallthrough is part of function.
      DoCodeAnalysis(
        Func,
        Cpu,
        VA + parsedSize,
        TRefInfo.Create(VA, REFKIND_FALLTHROUGH),
        CodeType,
        Flags,
        DoneList,
        Stats,
        RECURSIVE_CALL);
    end;

  finally
    // Put found blocks into core.
    CopyVARegionsIntoGlobalBlocks(c, DoneList);
    // cleanup
    if ownDoneList then
    begin
      // For topmost function fill graph (function cfg).
      if assigned(Func) then
        CopyVARegionsIntoGraph(DoneList, TIRCFG(Func.Cfg));
      DoneList.Free;
    end;
  end;
end;

{ TAnalysisStats }

procedure TAnalysisStats.Clear;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

{ TRefInfo }

constructor TRefInfo.Create(FromVA: TVA; Kind: TVDReferenceKind);
begin
  Self.FromVA := FromVA;
  Self.Kind := Kind;
end;

{ TVARegionInfo }

constructor TVARegionInfo.Create(const InitialRef: TRefInfo);
begin
  inherited Create;
  Self.Refs := TRefInfoList.Create;
  Self.Refs.Add(InitialRef);
end;

destructor TVARegionInfo.Destroy;
begin
  FreeAndNil(Refs);
  inherited;
end;

{ TDoneListContainer }

class procedure TDoneListContainer.DoneListNotify(Sender: TObject;
  const Item: TVARegionInfo; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

end.
