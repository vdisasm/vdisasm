unit uFrameDisasm;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,

  uColorText,
  uDisasmText,

  GraphControl,
  uDisasmGraphControl,
  uDisasmGeometricGraph,

  VDAPI;

type
  TDisAsmView = (dv_disasm, dv_graph);

  TFrameDisasm = class(TFrame)
    StatusBar1: TStatusBar;
    ppDis: TPopupMenu;
    N10: TMenuItem;
    AnalyseCode1: TMenuItem;
    DeanalyseCode1: TMenuItem;
    N7: TMenuItem;
    AnalysisOptions1: TMenuItem;
    StepIntoCall1: TMenuItem;
    MarkUnmarkDataRefs1: TMenuItem;
    MarkUnmarkCodeRefs1: TMenuItem;
    DefineUndefineCode1: TMenuItem;
    HandleCallsasJumps1: TMenuItem;
    N11: TMenuItem;
    Define1: TMenuItem;
    DefineType1: TMenuItem;
    DefineString1: TMenuItem;
    N9: TMenuItem;
    DefineU8: TMenuItem;
    DefineU16: TMenuItem;
    DefineU32: TMenuItem;
    DefineU64: TMenuItem;
    N5: TMenuItem;
    code1: TMenuItem;
    EditValue1: TMenuItem;
    DefineName: TMenuItem;
    DefineComment: TMenuItem;
    DefineExport: TMenuItem;
    N6: TMenuItem;
    Undefine1: TMenuItem;
    N8: TMenuItem;
    DeleteReferences1: TMenuItem;
    N1: TMenuItem;
    Go1: TMenuItem;
    VA1: TMenuItem;
    N3: TMenuItem;
    VMStart1: TMenuItem;
    VMEnd1: TMenuItem;
    N2: TMenuItem;
    CopyText1: TMenuItem;
    CopySpecial1: TMenuItem;
    N4: TMenuItem;
    HexDumpWidth1: TMenuItem;
    HexDumpWidth_0: TMenuItem;
    HexDumpWidth_4: TMenuItem;
    HexDumpWidth_8: TMenuItem;
    HexDumpWidth_16: TMenuItem;
    Highlighting1: TMenuItem;
    HighlightingOn1: TMenuItem;
    HighlightingOff1: TMenuItem;
    HighlightingText: TMenuItem;
    ppGraph: TPopupMenu;
    Layout1: TMenuItem;
    procedure AnalyseCode1Click(Sender: TObject);
    procedure code1Click(Sender: TObject);
    procedure CopySpecial1Click(Sender: TObject);
    procedure CopyText1Click(Sender: TObject);
    procedure HexDumpWidth_0Click(Sender: TObject);
    procedure DeanalyseCode1Click(Sender: TObject);
    procedure DefineCommentClick(Sender: TObject);
    procedure DefineExportClick(Sender: TObject);
    procedure DefineNameClick(Sender: TObject);
    procedure DefineString1Click(Sender: TObject);
    procedure DefineU16Click(Sender: TObject);
    procedure DefineU32Click(Sender: TObject);
    procedure DefineU8Click(Sender: TObject);
    procedure DefineU64Click(Sender: TObject);
    procedure DeleteReferences1Click(Sender: TObject);
    procedure HexDumpWidth_4Click(Sender: TObject);
    procedure HexDumpWidth_8Click(Sender: TObject);
    procedure HexDumpWidth_16Click(Sender: TObject);
    procedure HighlightingOn1Click(Sender: TObject);
    procedure HighlightingTextClick(Sender: TObject);
    procedure HighlightingOff1Click(Sender: TObject);
    procedure ppDisPopup(Sender: TObject);
    procedure EditValue1Click(Sender: TObject);
    procedure Undefine1Click(Sender: TObject);
    procedure VA1Click(Sender: TObject);
    procedure VMStart1Click(Sender: TObject);
    procedure VMEnd1Click(Sender: TObject);
    procedure DefineType1Click(Sender: TObject);
    procedure Layout1Click(Sender: TObject);
  private
    function TryDisplayType(VA: TVA): boolean;
    procedure RecordFieldChanged(Sender: TObject);
  private
    procedure DisasmKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure DisasmDblClick(Sender: TObject);
    procedure DisasmVAChanged(Sender: TObject);

    procedure GraphKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  protected
    CodeAnalysisFlags: TVDCodeAnalysisFlags;
    CodeDeAnalysisFlags: TVDCodeAnalysisFlags;
    procedure PerformAnalysis(Flags: TVDCodeAnalysisFlags);
  public
    CurrentViewType: TDisAsmView;
    procedure SwitchToView(View: TDisAsmView);
    procedure ToggleView;
  public
    DisasmText: TVDDisasmText;
    GraphText: TDisasmGraphControl;

    constructor Create(AOwner: TComponent); override;

    function DoEditValue: boolean;

    procedure DoMakeString;
    // If TypeName is empty, it is asked.
    procedure DoMakeType(const TypeName: string = '');
    procedure DoMakeUndef;
    procedure DoGoTo;
    procedure DoMakeName;
    procedure DoMakeComment;
    procedure DoMakePtrDword;
    procedure DoAnalyseCode;
    procedure DoDeAnalyseCode;
    procedure DoDecompile(VA: TVA);
    procedure DoToggleBreakpoint(VA: TVA);
    procedure DoTryGraph;
  published
    // It's published so that component loading will find it.
    procedure CodeAnalysisFlagClick(Sender: TObject);
  end;

implementation

{$R *.dfm}


uses
  System.Diagnostics,

  uStrings,
  uCore.Strings,

  // expand begin
  uFormMain,
  uFrameHex,
  uFrameLog,
  uFrameSections,
  uFrameImports,
  uFrameExports,
  uFrameRefs,
  uFrameNames,
  uFrameProblems,
  uFrameWatches,
  uFrameBreakpoints,
  // expand end

  uFormTextInput,
  uFormTypeLibBrowserRecord,
  uFormCopySpecial,
  uFormDefineString,

{$IFDEF ENABLE_DECOMPILATION_FRAME}
  uFrameDecompilation,
{$ENDIF}
  uKeyConsts,
  uEditField,

  uListenerMain,

  uDebugger,

  uDockLayouts,

  OGDF;

{ TFrame1 }

constructor TFrameDisasm.Create(AOwner: TComponent);
begin
  inherited;

  // Create and own DisasmText control.
  DisasmText := TVDDisasmText.Create(Self);
  DisasmText.Align := alClient;
  DisasmText.Parent := Self;
  DisasmText.OnKeyDown := DisasmKeyDown;
  DisasmText.OnDblClick := DisasmDblClick;
  DisasmText.OnVAChanged := DisasmVAChanged;
  DisasmText.PopupMenu := ppDis;
  // DisasmText.HeaderControl := HeaderControl1;

  GraphText := TDisasmGraphControl.Create(Self);
  GraphText.Visible := False;
  GraphText.Align := alClient;
  GraphText.Parent := Self;
  GraphText.OnKeyDown := Self.GraphKeyDown;
  GraphText.OwnsGraph := True; // to automatically free it
  GraphText.PopupMenu := ppGraph;

  // Setup flags for code analysis.
  CodeAnalysisFlags :=
    VDAPI.TVDCodeAnalysis.CA_STEP_INTO_CALL or
    VDAPI.TVDCodeAnalysis.CA_MARK_DATA_REFS or
    VDAPI.TVDCodeAnalysis.CA_MARK_CODE_REFS or
    VDAPI.TVDCodeAnalysis.CA_MAKE_CODE or
    VDAPI.TVDCodeAnalysis.CA_CONST_AS_ADDR or
    0;

  CodeDeAnalysisFlags :=
    VDAPI.TVDCodeAnalysis.CA_STEP_INTO_CALL or
    VDAPI.TVDCodeAnalysis.CA_UNDF_DATA_REFS or
    VDAPI.TVDCodeAnalysis.CA_UNDF_CODE_REFS or
    VDAPI.TVDCodeAnalysis.CA_UNDF_CODE or
    VDAPI.TVDCodeAnalysis.CA_CONST_AS_ADDR or
    0;
end;

procedure TFrameDisasm.DisasmDblClick(Sender: TObject);
begin
  DisasmText.NavigateToExpressionAtCursor;
end;

procedure TFrameDisasm.GraphKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkSpace:
      SwitchToView(dv_disasm);
  end;
end;

procedure TFrameDisasm.DisasmKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  c: IVDCore;
  str: string;
  VA: TVA;
begin
  c := CoreGet;
  VA := DisasmText.CursorVA;

  case Key of
    VK_RETURN:
      // ctrl + enter
      if ssCtrl in Shift then
      begin
        DisasmText.NavigateForward;
        exit;
      end;
  end;

  if Shift = [] then
  begin
    case Key of
      vkEscape, vkBack:
        DisasmText.NavigateBack;
      vkReturn:
        DisasmDblClick(nil);

      VK_EDIT_TYPED_VALUE:
        DoEditValue;

      vkC:
        if Shift = [] then
        begin
          if c.GetData.CodeType <> '' then
          begin
            str := string(c.GetData.CodeType);
            c.MakeType(VA, BSTR_IN(str));
            DisasmText.InvalidateAllRowsWithRebuilding;
          end;
        end;
      vkG:
        DoGoTo;
      vkN:
        DoMakeName;
      vkF:
        DoAnalyseCode;
      vkSlash:
        DoMakeComment;
      vkQ:
        DoMakePtrDword;

      vkU:
        DoMakeUndef;
      vkY:
        DoMakeType;
      vk1:
        DoMakeType('u8');
      vk2:
        DoMakeType('u16');
      vk4:
        DoMakeType('u32');
      vk8:
        DoMakeType('u64');

      vkF2:
        DoToggleBreakpoint(VA);
{$IFDEF ENABLE_DECOMPILATION_FRAME}
      vkF5:
        DoDecompile(VA);
{$ENDIF}
      vkSpace:
        DoTryGraph;
    end;

  end;
end;

procedure TFrameDisasm.DisasmVAChanged(Sender: TObject);
var
  c: IVDCore;
  VA: TVA;
begin
  // StatusBar1.Panels[0].Text := Format('%x', [DisasmText.CursorVA]);

  c := CoreGet;
  if c <> nil then
  begin
    VA := c.GetUserVA;
    StatusBar1.Panels[0].Text := Format('%x', [VA]);
  end;

  StatusBar1.Panels[1].Text := Format('%d,%d', [DisasmText.CaretX, DisasmText.CaretY]);

  UpdateViews([view_refs]);
end;

procedure TFrameDisasm.PerformAnalysis(Flags: TVDCodeAnalysisFlags);
var
  func: IVDFunction;
begin
  try
    // In try..finally because it can raise exception.
    func := CoreGet().CodeAnalysis(DisasmText.CursorVA, nil, Flags);
  finally
    DisasmText.InvalidateAllRowsWithRebuilding;
  end;
end;

function TFrameDisasm.DoEditValue: boolean;
begin
  Result := TryDisplayType(DisasmText.CursorVA);
end;

procedure TFrameDisasm.ToggleView;
begin
  case CurrentViewType of
    dv_disasm:
      SwitchToView(dv_graph);
    dv_graph:
      SwitchToView(dv_disasm);
  end;
end;

function TFrameDisasm.TryDisplayType(VA: TVA): boolean;
var
  c: IVDCore;
  Name: bstr;
  Size: uint32;
  Typ: IVDType;
  RecFrm: TFormTypeLibBrowserRecord;
begin
  Result := False;
  c := CoreGet();
  if c.Decoder.GetTypeName(VA, Name, Size) then
  begin
    Typ := c.TypeLib.FindType(nil, BSTR_IN(Name), True);
    if Assigned(Typ) then
    begin
      case Typ.Kind of
        TYPEKIND_RECORD:
          begin
            RecFrm := uFormTypeLibBrowserRecord.DisplayRecord(
              Typ as IVDRecordType, VA, Mouse.CursorPos, False);
            RecFrm.OnFieldChanged := RecordFieldChanged;
            exit(True);
          end;
      else
        if uEditField.TryEditFieldText(VA, 0, Name) then
          DisasmText.InvalidateCurrentLine;
      end;
    end;
  end;
end;

procedure TFrameDisasm.RecordFieldChanged(Sender: TObject);
begin
  DisasmText.InvalidateCurrentLine;
end;

procedure TFrameDisasm.SwitchToView(View: TDisAsmView);
begin
  case View of
    dv_disasm:
      begin
        GraphText.Hide;
        DisasmText.Show;
        if DisasmText.Enabled then
          DisasmText.SetFocus;
      end;
    dv_graph:
      begin
        DisasmText.Hide;
        GraphText.Show;
        if GraphText.Enabled then
          GraphText.SetFocus;
      end;
  end;
end;

procedure TFrameDisasm.DoAnalyseCode;
begin
  PerformAnalysis(CodeAnalysisFlags);
end;

procedure TFrameDisasm.DoDeAnalyseCode;
begin
  PerformAnalysis(CodeDeAnalysisFlags);
end;

procedure TFrameDisasm.DoDecompile(VA: TVA);
{$IFDEF ENABLE_DECOMPILATION_FRAME}
var
  ctl: TControl;
  fn: IVDFunction;
{$ENDIF}
begin
{$IFDEF ENABLE_DECOMPILATION_FRAME}
  ctl := ChangeViewVisible(view_decompiler, True);
  fn := AnalyseCodeIntoFunction(VA, nil);
  (ctl as TFrameDecompilation).SetFn(fn);
{$ENDIF}
end;

procedure TFrameDisasm.DoGoTo;
var
  VA: TVA;
  str: string;
begin
  if InputText(str, SEnterVA) then
  begin
    if CoreGet.EvaluateVA(BSTR_IN(str), VA) then
      DisasmText.NavigateToVA(VA);
  end;
end;

procedure TFrameDisasm.DoMakeComment;
var
  VA: TVA;
  c: IVDCore;
  Text: bstr;
  str: string;
begin
  if not DisasmText.GetVaAtCursorOrCursorVA(VA) then
    exit;
  c := CoreGet();
  if not c.VM.Exists(VA) then
    exit;
  if not c.Comments.Get(VA, Text) then
  begin
    // do nothing
    // Text := Format('%x', [VA]);
  end;
  str := Text;
  if InputText(str, Format(SInputCommentForVA, [VA])) then
  begin
    if str <> '' then
      c.Comments.Put(VA, str, 0)
    else
      c.Comments.Del(VA);
    DisasmText.InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TFrameDisasm.DoMakeName;
var
  VA: TVA;
  c: IVDCore;
  Text: bstr;
  str: string;
begin
  if not DisasmText.GetVaAtCursorOrCursorVA(VA) then
    exit;
  c := CoreGet();
  if not c.VM.Exists(VA) then
    exit;
  if not c.Names.Get(VA, Text) then
    Text := Format('%x', [VA]);
  str := Text;
  if InputText(str, Format(SInputNameForVA, [VA])) then
  begin
    if str <> '' then
      c.Names.Put(VA, str, 0)
    else
      c.Names.Del(VA);
    DisasmText.InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TFrameDisasm.DoMakePtrDword;
begin
  case CoreGet.GetData.AddressSize of
    1:
      DoMakeType('u8');
    2:
      DoMakeType('u16');
    4:
      DoMakeType('u32');
    8:
      DoMakeType('u64');
  end;
end;

procedure TFrameDisasm.DoMakeString;
var
  VA: TVA;
  c: IVDCore;
begin
  c := CoreGet();
  VA := DisasmText.CursorVA;
  if not c.VM.Exists(VA) then
    exit;

  uFormDefineString.Invoke(VA);
end;

procedure TFrameDisasm.AnalyseCode1Click(Sender: TObject);
begin
  DoAnalyseCode;
end;

procedure TFrameDisasm.code1Click(Sender: TObject);
var
  s: string;
begin
  s := string(CoreGet().GetData.CodeType);
  if s <> '' then
    DoMakeType(s);
end;

procedure TFrameDisasm.CopySpecial1Click(Sender: TObject);
var
  va0, VA1: TVA;
begin
  if DisasmText.GetSelectionVA(va0, VA1) then
    FormCopySpecial.Show(va0, VA1)
  else
  begin
    va0 := DisasmText.GetCursorVA;
    FormCopySpecial.Show(va0, va0);
  end;
end;

procedure TFrameDisasm.CopyText1Click(Sender: TObject);
var
  va0, VA1: TVA;
begin
  if DisasmText.GetSelectionVA(va0, VA1) then
    DisasmText.CopyToClipboardVaRange(va0, VA1);
end;

procedure TFrameDisasm.DeanalyseCode1Click(Sender: TObject);
begin
  DoDeAnalyseCode;
end;

procedure TFrameDisasm.DefineCommentClick(Sender: TObject);
begin
  DoMakeComment;
end;

procedure TFrameDisasm.DefineExportClick(Sender: TObject);
var
  VA: TVA;
  c: IVDCore;
  str: string;
begin
  if DisasmText.GetVaAtCursorOrCursorVA(VA) then
  begin
    c := CoreGet();

    if InputText(str, Format(SPutExportSymbolForVA, [VA])) then
    begin
      c.ExportSymbols.Put(VA, BSTR_IN(str), 0);
      DisasmText.InvalidateAllRowsWithRebuilding;
    end;
  end;
end;

procedure TFrameDisasm.DefineNameClick(Sender: TObject);
begin
  DoMakeName;
end;

procedure TFrameDisasm.DefineString1Click(Sender: TObject);
begin
  DoMakeString;
end;

procedure TFrameDisasm.DefineU16Click(Sender: TObject);
begin
  DoMakeType(TVDStdTypeName.u16);
end;

procedure TFrameDisasm.DefineU32Click(Sender: TObject);
begin
  DoMakeType(TVDStdTypeName.u32);
end;

procedure TFrameDisasm.DefineU64Click(Sender: TObject);
begin
  DoMakeType(TVDStdTypeName.u64);
end;

procedure TFrameDisasm.DefineU8Click(Sender: TObject);
begin
  DoMakeType(TVDStdTypeName.u8);
end;

procedure TFrameDisasm.CodeAnalysisFlagClick(Sender: TObject);
begin
  if Sender = StepIntoCall1 then
  begin
    CodeAnalysisFlags := CodeAnalysisFlags xor TVDCodeAnalysis.CA_STEP_INTO_CALL;
    CodeDeAnalysisFlags := CodeDeAnalysisFlags xor TVDCodeAnalysis.CA_STEP_INTO_CALL;
  end
  else if Sender = MarkUnmarkDataRefs1 then
  begin
    CodeAnalysisFlags := CodeAnalysisFlags xor TVDCodeAnalysis.CA_MARK_DATA_REFS;
    CodeDeAnalysisFlags := CodeDeAnalysisFlags xor TVDCodeAnalysis.CA_UNDF_DATA_REFS;
  end
  else if Sender = MarkUnmarkCodeRefs1 then
  begin
    CodeAnalysisFlags := CodeAnalysisFlags xor TVDCodeAnalysis.CA_MARK_CODE_REFS;
    CodeDeAnalysisFlags := CodeDeAnalysisFlags xor TVDCodeAnalysis.CA_UNDF_CODE_REFS;
  end
  else if Sender = DefineUndefineCode1 then
  begin
    CodeAnalysisFlags := CodeAnalysisFlags xor TVDCodeAnalysis.CA_MAKE_CODE;
    CodeDeAnalysisFlags := CodeDeAnalysisFlags xor TVDCodeAnalysis.CA_UNDF_CODE;
  end
  else if Sender = HandleCallsasJumps1 then
  begin
    CodeAnalysisFlags := CodeAnalysisFlags xor TVDCodeAnalysis.CA_CALLS_AS_JUMPS;
    CodeDeAnalysisFlags := CodeDeAnalysisFlags xor TVDCodeAnalysis.CA_CALLS_AS_JUMPS;
  end;
end;

procedure TFrameDisasm.DeleteReferences1Click(Sender: TObject);
var
  VA: TVA;
begin
  if DisasmText.GetVaAtCursorOrCursorVA(VA) then
  begin
    if MessageDlg(SConfirmAction, TMsgDlgType.mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      CoreGet.Refs.DelAll(VA);
      DisasmText.InvalidateAllRowsWithRebuilding;
      UpdateViews([view_refs]);
    end;
  end;
end;

procedure TFrameDisasm.HexDumpWidth_0Click(Sender: TObject);
begin
  DisasmText.DumpWidth := 0;
end;

procedure TFrameDisasm.HexDumpWidth_16Click(Sender: TObject);
begin
  DisasmText.DumpWidth := 16;
end;

procedure TFrameDisasm.HexDumpWidth_4Click(Sender: TObject);
begin
  DisasmText.DumpWidth := 4;
end;

procedure TFrameDisasm.HexDumpWidth_8Click(Sender: TObject);
begin
  DisasmText.DumpWidth := 8;
end;

procedure TFrameDisasm.HighlightingOff1Click(Sender: TObject);
begin
  DisasmText.EnableHighlighting := False;
end;

procedure TFrameDisasm.HighlightingOn1Click(Sender: TObject);
begin
  DisasmText.EnableHighlighting := True;
end;

procedure TFrameDisasm.HighlightingTextClick(Sender: TObject);
var
  s: string;
begin
  s := '';
  if InputText(s, '') then
  begin
    DisasmText.TextToHighlight := s;
    DisasmText.EnableHighlighting := True;
  end;
end;

procedure TFrameDisasm.Layout1Click(Sender: TObject);
begin
  GraphText.Layout();
end;

procedure TFrameDisasm.DoMakeType(const TypeName: string = '');
var
  str: string;
  VA: TVA;
  c: IVDCore;
begin
  c := CoreGet();
  VA := DisasmText.CursorVA;

  if not c.VM.Exists(VA) then
    exit;

  if TypeName = '' then
  begin
    if not InputText(str, SInputType) then
      exit
  end
  else
    str := TypeName;

  if str <> '' then
  begin
    c.MakeType(VA, BSTR_IN(str));
    DisasmText.InvalidateAllRowsWithRebuilding;
  end;
end;

procedure TFrameDisasm.DoMakeUndef;
var
  va0, VA1: TVA;
begin
  if DisasmText.GetSelectionVA(va0, VA1) then
    CoreGet.Undefine(va0, VA1 - va0 + 1)
  else
    CoreGet.Undefine(DisasmText.CursorVA);
  DisasmText.ClearSelection;
  DisasmText.InvalidateAllRowsWithRebuilding;
end;

procedure TFrameDisasm.ppDisPopup(Sender: TObject);
var
  c: IVDCore;
  VA: TVA;
  CursorVAorVAatCursor: TVA;
  Size: uint32;
  TypeName: bstr;
  present: boolean;
  bIsTypeDefined: boolean;
begin
  c := CoreGet;
  if c = nil then
    exit;

  VA := DisasmText.CursorVA;
  DisasmText.GetVaAtCursorOrCursorVA(CursorVAorVAatCursor);

  bIsTypeDefined := (VA <> BAD_VA) and c.Decoder.GetTypeName(VA, TypeName, Size);

  Define1.Visible := not bIsTypeDefined;
  EditValue1.Visible := bIsTypeDefined;

  code1.Caption := string(c.GetData.CodeType);
  code1.Visible := code1.Caption <> '';

  // refs
  if CursorVAorVAatCursor <> BAD_VA then
    present := c.Refs.HasReferences(CursorVAorVAatCursor)
  else
    present := False;
  DeleteReferences1.Visible := present;
  // ReferenceWindow1.Visible := present;

  // analysis options
  StepIntoCall1.Checked := (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_STEP_INTO_CALL) <> 0;
  MarkUnmarkDataRefs1.Checked := (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_MARK_DATA_REFS) <> 0;
  MarkUnmarkCodeRefs1.Checked := (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_MARK_CODE_REFS) <> 0;
  DefineUndefineCode1.Checked := (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_MAKE_CODE) <> 0;
  HandleCallsasJumps1.Checked := (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_CALLS_AS_JUMPS) <> 0;
end;

procedure TFrameDisasm.EditValue1Click(Sender: TObject);
begin
  DoEditValue;
end;

procedure TFrameDisasm.Undefine1Click(Sender: TObject);
begin
  DoMakeUndef;
end;

procedure TFrameDisasm.VA1Click(Sender: TObject);
begin
  DoGoTo;
end;

function ViewRefsEnumFunc(VA: TVA; ud: pointer): bool; stdcall;
begin
  CoreGet().Log.WriteLn(Format('  %x', [VA]));
  Result := True;
end;

procedure TFrameDisasm.VMEnd1Click(Sender: TObject);
begin
  DisasmText.NavigateToEnd;
end;

procedure TFrameDisasm.VMStart1Click(Sender: TObject);
begin
  DisasmText.NavigateToBegin;
end;

procedure TFrameDisasm.DefineType1Click(Sender: TObject);
begin
  Self.DoMakeType;
end;

procedure TFrameDisasm.DoToggleBreakpoint(VA: TVA);
begin
  VA := DisasmText.CursorVA;
  ToggleBreakpoint(VA);
  DisasmText.InvalidateVA(VA);
end;

procedure TFrameDisasm.DoTryGraph;
var
  c: IVDCore;
  func: IVDFunction;
  Flags: uint32;
  StartNode: TDisasmGGNode;
begin
  if not OGDFLoaded then
    exit;

  c := CoreGet;

  // ---------------------------------------------------------------------------
  Flags := TVDCodeAnalysis.CA_CREATE_FUNCTION;
  if (CodeAnalysisFlags and VDAPI.TVDCodeAnalysis.CA_CALLS_AS_JUMPS) <> 0 then
    Flags := Flags or TVDCodeAnalysis.CA_CALLS_AS_JUMPS;
  // ---------------------------------------------------------------------------

  func := c.CodeAnalysis(DisasmText.CursorVA, nil, Flags);
  if not Assigned(func) then
    exit;

  SwitchToView(dv_graph);
  if Assigned(GraphText.Graph) then
    GraphText.Graph := nil;

  GraphText.Graph := BuildGeometricGraphFromFunction(func, GraphText.BackBuffer, StartNode);
  try
    GraphText.Layout();
    GraphText.GoToNode(StartNode);
  except
    // It's not always catched (for example on delayed function failure, it's
    // to low level to be catched here).
    SwitchToView(dv_disasm);
  end;
end;

end.
