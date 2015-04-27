unit uFrameWatches;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,

  uCore.Strings,

  VirtualTrees,

  VDAPI;

type
  TWatchFlag = (wf_changed, wf_disabled);
  TWatchFlags = set of TWatchFlag;

  TWatchProcessingOption = (wpo_null, wpo_check_modification, wpo_clear_changed_flag);

  TWatchItem = record
  public
    Expr: string;
    Value: string;
    PrevValue: string;
    Flags: TWatchFlags;
    constructor Create(const Expr, PrevValue: string);
  end;

  PWatchItem = ^TWatchItem;

  TWatches = class(TList<TWatchItem>)
  public
    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
  end;

  TFrameWatches = class(TFrame)
  private
    FWatches: TWatches;
    FWatchesFileName: string;
    FWatchesWereLoaded: Boolean;
    FWatchesWereChanged: Boolean;
    procedure LoadWatchesFromFile;
    procedure SaveWatchesToFile;

    procedure VSTInitNode(Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
    procedure VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType; var Text: UnicodeString);
    procedure VSTNodeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure VSTPaintText(Sender: TBaseVirtualTree; const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);

    procedure lvWatchesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvWatchesDblClick(Sender: TObject);
  public
    lvWatches: TVirtualStringTree;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure InsertWatch(ReplaceSelected: Boolean = false);
    procedure ReplaceWatch;
    procedure DeleteSelected;

    procedure UpdateWatch(Index: integer; ModificationOption: TWatchProcessingOption = wpo_null);
    procedure UpdateWatches(ModificationOption: TWatchProcessingOption = wpo_null);
  end;

implementation

{$R *.dfm}


procedure TFrameWatches.LoadWatchesFromFile;
begin
  FWatchesWereLoaded := True;
  if FileExists(FWatchesFileName) then
  begin
    FWatches.LoadFromFile(FWatchesFileName);
    lvWatches.RootNodeCount := FWatches.Count;
    FWatchesWereChanged := True;
  end;
end;

procedure TFrameWatches.SaveWatchesToFile;
begin
  FWatches.SaveToFile(FWatchesFileName);
end;

constructor TFrameWatches.Create(AOwner: TComponent);
var
  Column: TVirtualTreeColumn;
begin
  inherited Create(AOwner);

  // init tree --------------------------------
  lvWatches := TVirtualStringTree.Create(self);
  lvWatches.Parent := self;
  lvWatches.Align := alClient;

  lvWatches.TreeOptions.MiscOptions := lvWatches.TreeOptions.MiscOptions +
    [toCheckSupport];

  lvWatches.TreeOptions.PaintOptions := lvWatches.TreeOptions.PaintOptions +
    [toShowHorzGridLines];

  lvWatches.TreeOptions.SelectionOptions := lvWatches.TreeOptions.SelectionOptions +
    [toFullRowSelect, toMultiSelect];

  lvWatches.Header.Options := lvWatches.Header.Options + [hoVisible];

  lvWatches.OnInitNode := VSTInitNode;
  lvWatches.OnFreeNode := VSTFreeNode;
  lvWatches.OnGetText := VSTGetText;
  lvWatches.OnChecked := VSTNodeChecked;
  lvWatches.OnKeyDown := lvWatchesKeyDown;
  lvWatches.OnPaintText := VSTPaintText;
  lvWatches.OnDblClick := lvWatchesDblClick;

  Column := lvWatches.Header.Columns.Add;
  Column.Text := 'Expression';
  Column.Width := 150;

  Column := lvWatches.Header.Columns.Add;
  Column.Text := 'Value';
  Column.Width := 200;
  // init tree --------------------------------

  FWatchesFileName := IOGet.ExpandPath('%local%\watches.txt');
  FWatches := TWatches.Create;
end;

destructor TFrameWatches.Destroy;
begin
  SaveWatchesToFile;
  FWatches.Free;
  inherited;
end;

procedure TFrameWatches.VSTInitNode(Sender: TBaseVirtualTree; ParentNode,
  Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
begin
  Node.CheckType := ctCheckBox;
  if wf_disabled in FWatches[Node.Index].Flags then
    Node.CheckState := csUncheckedNormal
  else
    Node.CheckState := csCheckedNormal;
end;

procedure TFrameWatches.VSTFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  WatchCount: int64;
begin
  WatchCount := FWatches.Count;
  if Node.Index < WatchCount then
    FWatches.Delete(Node.Index);
end;

procedure TFrameWatches.VSTNodeChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Item: TWatchItem;
begin
  Item := FWatches[Node.Index];
  case Node.CheckState of
    csCheckedNormal:
      exclude(Item.Flags, wf_disabled);
    csUncheckedNormal:
      include(Item.Flags, wf_disabled);
  end;
  FWatches[Node.Index] := Item;
  UpdateWatch(Node.Index);
end;

procedure TFrameWatches.VSTGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
  Column: TColumnIndex; TextType: TVSTTextType; var Text: UnicodeString);
begin
  case Column of
    0:
      Text := FWatches[Node.Index].Expr;
    1:
      Text := FWatches[Node.Index].Value;
  end;
end;

procedure TFrameWatches.VSTPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
begin
  if wf_disabled in FWatches[Node.Index].Flags then
  begin
    TargetCanvas.Font.Color := clLtGray;
    exit;
  end;

  if wf_changed in FWatches[Node.Index].Flags then
  begin
    TargetCanvas.Font.Color := clRed;
    TargetCanvas.Font.Style := [fsBold];
    exit;
  end;
end;

procedure TFrameWatches.DeleteSelected;
begin
  lvWatches.DeleteSelectedNodes;
end;

procedure TFrameWatches.ReplaceWatch;
begin
  if Assigned(lvWatches.FocusedNode) then
    InsertWatch(True);
end;

procedure TFrameWatches.InsertWatch(ReplaceSelected: Boolean);
var
  WatchName, OldWatchName: string;
  w: TWatchItem;
  CheckState: TCheckState;
begin
  WatchName := '';

  CheckState := csCheckedNormal;

  if ReplaceSelected and Assigned(lvWatches.FocusedNode) then
  begin
    WatchName := FWatches[lvWatches.FocusedNode.Index].Expr;
    CheckState := lvWatches.FocusedNode.CheckState;
  end;

  OldWatchName := WatchName;
  if not InputQuery(SWatches, SWatchExpr, WatchName) then
    exit;

  if WatchName = '' then
    exit;

  if WatchName <> OldWatchName then
  begin
    w := TWatchItem.Create(WatchName, ''); // new expression, always checked
    CheckState := csCheckedNormal;
  end
  else
    w := FWatches[lvWatches.FocusedNode.Index]; // same expression

  if not Assigned(lvWatches.FocusedNode) then
  begin
    // If no focused node then append.
    FWatches.Add(w);
  end
  else
  begin
    if ReplaceSelected then
    begin
      FWatches[lvWatches.FocusedNode.Index] := w;
      lvWatches.FocusedNode.CheckState := CheckState;
    end
    else
      FWatches.Insert(lvWatches.FocusedNode.Index, w);
  end;

  FWatchesWereChanged := True;
  UpdateWatches;
end;

procedure TFrameWatches.lvWatchesDblClick(Sender: TObject);
begin
  ReplaceWatch;
end;

procedure TFrameWatches.lvWatchesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkReturn:
      ReplaceWatch;
    vkInsert:
      InsertWatch(false);
    vkDelete:
      DeleteSelected;
  end;
end;

procedure TFrameWatches.UpdateWatch(Index: integer; ModificationOption: TWatchProcessingOption);
var
  w: TWatchItem;
  Value: IVDConstExpression;
begin
  w := FWatches[Index];

  if wf_disabled in w.Flags then
    w.Value := SWatchDisabled
  else
    if not CoreGet.EvaluateExpr(BSTR_IN(w.Expr), Value) then
    w.Value := '?'
  else
    // todo: hex prefix is hardcoded here
    w.Value := Format('0x%x', [Value.GetUInt]);

  case ModificationOption of
    wpo_check_modification:
      begin
        if w.Value <> w.PrevValue then
          include(w.Flags, wf_changed)
        else
          exclude(w.Flags, wf_changed);
        w.PrevValue := w.Value;
      end;
    wpo_clear_changed_flag:
      begin
        exclude(w.Flags, wf_changed);
      end;
  end;

  FWatches[Index] := w;
end;

procedure TFrameWatches.UpdateWatches(ModificationOption: TWatchProcessingOption);
var
  i: integer;
begin
  if not FWatchesWereLoaded then
    LoadWatchesFromFile;

  if ModificationOption = wpo_null then
    if not FWatchesWereChanged then
      exit;

  FWatchesWereChanged := false;

  for i := 0 to FWatches.Count - 1 do
    UpdateWatch(i, ModificationOption);

  lvWatches.RootNodeCount := FWatches.Count;
  lvWatches.Invalidate;
end;

{ TWatches }

procedure TWatches.LoadFromFile(const FileName: string);
var
  sl: TStringList;
  s, Expr: string;
  parts: TArray<string>;
  Item: TWatchItem;
  a, b: integer;
begin
  Clear;
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    for s in sl do
    begin
      if s.IsEmpty then
        Continue;

      // expression
      a := s.IndexOf('"');
      b := s.LastIndexOf('"');
      if (a = -1) or (b = -1) then
        raise Exception.Create('Invalid format of watches file');
      Expr := s.Substring(a + 1, b - a - 1);

      // Parts
      parts := s.Substring(b + 1).Split([',']);

      // Create item
      Item := TWatchItem.Create(Expr, '');

      // check Disabled
      if (high(parts) >= 1) and (not parts[1].IsEmpty) then
        include(Item.Flags, wf_disabled);

      Add(Item);
    end;
  finally
    sl.Free;
  end;
end;

procedure TWatches.SaveToFile(const FileName: string);
var
  sl: TStringList;
  w: TWatchItem;
  disabled: string;
begin
  sl := TStringList.Create;
  try
    for w in self do
    begin
      if wf_disabled in w.Flags then
        disabled := 'disabled'
      else
        disabled := '';
      sl.Add(Format('"%s",%s', [w.Expr, disabled]));
    end;
    sl.SaveToFile(FileName);
  finally
    sl.Free;
  end;
end;

{ TWatchItem }

constructor TWatchItem.Create(const Expr, PrevValue: string);
begin
  self.Expr := Expr;
  self.PrevValue := PrevValue;
  self.PrevValue := '';
  self.Flags := [];
end;

end.
