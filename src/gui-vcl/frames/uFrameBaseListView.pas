{
  todo: BaseListView: timer seems to be redundant here as async update is called
  by external timer.
}
unit uFrameBaseListView;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,
  System.Generics.Defaults,
  System.Generics.Collections,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.ComCtrls,
  Vcl.Menus,
  Vcl.ExtCtrls,
  Vcl.Clipbrd,
  Vcl.StdCtrls,

  uUiUtils,

  uCore.Strings,

  vdBufferedStream,

  VDAPI;

type
  TLVItems = TObjectList<TObject>;

  TVADataItem = class
  public
    VA: TVA;
  end;

  TFrameBaseListView = class(TFrame)
    FListView: TListView;
    PopupMenu1: TPopupMenu;
    MenuFind: TMenuItem;
    MenuFindNext: TMenuItem;
    Update1: TMenuItem;
    N1: TMenuItem;
    StatusBar1: TStatusBar;
    TimerUpdateAsync: TTimer;
    N2: TMenuItem;
    Exporttotextfile1: TMenuItem;
    SaveDialog1: TSaveDialog;
    SelectAll1: TMenuItem;
    N3: TMenuItem;
    Copyselectiontobuffer1: TMenuItem;
    procedure FListViewData(Sender: TObject; Item: TListItem);
    procedure Update1Click(Sender: TObject);
    procedure FListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState); virtual;
    procedure FListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FrameEnter(Sender: TObject);
    procedure FListViewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FListViewDblClick(Sender: TObject);
    procedure MenuFindClick(Sender: TObject);
    procedure MenuFindNextClick(Sender: TObject);
    procedure TimerUpdateAsyncTimer(Sender: TObject);
    procedure Exporttotextfile1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
    procedure Copyselectiontobuffer1Click(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
  public type
    TComparisionLess = reference to function(const a, b: TObject): boolean;
  private
    FSearchString: string;
    FHighlightPanel0: boolean;
    procedure DoItemTriggered;
  protected
    function GetComparisionLess(ColumnId: Integer): TComparisionLess; virtual; abstract;
    procedure UpdateStatusBar;
  protected
    FNeedUpdate: boolean;
  public
    FItems: TLVItems;

    // Reload items and return count.
    function ReloadItems: Integer; virtual;

    // Build text of item by data stored in object (TDataItem).
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); virtual;

    // Set FNeedUpdate to True.
    // External timer calls UpdateUISync when it wants to update.
    // If FNeedUpdate is false actual update is skipped (no changes).
    procedure UpdateUIAsync;

    // Reload items, update status bar.
    // Executed if FNeedUpdate is True or if it's forced.
    procedure UpdateUISync(Force: boolean = False);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    // Search started from Index variable.
    // If result is True, new item found and Index is changed.
    function FindItemIndexByText(const Text: string; out Index: Integer): boolean;

    procedure ItemTriggered(Index: Integer); virtual;

    // If item is child of TVADataItem go to VA.
    function TryGoToCurItemVA: boolean;

    procedure ExportSelectedItemsToTextStream(txt: TBufferedTextStream);

    // FListView.ClearSelectionFast is very slow (specially when there are lot
    // of items) because it change state of each item in cycle.
    procedure ClearSelectionFast;
    procedure SelectAllFast;

    procedure DisplayItemFoundStatus(Found: boolean; const SearchText: string = '');
  end;

implementation

uses
  Winapi.CommCtrl;

{$R *.dfm}

{ TFrameBaseListView }

constructor TFrameBaseListView.Create(AOwner: TComponent);
begin
  inherited;
  FItems := TLVItems.Create;
end;

destructor TFrameBaseListView.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TFrameBaseListView.DisplayItemFoundStatus(Found: boolean; const SearchText: string);
begin
  if not Found then
  begin
    FHighlightPanel0 := True;
    StatusBar1.Panels[0].Text := Format(SCantFindTextFmt, [SearchText]);
  end;
  StatusBar1.Invalidate; // in any case
end;

procedure TFrameBaseListView.TimerUpdateAsyncTimer(Sender: TObject);
begin
  if FNeedUpdate then
    UpdateUISync;
end;

function TFrameBaseListView.TryGoToCurItemVA: boolean;
var
  VA: TVA;
  Obj: TObject;
begin
  if Assigned(FListView.Selected) then
  begin
    Obj := FItems[FListView.Selected.Index];
    if Obj is TVADataItem then
    begin
      VA := TVADataItem(Obj).VA;
      CoreGet().ChangeVA(VA, True);
      Exit(True);
    end;
  end;
  Exit(False);
end;

procedure TFrameBaseListView.DoItemTriggered;
var
  i: Integer;
begin
  i := FListView.ItemIndex;
  if i <> -1 then
    ItemTriggered(i);
end;

procedure TFrameBaseListView.ExportSelectedItemsToTextStream(
  txt: TBufferedTextStream);
var
  c: IVDCore;
  nItem, nSubItem: Integer;
  Item: TListItem;
  highCol: Integer;
begin
  if (FListView.Items.Count = 0) or (FListView.SelCount = 0) then
    Exit;

  c := CoreGet();

  // Disable update while saving.
  c.UI.SetGuiRefreshState(False);
  try
    for nItem := 0 to FListView.Items.Count - 1 do
    begin
      Item := FListView.Items[nItem];
      if Item.Selected then
      begin
        highCol := Item.SubItems.Count - 1;

        txt.WriteChars(Item.Caption);

        for nSubItem := 0 to highCol do
          txt.WriteChars('|' + Item.SubItems[nSubItem]);

        txt.WriteLn();
      end;
    end;
  finally
    c.UI.SetGuiRefreshState(True);
  end;
end;

procedure TFrameBaseListView.Exporttotextfile1Click(Sender: TObject);
var
  txt: TBufferedTextStream;
begin
  if (FListView.Items.Count = 0) or (FListView.SelCount = 0) then
    Exit;

  if not SaveDialog1.Execute then
    Exit;

  txt := TBufferedTextFileStream.Create(SaveDialog1.FileName);
  try
    ExportSelectedItemsToTextStream(txt);
  finally
    txt.Free;
  end;
end;

procedure TFrameBaseListView.ClearSelectionFast;
begin
  // -1: means for all items
  // 0: means state is false
  // LVIS_SELECTED is field to change
  ListView_SetItemState(FListView.Handle, -1, 0, LVIS_SELECTED);
end;

procedure TFrameBaseListView.SelectAllFast;
begin
  ListView_SetItemState(FListView.Handle, -1, LVIS_SELECTED, LVIS_SELECTED);
end;

procedure TFrameBaseListView.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
var
  cv: TCanvas;
begin
  case Panel.Index of
    0:
      begin
        cv := StatusBar.Canvas;
        if FHighlightPanel0 then
        begin
          cv.Font.Color := clRed;
          FHighlightPanel0 := False;
        end;
        cv.TextOut(Rect.Left, Rect.Top, Panel.Text);
      end;
  end;
end;

procedure TFrameBaseListView.Copyselectiontobuffer1Click(Sender: TObject);
var
  txt: TBufferedTextMemoryStream;
  clp: Vcl.Clipbrd.TClipboard;
begin
  txt := TBufferedTextMemoryStream.Create;
  try
    txt.SkipBOM := True;

    ExportSelectedItemsToTextStream(txt);
    txt.Flush; // to copy buffer into stream

    clp := TClipboard.Create;
    try
      clp.AsText := txt.AsString;
    finally
      clp.Free;
    end;
  finally
    txt.Free;
  end;
end;

procedure TFrameBaseListView.FListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  UpdateStatusBar;
end;

procedure TFrameBaseListView.FListViewColumnClick(Sender: TObject; Column: TListColumn);
const
  TAG_INVERT_SORT = 1 shl 0;
var
  bInvert: boolean;
  FCur: TObject;
  less: TComparisionLess;
  cmp: IComparer<TObject>;
  FoundIndex: Integer;
begin
  // Get comparer "less" for column.
  less := GetComparisionLess(Column.Index);

  // If no comparer, no reason to continue.
  if not Assigned(less) then
    Exit;

  Column.Tag := Column.Tag xor TAG_INVERT_SORT;
  bInvert := (Column.Tag and TAG_INVERT_SORT) <> 0;

  if FListView.Selected <> nil then
    FCur := FItems[FListView.Selected.Index]
  else
    FCur := nil;

  cmp := TComparer<TObject>.Construct(
    function(const a, b: TObject): Integer
    begin
      if less(a, b) then
        Result := -1
      else if less(b, a) then
        Result := 1
      else
        Result := 0;
      if bInvert then
        Result := -Result;
    end);

  FItems.Sort(cmp);

  if FListView.Selected <> nil then
  begin
    if FItems.BinarySearch(FCur, FoundIndex, cmp) then
    begin
      FListView.ItemIndex := FoundIndex;
      FListView.Selected.MakeVisible(False);
    end;
  end;

  FListView.Invalidate;
end;

procedure TFrameBaseListView.FListViewData(Sender: TObject; Item: TListItem);
var
  idx: Integer;
  Obj: TObject;
begin
  idx := Item.Index;
  if (idx >= 0) and (idx < FItems.Count) then
  begin
    Obj := FItems[Item.Index];
    ObjToItem(Obj, Item);
  end;
end;

procedure TFrameBaseListView.FListViewDblClick(Sender: TObject);
begin
  DoItemTriggered;
end;

procedure TFrameBaseListView.FListViewKeyDown(Sender: TObject; var Key: Word;
Shift: TShiftState);
begin
  case Key of
    vkF5:
      Update1Click(nil);
    vkReturn:
      DoItemTriggered;
  end;
end;

procedure TFrameBaseListView.FrameEnter(Sender: TObject);
begin
  // UpdateUI;
end;

procedure TFrameBaseListView.ItemTriggered(Index: Integer);
begin
  // called by DoItemTriggered
end;

procedure TFrameBaseListView.ObjToItem(const Obj: TObject;
const Item: TListItem);
begin

end;

function TFrameBaseListView.ReloadItems: Integer;
// var
// c: IVDCore;
begin
{$IFDEF DIAG_LISTVIEWS_RELOADITEMS}
  Writeln('ReloadItems of ', ClassName);
{$ENDIF}
  FItems.Clear;
  Result := 0;

  // c := CoreGet();
  // if Assigned(c) then
  // c.Log.WriteLn(SReloadingFrame);
end;

procedure TFrameBaseListView.SelectAll1Click(Sender: TObject);
begin
  SelectAllFast;
end;

procedure TFrameBaseListView.Update1Click(Sender: TObject);
begin
  UpdateUIAsync;
end;

procedure TFrameBaseListView.UpdateStatusBar;
var
  str: string;
begin
  if FListView.ItemIndex <> -1 then
    str := Format('%d/%d', [FListView.ItemIndex, FListView.Items.Count - 1])
  else
    str := Format('%d items', [FListView.Items.Count]);

  if FListView.SelCount <> 0 then
    str := Format('%s (%d selected)', [str, FListView.SelCount]);

  StatusBar1.Panels[0].Text := str;
end;

procedure TFrameBaseListView.UpdateUIAsync;
begin
  FNeedUpdate := True;
end;

procedure TFrameBaseListView.UpdateUISync(Force: boolean);
begin
  if FNeedUpdate or Force then
  begin
    FNeedUpdate := False;
    FListView.Items.Count := ReloadItems;
    UpdateStatusBar;
    FListView.Repaint;
  end;
end;

function TFrameBaseListView.FindItemIndexByText(const Text: string;
out Index: Integer): boolean;
var
  i, col: Integer;
  li: TListItem;
  vrf, src: string;
begin
  Result := False;
  if FListView.Items.Count = 0 then
    Exit;
  Index := FListView.ItemIndex;
  if Index < 0 then
    Index := 0;
  vrf := Text.ToLower;
  for i := Index + 1 to FListView.Items.Count - 1 do
  begin
    li := FListView.Items[i];
    for col := 0 to li.SubItems.Count do
    begin
      if col = 0 then
        src := li.Caption.ToLower
      else
        src := li.SubItems[col - 1].ToLower;
      if src.Contains(vrf) then
      begin
        Index := i;
        Exit(True);
      end;
    end;
  end;
end;

procedure TFrameBaseListView.MenuFindClick(Sender: TObject);
begin
  FSearchString := InputBox(SFindText, SText, FSearchString);
  MenuFindNextClick(nil);
end;

procedure TFrameBaseListView.MenuFindNextClick(Sender: TObject);
var
  i: Integer;
  prnt: TWinControl;
begin
  if FSearchString = '' then
    Exit; // nothing to search

  if FindItemIndexByText(FSearchString, i) then
  begin
    ClearSelectionFast;
    FListView.ItemIndex := i;
    FListView.Selected.MakeVisible(False);
    DisplayItemFoundStatus(True);
    Exit;
  end;

  // Not found.
  prnt := self.Parent;
  if Assigned(prnt) then
    FlashWindowAndBeep(prnt.Handle);
  DisplayItemFoundStatus(False, FSearchString);
end;

end.
