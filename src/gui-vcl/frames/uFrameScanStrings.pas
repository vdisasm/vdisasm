unit uFrameScanStrings;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFrameBaseListView, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.ComCtrls;

const
  DEFAULT_MIN_STRING_LEN = 5;

type
  TFrameScanStrings = class(TFrameBaseListView)
    N4: TMenuItem;
    Defineselecteditemsastrings1: TMenuItem;
    procedure PopupMenu1Popup(Sender: TObject);
    procedure Defineselecteditemsastrings1Click(Sender: TObject);
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    MinLen: Integer;
    constructor Create(AOwner: TComponent); override;

    function ReloadItems: Integer; override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    procedure ItemTriggered(Index: Integer); override;
  end;

  // var
  // FrameScanStrings: TFrameScanStrings;

implementation

uses
  VDAPI;

{$R *.dfm}


const
  COL_VA   = 0;
  COL_TEXT = 1;

type
  TDataItem = class(TVADataItem)
  public
    // Hold VA, Size and CodePage because it can be helpful if user wants to
    // define found string(s) in database, i.e. covert these ranges.
    Size: int;
    CodePage: VDAPI.TCodePage;

    // Text is defined here for fast search.
    // When item requested we just return this Text instead of doing VM reading.
    Text: string;

    constructor Create(VA: TVA; Size: int; CodePage: VDAPI.TCodePage; const Text: string);
  end;

  { TFrameScanStrings }

constructor TFrameScanStrings.Create(AOwner: TComponent);
begin
  inherited;
  Self.MinLen := DEFAULT_MIN_STRING_LEN;
end;

procedure TFrameScanStrings.Defineselecteditemsastrings1Click(Sender: TObject);
var
  c: IVDCore;
  i, left: Integer;
  Item: TDataItem;
  li: TListItem;
begin
  inherited;

  left := FListView.SelCount;

  case left of
    0:
      ;
    1:
      begin
        c := CoreGet();
        li := FListView.Selected;
        Item := TDataItem(FItems[li.Index]);
        c.Strings.Define(Item.VA, Item.Size, Item.CodePage);
        c.UI.RepaintView(TUIViewType.UIVT_DISASM);
      end
  else
    c := CoreGet();

    i := 0;
    while left <> 0 do
    begin
      li := FListView.Items[i];
      if li.Selected then
      begin
        Item := TDataItem(FItems[i]);
        c.Strings.Define(Item.VA, Item.Size, Item.CodePage);
        dec(left);
      end;
      inc(i);
    end;

    c.UI.RepaintView(TUIViewType.UIVT_DISASM);
  end;
end;

function TFrameScanStrings.GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess;
begin
  result := nil;
  case ColumnId of
    COL_VA:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).VA < TDataItem(b).VA;
          end;
      end;
    COL_TEXT:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).Text, TDataItem(b).Text) < 0;
          end;
      end;
  end;
end;

procedure TFrameScanStrings.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameScanStrings.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  Item.Caption := Format('0x%x', [TDataItem(Obj).VA]);
  Item.SubItems.Add(TDataItem(Obj).Text);
end;

procedure TFrameScanStrings.PopupMenu1Popup(Sender: TObject);
begin
  inherited;
  Defineselecteditemsastrings1.Enabled := Assigned(FListView.Selected);
end;

function EnumFunc(VA: TVA; Size: int; CodePage: TCodePage; ud: pointer): BOOL; stdcall;
var
  StrText: BSTR;
begin
  if CoreGet.Strings.ReadString(VA, Size, CodePage, StrText) then
    StrText := Trim(StrText)
  else
    StrText := '';
  TFrameScanStrings(ud).FItems.Add(TDataItem.Create(VA, Size, CodePage, StrText));
  exit(True);
end;

function TFrameScanStrings.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.Strings.ScanForStrings(BAD_VA, BAD_VA, Self.MinLen, EnumFunc, Self);
  result := FItems.Count;
end;

{ TDataItem }

constructor TDataItem.Create(VA: TVA; Size: int; CodePage: VDAPI.TCodePage; const Text: string);
begin
  Self.VA := VA;
  Self.Size := Size;
  Self.CodePage := CodePage;
  Self.Text := Text;
end;

end.
