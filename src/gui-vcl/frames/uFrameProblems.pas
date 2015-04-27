unit uFrameProblems;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  uFrameBaseListView,

  VDAPI;

const
  ProblemStrings: array [0 .. TProblemKinds.PROBLEM_LAST] of string =
    (
    '',
    'basic decode failed',
    'unknown branch target address',
    'last'
    );

type
  TFrameProblems = class(TFrameBaseListView)
  private
    procedure TryDelCurItem;
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    procedure ItemTriggered(Index: Integer); override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    function ReloadItems: Integer; override;
  published
    procedure FListViewKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); override;
  end;

var
  FrameProblems: TFrameProblems;

implementation

{$R *.dfm}


const
  COL_VA   = 0;
  COL_KIND = 1;

type
  TDataItem = class(TVADataItem)
    Kind: TProblemKind;
    constructor Create(VA: TVA; Kind: TProblemKind);
  end;

  { TFrameProblems }

procedure TFrameProblems.FListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkDelete:
      begin
        TryDelCurItem;
        exit;
      end;
  end;

  inherited;
end;

function TFrameProblems.GetComparisionLess(
  ColumnId: Integer): TFrameBaseListView.TComparisionLess;
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
    COL_KIND:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).Kind < TDataItem(b).Kind;
          end;
      end;
  end;
end;

procedure TFrameProblems.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameProblems.ObjToItem(const Obj: TObject; const Item: TListItem);
const
  Flags = TVDDecodeToTextFlag.Body;
var
  VA: TVA;
  c: IVDCore;
  Layout: IVDVATextLayout;
begin
  VA := TDataItem(Obj).VA;

  Item.Caption := Format('0x%x', [VA]);
  Item.SubItems.Add(ProblemStrings[TDataItem(Obj).Kind]);

  c := CoreGet();
  if Assigned(c) then
  begin
    Layout := CreateVATextLayout(TVDTextFlag.Plain);
    c.Decoder.SetHexDumpByteCount(0);
    c.Decoder.DecodeToText(VA, Layout, Flags);
    Item.SubItems.Add(Layout.Get);
  end;
end;

function EnumFunc(
  VA: TVA;
  Kind: TProblemKind;
  ud: Pointer): BOOL; stdcall;
begin
  TFrameProblems(ud).FItems.Add(TDataItem.Create(VA, Kind));
  exit(True);
end;

function TFrameProblems.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.Problems.Enumerate(BAD_VA, BAD_VA, EnumFunc, self);
  result := FItems.Count;
end;

procedure TFrameProblems.TryDelCurItem;
var
  Item: TDataItem;
begin
  if FListView.ItemIndex <> -1 then
  begin
    Item := TDataItem(FItems[FListView.ItemIndex]);
    CoreGet.Problems.Delete(Item.VA, Item.Kind);
  end;
end;

{ TDataItem }

constructor TDataItem.Create(VA: TVA; Kind: TProblemKind);
begin
  inherited Create;
  self.VA := VA;
  self.Kind := Kind;
end;

end.
