unit uFrameRefs;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.Menus,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,

  uFrameBaseListView,

  uListenerMain,

  VDAPI;

type
  TFrameRefs = class(TFrameBaseListView)
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    function ReloadItems: Integer; override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    procedure ItemTriggered(Index: Integer); override;
  end;

var
  FrameRefs: TFrameRefs;

implementation

{$R *.dfm}


const
  COL_VA   = 0;
  COL_NAME = 1;

type
  TDataItem = class(TVADataItem)
  public
    Text: string;
    constructor Create(VA: TVA; const Text: string);
  end;

  { TDataItem }

constructor TDataItem.Create(VA: TVA; const Text: string);
begin
  inherited Create();
  self.VA := VA;
  self.Text := Text;
end;

{ TFrameRefs }

function TFrameRefs.GetComparisionLess(
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
    COL_NAME:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).Text.ToLower, TDataItem(b).Text.ToLower) < 0;
          end;
      end;
  end;
end;

procedure TFrameRefs.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameRefs.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  Item.Caption := Format('0x%x', [TDataItem(Obj).VA]);
  Item.SubItems.Add(TDataItem(Obj).Text);
end;

function it(const a: boolean; const ATrue, AFalse: string): string; inline;
begin
  if a then
    result := ATrue
  else
    result := AFalse;
end;

function EnumRefs(VA: TVA; Kind: TVDReferenceKind; ud: Pointer): BOOL; stdcall;
const
  Flags: TVDDecodeToTextFlags = TVDDecodeToTextFlag.Body;
var
  Text: IVDVATextLayout;
  decoder: IVDDecoder;
  TmpVA: TVA;
begin
  decoder := CoreGet.decoder;
  decoder.SetHexDumpByteCount(0); // don't need hex dump
  Text := CreateVATextLayout(TVDTextFlag.Plain);
  TmpVA := VA;
  TFrameRefs(ud).FItems.Add(TDataItem.Create(
    { va } TmpVA,
    { text } it(decoder.DecodeToText(VA, Text, Flags) <> 0, Text.Get, '')
    ));
  exit(True);
end;

function TFrameRefs.ReloadItems: Integer;
var
  c: IVDCore;
  RefVA: TVA;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
  begin
    RefVA := c.GetUserVA;
    c.Refs.Enumerate(RefVA, EnumRefs, self);
  end;
  result := FItems.Count;
end;

end.
