unit uFrameExports;

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

  VDAPI;

type
  TFrameExports = class(TFrameBaseListView)
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    function ReloadItems: Integer; override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    procedure ItemTriggered(Index: Integer); override;
  end;

//var
//  FrameExports: TFrameExports;

implementation

{$R *.dfm}


const
  COL_VA   = 0;
  COL_ORD  = 1;
  COL_NAME = 2;

type
  TDataItem = class(TVADataItem)
  public
    Ordinal: TVDSymbolOrdinal;
    SymbolName: string;
    constructor Create(VA: TVA; Ordinal: TVDSymbolOrdinal; const SymbolName: string);
  end;

  { TFrameExports }

function EnumFunc(
  VA: TVA;
  SymbolName: BSTR_IN;
  Ordinal: TVDSymbolOrdinal;
  ud: Pointer): BOOL; stdcall;
begin
  TFrameExports(ud).FItems.Add(TDataItem.Create(VA, Ordinal, SymbolName));
  exit(True);
end;

function TFrameExports.GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess;
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
    COL_ORD:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).Ordinal < TDataItem(b).Ordinal;
          end;
      end;
    COL_NAME:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).SymbolName.ToLower, TDataItem(b).SymbolName.ToLower) < 0;
          end;
      end;
  end;
end;

procedure TFrameExports.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameExports.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  Item.Caption := Format('0x%x', [TDataItem(Obj).VA]);
  Item.SubItems.Add(Format('%d', [TDataItem(Obj).Ordinal]));
  Item.SubItems.Add(TDataItem(Obj).SymbolName);
end;

function TFrameExports.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.ExportSymbols.Enumerate(BAD_VA, BAD_VA, EnumFunc, self);
  result := FItems.Count;
end;

{ TDataItem }

constructor TDataItem.Create(VA: TVA; Ordinal: TVDSymbolOrdinal;
  const SymbolName: string);
begin
  self.VA := VA;
  self.Ordinal := Ordinal;
  self.SymbolName := SymbolName;
end;

end.
