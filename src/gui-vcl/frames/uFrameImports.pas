unit uFrameImports;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Contnrs,

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
  TFrameImports = class(TFrameBaseListView)
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    function ReloadItems: Integer; override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    procedure ItemTriggered(Index: Integer); override;
  end;

var
  FrameImports: TFrameImports;

implementation

{$R *.dfm}


const
  COL_VA   = 0;
  COL_ORD  = 1;
  COL_NAME = 2;
  COL_LIB  = 3;

  BAD_ORD = 0;

type
  TDataItem = class(TVADataItem)
  public
    Ord: TVDSymbolOrdinal;
    Sym: string;
    Lib: string;
    constructor Create(VA: TVA; Ord: TVDSymbolOrdinal; const Sym, Lib: string);
  end;

  { TFrameImports }

function TFrameImports.GetComparisionLess(
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
    COL_ORD:
      begin
        result := function(const a, b: TObject): boolean
          begin
            if (TDataItem(a).Ord = BAD_ORD) and (TDataItem(b).Ord = BAD_ORD) then
              result := CompareStr(TDataItem(a).Sym, TDataItem(b).Sym) < 0
            else
              result := TDataItem(a).Ord < TDataItem(b).Ord;
          end;
      end;
    COL_NAME:
      begin
        result := function(const a, b: TObject): boolean
          begin
            if (TDataItem(a).Sym = '') and (TDataItem(b).Sym = '') then
              result := TDataItem(a).Ord < TDataItem(b).Ord
            else
              result := CompareStr(TDataItem(a).Sym, TDataItem(b).Sym) < 0;
          end;
      end;
    COL_LIB:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).Lib, TDataItem(b).Lib) < 0;
          end;
      end;
  end;
end;

procedure TFrameImports.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameImports.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  Item.Caption := Format('0x%x', [TDataItem(Obj).VA]);
  if TDataItem(Obj).Ord <> 0 then
    Item.SubItems.Add(Format('%d', [TDataItem(Obj).Ord]))
  else
    Item.SubItems.Add('');
  Item.SubItems.Add(TDataItem(Obj).Sym);
  Item.SubItems.Add(TDataItem(Obj).Lib);
end;

function EnumFunc(VA: TVA; LibStr, SymStr: BSTR_IN; SymOrd: TVDSymbolOrdinal; ud: Pointer): BOOL; stdcall;
begin
  TFrameBaseListView(ud).FItems.Add(TDataItem.Create(VA, SymOrd, SymStr, LibStr));
  Exit(True);
end;

function TFrameImports.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.ImportSymbols.EnumSyms(BAD_VA, BAD_VA, EnumFunc, self);
  result := FItems.Count;
end;

{ TDataItem }

constructor TDataItem.Create(VA: TVA; Ord: TVDSymbolOrdinal; const Sym,
  Lib: string);
begin
  self.VA := VA;
  self.Ord := Ord;
  self.Sym := Sym;
  self.Lib := Lib;
end;

end.
