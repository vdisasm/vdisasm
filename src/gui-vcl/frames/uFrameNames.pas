unit uFrameNames;

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
  TFrameNames = class(TFrameBaseListView)
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    procedure ItemTriggered(Index: Integer); override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    function ReloadItems: Integer; override;
  end;

var
  FrameNames: TFrameNames;

implementation

{$R *.dfm}


const
  COL_VA   = 0;
  COL_NAME = 1;

type
  TDataItem = class(TVADataItem)
    Text: string;
    constructor Create(VA: TVA; const Text: string);
  end;

  { TFrameNames }

function TFrameNames.GetComparisionLess(
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

procedure TFrameNames.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameNames.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  Item.Caption := Format('0x%x', [TDataItem(Obj).VA]);
  Item.SubItems.Add(TDataItem(Obj).Text);
end;

function EnumNames(VA: TVA; Text: BSTR; Flags: TTextualFlags; ud: Pointer): BOOL; stdcall;
begin
  TFrameNames(ud).FItems.Add(TDataItem.Create(VA, Text));
  exit(True);
end;

function TFrameNames.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.Names.Enumerate(EnumNames, self);
  result := FItems.Count;
end;

{ TDataItem }

constructor TDataItem.Create(VA: TVA; const Text: string);
begin
  self.VA := VA;
  self.Text := Text;
end;

end.
