unit uFrameSections;

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
  TFrameSections = class(TFrameBaseListView)
    N4: TMenuItem;
    Savesectiontofile1: TMenuItem;
    procedure Savesectiontofile1Click(Sender: TObject);
  protected
    function GetComparisionLess(ColumnId: Integer): TFrameBaseListView.TComparisionLess; override;
  public
    function ReloadItems: Integer; override;
    procedure ObjToItem(const Obj: TObject; const Item: TListItem); override;
    procedure ItemTriggered(Index: Integer); override;
  end;

var
  FrameSections: TFrameSections;

implementation

{$R *.dfm}


const
  COL_ID    = 0;
  COL_NAME  = 1;
  COL_VA    = 2;
  COL_ENDVA = 3;
  COL_SIZE  = 4;
  COL_FLAGS = 5;

type
  TDataItem = class(TVADataItem)
    endva: TVA;
    id: TVDSectionId;
    size: TVDSectionSize;
    name, flags: string;
  end;

  { TFrameSections }

function TFrameSections.GetComparisionLess(
  ColumnId: Integer): TFrameBaseListView.TComparisionLess;
begin
  result := nil;
  case ColumnId of
    COL_ID:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).id < TDataItem(b).id;
          end;
      end;
    COL_NAME:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).name.ToLower, TDataItem(b).name.ToLower) < 0;
          end;
      end;
    COL_VA:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).va < TDataItem(b).va;
          end;
      end;
    COL_ENDVA:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).endva < TDataItem(b).endva;
          end;
      end;
    COL_SIZE:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := TDataItem(a).size < TDataItem(b).size;
          end;
      end;
    COL_FLAGS:
      begin
        result := function(const a, b: TObject): boolean
          begin
            result := CompareStr(TDataItem(a).name.ToLower, TDataItem(b).name.ToLower) < 0;
          end;
      end;
  end;
end;

procedure TFrameSections.ItemTriggered(Index: Integer);
begin
  TryGoToCurItemVA;
end;

procedure TFrameSections.ObjToItem(const Obj: TObject; const Item: TListItem);
begin
  // id, name, va, end va, size, flags
  Item.Caption := IntToStr(TDataItem(Obj).id);
  Item.SubItems.Add(TDataItem(Obj).name);
  Item.SubItems.Add(format('0x%x', [TDataItem(Obj).va]));
  Item.SubItems.Add(format('0x%x', [TDataItem(Obj).endva]));
  Item.SubItems.Add(format('0x%x', [TDataItem(Obj).size]));
  Item.SubItems.Add(TDataItem(Obj).flags);
end;

function EnumFunc(Section: IVDSection; ud: Pointer): BOOL; stdcall;
var
  rec: TDataItem;
begin
  rec := TDataItem.Create;

  rec.id := Section.GetId;
  rec.name := Section.GetName;
  rec.va := Section.GetStartVA;
  rec.endva := Section.GetEndVA;
  rec.size := rec.endva - rec.va;
  rec.flags := Section.GetFlagsAsString;

  TFrameSections(ud).FItems.Add(rec);
  exit(True);
end;

function TFrameSections.ReloadItems: Integer;
var
  c: IVDCore;
begin
  inherited;
  c := CoreGet();
  if Assigned(c) and (c.IsDatabaseOpened) then
    c.VM.Sections.Enumerate(EnumFunc, self);
  result := FItems.Count;
end;

procedure TFrameSections.Savesectiontofile1Click(Sender: TObject);
var
  c: IVDCore;
  rec: TDataItem;
  sec: IVDSection;
  sd: TSaveDialog;
begin
  inherited;
  if Assigned(FListView.Selected) then
  begin
    c := CoreGet;
    rec := TDataItem(FItems[FListView.Selected.Index]);
    sec := c.VM.Sections.FindByID(rec.id);
    if Assigned(sec) then
    begin
      sd := TSaveDialog.Create(nil);
      try
        sd.FileName := format('s_at_0x%x_size_0x%x', [sec.GetStartVA, sec.GetSize]);
        if sd.Execute then
        begin
          c.VM.Sections.SaveToFile(sec, BSTR_IN(sd.FileName));
        end;
      finally
        sd.Free;
      end;
    end;
  end;
end;

end.
