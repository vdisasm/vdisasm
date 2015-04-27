unit uFormTypeLibBrowser;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus,

  VDAPI;

type
  TFormTypeLibBrowser = class(TForm)
    lvImp: TListView;
    lvTypes: TListView;
    Splitter1: TSplitter;
    PopupMenu1: TPopupMenu;
    NewType1: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvImpDblClick(Sender: TObject);
    procedure lvTypesChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure lvTypesDblClick(Sender: TObject);
    procedure lvTypesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvImpKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListViewsEnter(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure NewType1Click(Sender: TObject);
  private
    FCurLib: Pointer; // IVDTypeLibrary
    FCurLibName: string;
    FCurTypName: string;
    class function AddNamesToListView(Name: BSTR; ud: Pointer): BOOL; stdcall; static;
  protected
    procedure BrowseRecord(const Rec: IVDRecordType);
  public
    procedure ClearControls;

    // '' is database typelib.
    procedure Reload(const ImportedLibFileName: string);
  end;

var
  FormTypeLibBrowser: TFormTypeLibBrowser;

implementation

uses
  uFormTypeLibBrowserRecord,
  uFormTypeEditor;

{$R *.dfm}
{ TFormTypeLibBrowser }

procedure TFormTypeLibBrowser.BrowseRecord(const Rec: IVDRecordType);
begin
  uFormTypeLibBrowserRecord.DisplayRecord(Rec, 0, Mouse.CursorPos, True);
end;

procedure TFormTypeLibBrowser.ClearControls;
begin
  lvImp.Clear;
  lvTypes.Clear;

  FCurLib := nil;
  FCurLibName := '';
  FCurTypName := '';
end;

class function TFormTypeLibBrowser.AddNamesToListView(Name: BSTR; ud: Pointer): BOOL;
begin
  TListView(ud).Items.Add.Caption := Name;
  Result := True;
end;

procedure TFormTypeLibBrowser.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;

procedure TFormTypeLibBrowser.FormShow(Sender: TObject);
begin
  Reload('');
end;

procedure TFormTypeLibBrowser.lvImpDblClick(Sender: TObject);
var
  sel: TListItem;
begin
  sel := (Sender as TListView).Selected;
  if sel <> nil then
  begin
    if sel.Caption = '..' then
      Reload('')
    else
      Reload(sel.Caption);
  end;
end;

procedure TFormTypeLibBrowser.lvImpKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        (Sender as TListView).OnDblClick(Sender);
      end;
  end;
end;

procedure TFormTypeLibBrowser.lvTypesChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  FCurTypName := Item.Caption;
end;

procedure TFormTypeLibBrowser.lvTypesDblClick(Sender: TObject);
var
  lib: IVDTypeLibrary;
  typ: IVDType;
  c: IVDCore;
  Kind: TVDTypeKind;
begin
  if FCurLib = nil then
    exit;

  c := CoreGet();
  if c = nil then
    exit;

  lib := IVDTypeLibrary(FCurLib);
  typ := lib.FindType(nil, BSTR_IN(FCurTypName), False);

  if typ = nil then
  begin
    c.Log.WriteLn(Format('Type "%s" not found', [FCurTypName]));
    exit;
  end;

  Kind := typ.GetKind;

  case Kind of
    TYPEKIND_RECORD:
      BrowseRecord(typ as IVDRecordType);
  end;

end;

procedure TFormTypeLibBrowser.ListViewsEnter(Sender: TObject);
var
  lv: TListView;
begin
  lv := Sender as TListView;
  if lv.ItemIndex = -1 then
    if lv.Items.Count > 0 then
    begin
      lv.ItemIndex := 0;
    end;
end;

procedure TFormTypeLibBrowser.lvTypesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      begin
        (Sender as TListView).OnDblClick(Sender);
      end;
  end;
end;

procedure TFormTypeLibBrowser.NewType1Click(Sender: TObject);
begin
  uFormTypeEditor.Invoke(CoreGet().TypeLib, '');
end;

procedure TFormTypeLibBrowser.PopupMenu1Popup(Sender: TObject);
var
  isCurLibSelected: boolean;
begin
  isCurLibSelected := FCurLib = Pointer(CoreGet.TypeLib);
  NewType1.Visible := isCurLibSelected;
end;

procedure TFormTypeLibBrowser.Reload(const ImportedLibFileName: string);
var
  c: IVDCore;
  lib: IVDTypeLibrary;
  libName: string;
begin
  ClearControls;

  c := CoreGet();
  if c = nil then
    exit;

  lib := c.TypeLib;

  if ImportedLibFileName <> '' then
  begin
    lib := lib.FindImportedTypeLibByFileName(BSTR_IN(ImportedLibFileName));
  end;

  if lib = nil then
    exit;

  // display lib
  libName := lib.GetName;
  FCurLibName := libName;

  FCurLib := Pointer(lib);

  lvTypes.Columns[0].Caption := Format('Types of "%s"', [libName]);

  lvImp.Items.BeginUpdate;
  lvTypes.Items.BeginUpdate;
  try
    if ImportedLibFileName <> '' then
      lvImp.Items.Add.Caption := '..';

    lib.EnumImportedLibFileNames(self.AddNamesToListView, lvImp);
    lib.EnumTypeNames(self.AddNamesToListView, lvTypes);
  finally
    lvImp.Items.EndUpdate;
    lvTypes.Items.EndUpdate;
  end;

  // update focus
  if lvTypes.Items.Count = 0 then
  begin
    if lvImp.Items.Count <> 0 then
      lvImp.SetFocus;
  end
  else if lvImp.Items.Count = 0 then
  begin
    if lvTypes.Items.Count <> 0 then
      lvTypes.SetFocus;
  end;
end;

end.
