unit uFrameHex;

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

  System.UITypes,

  VDAPI,

  uHexEdit,
  uColorText;

type
  TFrameHex = class(TFrame)
    StatusBar1: TStatusBar;
    PopupMenu1: TPopupMenu;
    Go1: TMenuItem;
    N1: TMenuItem;
    SaveChanges1: TMenuItem;
    DiscardChanges1: TMenuItem;
    N2: TMenuItem;
    Columns1: TMenuItem;
    N41: TMenuItem;
    N81: TMenuItem;
    N161: TMenuItem;
    N321: TMenuItem;
    FollowinDisassembler1: TMenuItem;
    procedure Go1Click(Sender: TObject);
    procedure SaveChanges1Click(Sender: TObject);
    procedure DiscardChanges1Click(Sender: TObject);
    procedure N41Click(Sender: TObject);
    procedure N81Click(Sender: TObject);
    procedure N161Click(Sender: TObject);
    procedure N321Click(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure FollowinDisassembler1Click(Sender: TObject);
  private
    procedure HexEditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure HexEditVAChanged(Sender: TObject);
    procedure HexEditDblClick(Sender: TObject);
  public
    HexEdit: TVDHexEdit;
    procedure GoVA;
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  uFormTextInput,
  uStrings;

{$R *.dfm}


constructor TFrameHex.Create(AOwner: TComponent);
begin
  inherited;
  HexEdit := TVDHexEdit.Create(self);
  HexEdit.Parent := self;
  HexEdit.Align := alClient;
  HexEdit.OnKeyDown := HexEditKeyDown;
  HexEdit.OnVAChanged := HexEditVAChanged;
  HexEdit.OnDblClick := HexEditDblClick;
  HexEdit.PopupMenu := PopupMenu1;
end;

procedure TFrameHex.DiscardChanges1Click(Sender: TObject);
begin
  HexEdit.DiscardModifications;
end;

procedure TFrameHex.FollowinDisassembler1Click(Sender: TObject);
var
  VA: TVA;
begin
  VA := HexEdit.CursorVA;
  CoreGet().ChangeVA(VA, True);
end;

procedure TFrameHex.Go1Click(Sender: TObject);
begin
  GoVA;
end;

procedure TFrameHex.GoVA;
var
  Str: string;
  VA: TVA;
begin
  if InputText(Str, SEnterVA) then
  begin
    if CoreGet.EvaluateVA(BSTR_IN(Str), VA) then
      HexEdit.NavigateToVA(VA);
  end;
end;

procedure TFrameHex.HexEditDblClick(Sender: TObject);
begin
  CoreGet.ChangeVA(HexEdit.CursorVA, True);
end;

procedure TFrameHex.HexEditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkEscape:
      begin
        if HexEdit.Modified then
          DiscardChanges1Click(nil);
      end;
  end;

  // only in hex
  if HexEdit.Block <> block_hex then
    exit;

  case Key of
    vkG:
      // G in hex
      if Shift = [] then
        GoVA;
  end;
end;

procedure TFrameHex.HexEditVAChanged(Sender: TObject);
var
  CurVA: TVA;
begin
  CurVA := HexEdit.CursorVA;
  if CurVA <> BAD_VA then
  begin
    StatusBar1.Panels[0].Text := Format('%x', [CurVA]);
    StatusBar1.Panels[1].Text := Format('%d, %d', [HexEdit.CaretY, HexEdit.ColumnId]);
  end
  else
  begin
    StatusBar1.Panels[0].Text := '';
    StatusBar1.Panels[1].Text := '';
  end;
end;

procedure TFrameHex.N161Click(Sender: TObject);
begin
  HexEdit.ColumnCount := 16;
end;

procedure TFrameHex.N321Click(Sender: TObject);
begin
  HexEdit.ColumnCount := 32;
end;

procedure TFrameHex.N41Click(Sender: TObject);
begin
  HexEdit.ColumnCount := 4;
end;

procedure TFrameHex.N81Click(Sender: TObject);
begin
  HexEdit.ColumnCount := 8;
end;

procedure TFrameHex.PopupMenu1Popup(Sender: TObject);
begin
  SaveChanges1.Visible := HexEdit.Modified;
  DiscardChanges1.Visible := HexEdit.Modified;
end;

procedure TFrameHex.SaveChanges1Click(Sender: TObject);
begin
  HexEdit.SaveModifications;
end;

end.
