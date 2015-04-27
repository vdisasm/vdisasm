unit uFormDefineString;

interface

uses
  System.UITypes,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  VDAPI;

type
  TStringTypeToDefine = (
    st_none,
    st_use_size,
    st_null_term,
    st_char_term,
    st_len_pfx
    );

  TFormDefineString = class(TForm)
    edVA: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edCodePage: TEdit;
    edCustomChar: TEdit;
    Label3: TLabel;
    Label4: TLabel;
    edLenPfxSize: TEdit;
    Label5: TLabel;
    edSize: TEdit;
    rbNullTerm: TRadioButton;
    rbCharTerm: TRadioButton;
    rbLenPrefixed: TRadioButton;
    btDone: TButton;
    mmPreview: TMemo;
    Label6: TLabel;
    rbUseSize: TRadioButton;
    btUpdatePreview: TButton;
    btCpUtf8: TButton;
    btCpUtf16: TButton;
    btCpReset: TButton;
    procedure rbNullTermClick(Sender: TObject);
    procedure btUpdatePreviewClick(Sender: TObject);
    procedure btDoneClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btCpUtf8Click(Sender: TObject);
    procedure btCpUtf16Click(Sender: TObject);
    procedure btCpResetClick(Sender: TObject);
  private
    FVA: TVA;             // address of string bytes
    FLenPfxSize: integer; // size of string length prefix
    FSize: integer;       // size of string bytes
    FStrType: TStringTypeToDefine;
    FStrTermChar: WideChar;
    FMaxStringSize: integer;
    FCodePage: integer;
    procedure GetStrParams;
  public
    procedure UpdateControlStates;
    procedure UpdateControlStatesAndPreview;
    procedure MakePreview;
  end;

var
  FormDefineString: TFormDefineString;

procedure Invoke(VA: TVA);

implementation

{$R *.dfm}


const
  MAX_REASONABLE_STRING_SIZE = $FFFF;

procedure Invoke(VA: TVA);
var
  strCodePage: TCodePage;
  Size: int;
begin
  FormDefineString.Show;

  Size := 0;

  if CoreGet.Strings.Get(VA, Size, strCodePage) then
  begin
    FormDefineString.edCodePage.Text := IntToStr(strCodePage);
  end;

  FormDefineString.FVA := VA;
  if Size <> 0 then
    FormDefineString.FSize := Size;

  FormDefineString.edVA.Text := Format('0x%x', [VA]);
  FormDefineString.edSize.Text := Format('%d', [Size]);

  FormDefineString.UpdateControlStatesAndPreview;
end;

procedure TFormDefineString.btUpdatePreviewClick(Sender: TObject);
begin
  MakePreview;
end;

procedure TFormDefineString.btCpResetClick(Sender: TObject);
begin
  edCodePage.Text := '';
  UpdateControlStatesAndPreview;
end;

procedure TFormDefineString.btCpUtf16Click(Sender: TObject);
begin
  edCodePage.Text := '1200';
  UpdateControlStatesAndPreview;
end;

procedure TFormDefineString.btCpUtf8Click(Sender: TObject);
begin
  edCodePage.Text := '65001';
  UpdateControlStatesAndPreview;
end;

procedure TFormDefineString.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkReturn:
      btDoneClick(nil);
    vkEscape:
      Close;
  end;
end;

procedure TFormDefineString.GetStrParams;
var
  c: IVDCore;
begin
  c := CoreGet();

  // codepage
  if not TryStrToInt(edCodePage.Text, FCodePage) then
    FCodePage := 0;

  // -------------------------------------------------------------
  FStrType := st_none;
  FLenPfxSize := 0;

  if rbUseSize.Checked then
  begin
    // size, decimal
    if TryStrToInt(edSize.Text, FSize) then
      FStrType := st_use_size;
  end
  else if rbNullTerm.Checked then
  begin
    FSize := c.Strings.ScanCharTerminatedStringSize(FVA, FCodePage, #0, FMaxStringSize);
    if FSize <> 0 then
      FStrType := st_null_term;
  end
  else if rbCharTerm.Checked then
  begin
    if edCustomChar.Text <> '' then
    begin
      FStrTermChar := edCustomChar.Text[1];
      FSize := c.Strings.ScanCharTerminatedStringSize(FVA, FCodePage, FStrTermChar, FMaxStringSize);
      if FSize <> 0 then
        FStrType := st_char_term;
    end;
  end
  else if rbLenPrefixed.Checked then
  begin
    FSize := 0;

    // length prefix size
    if not TryStrToInt(edLenPfxSize.Text, FLenPfxSize) then
      FLenPfxSize := 0;

    if FLenPfxSize <> 0 then
    begin
      FSize := c.Strings.GetLenPrefixedStringSize(FVA, FLenPfxSize);
      if (FSize <> 0) and (FSize <= MAX_REASONABLE_STRING_SIZE) then
        FStrType := st_len_pfx;
    end;
  end;

  edSize.Text := IntToStr(FSize);
  // -------------------------------------------------------------
end;

procedure TFormDefineString.MakePreview;
var
  Text: BSTR;
begin
  GetStrParams;
  mmPreview.Clear;
  if FSize <> 0 then
    if CoreGet().Strings.ReadString(FVA + FLenPfxSize, FSize, FCodePage, Text) then
      mmPreview.Text := Text;
end;

procedure TFormDefineString.btDoneClick(Sender: TObject);
begin
  GetStrParams;
  if FSize <> 0 then
  begin
    CoreGet.Strings.Define(FVA + FLenPfxSize, FSize, FCodePage);
    CoreGet.UI.RepaintView(VDAPI.TUIViewType.UIVT_DISASM);
    Close;
  end;
end;

procedure TFormDefineString.rbNullTermClick(Sender: TObject);
begin
  UpdateControlStatesAndPreview;
end;

procedure TFormDefineString.UpdateControlStates;
begin
  edCustomChar.Enabled := rbCharTerm.Checked;
  // edLenPfxSize.Enabled := rbLenPrefixed.Checked;
  edSize.Enabled := rbUseSize.Checked;
end;

procedure TFormDefineString.UpdateControlStatesAndPreview;
begin
  UpdateControlStates;
  MakePreview;
end;

end.
