unit uFormCalc;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  System.UITypes,

  uCore.Strings,
  VDAPI;

type
  TFormCalc = class(TForm)
    MemoExpr: TMemo;
    MemoResult: TMemo;
    GroupBox1: TGroupBox;
    RadioButton64: TRadioButton;
    RadioButton32: TRadioButton;
    RadioButton16: TRadioButton;
    RadioButton8: TRadioButton;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MemoExprKeyPress(Sender: TObject; var Key: Char);
    procedure RadioButton64Click(Sender: TObject);
  private
  public
    procedure Calculate;
    procedure ShowCalc(const Text: string = '');
  end;

var
  FormCalc: TFormCalc;

implementation

uses
  uFormMain; // for rainbow

{$R *.dfm}


function u64toBinStr(const value: uint64; bits: byte): string;
var
  mask: uint64;
  c: Char;
begin
  result := '';
  if bits = 0 then
    exit;
  mask := uint64(1) shl (bits - 1);
  while bits > 0 do
  begin
    if (value and mask) <> 0 then
      c := '1'
    else
      c := '0';

    if result <> '' then
    begin
      if (bits mod 8) = 0 then
        result := result + ' ';
      // else if (bits mod 4) = 0 then
      // result := result + '''';
    end;

    result := result + c;

    dec(bits);
    mask := mask shr 1;
  end;
end;

procedure TFormCalc.Calculate;
var
  CUR_BITS: integer;
var
  value: IVDConstExpression;
  u64: uint64;
  c: IVDCore;
  BStrText: BSTR;
begin
  MemoResult.Clear;

  if not CoreGet().EvaluateExpr(BSTR_IN(MemoExpr.Text), value) then
  begin
{$IFDEF COLORTEXT_RAINBOW}
    if MemoExpr.Text = 'rainbow' then
    begin
      MemoResult.Text := 'Nice expression';
      FormMain.DockLayouts.FrameDisasm1.DisasmText.AnimateRainbow;
      exit;
    end;
    MemoResult.Lines.Add(SBadExpression);
{$ENDIF}
    // Exit if bad expression.
    exit;
  end;

  if RadioButton64.Checked then
    CUR_BITS := 64
  else if RadioButton32.Checked then
    CUR_BITS := 32
  else if RadioButton16.Checked then
    CUR_BITS := 16
  else if RadioButton8.Checked then
    CUR_BITS := 8
  else
    CUR_BITS := 64;

  u64 := value.GetUInt;

  MemoResult.Lines.BeginUpdate;
  try
    MemoResult.Lines.Add(Format('hex:  0x%x', [u64]));
    MemoResult.Lines.Add(Format('dec:  %d', [u64]));
    MemoResult.Lines.Add(Format('bin:  %s', [u64toBinStr(u64, CUR_BITS)]));

    c := CoreGet();
    if Assigned(c) then
    begin
      if c.Names.Get(u64, BStrText) then
        MemoResult.Lines.Add('name: ' + BStrText);
    end;

  finally
    MemoResult.Lines.EndUpdate;
  end;
end;

procedure TFormCalc.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    vkEscape:
      Close;
  end;
end;

procedure TFormCalc.MemoExprKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
  begin
    Key := #0;
    Calculate;
  end;
end;

procedure TFormCalc.RadioButton64Click(Sender: TObject);
begin
  Calculate;
end;

procedure TFormCalc.ShowCalc(const Text: string);
begin
  Show;
  MemoExpr.Text := Text;
  Calculate;
end;

end.
