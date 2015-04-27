unit uFormCopySpecial;

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
  Vcl.StdCtrls,
  Vcl.ClipBrd,
  Vcl.Samples.Spin,

  VDAPI;

type
  TFormCopySpecial = class(TForm)
    edVaFirst: TEdit;
    btCopy: TButton;
    GroupBox1: TGroupBox;
    rbTNorm: TRadioButton;
    rbTPat: TRadioButton;
    rbTPas: TRadioButton;
    rbTC: TRadioButton;
    mmPreview: TMemo;
    Label2: TLabel;
    rbTPasAsmDb: TRadioButton;
    edVaLast: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    cbSize: TComboBox;
    GroupBox3: TGroupBox;
    Label5: TLabel;
    edNameFmt: TEdit;
    edByteFmt: TEdit;
    Label6: TLabel;
    edDelimiter: TEdit;
    edIndent: TEdit;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    mmHeader: TMemo;
    mmFooter: TMemo;
    Label11: TLabel;
    edColumnCount: TSpinEdit;
    cbDelimAtEOL: TCheckBox;
    cbUseSize: TCheckBox;
    rbTCSharp: TRadioButton;
    procedure btCopyClick(Sender: TObject);
    procedure edNameFmtChange(Sender: TObject);
    procedure cbHeaderClick(Sender: TObject);
    procedure cbFooterClick(Sender: TObject);
    procedure cbIndentClick(Sender: TObject);
    procedure edByteFmtChange(Sender: TObject);
    procedure edDelimiterChange(Sender: TObject);
    procedure rbTCClick(Sender: TObject);
    procedure rbTPasClick(Sender: TObject);
    procedure rbTPasAsmDbClick(Sender: TObject);
    procedure edIndentChange(Sender: TObject);
    procedure rbTPatClick(Sender: TObject);
    procedure rbTNormClick(Sender: TObject);
    procedure mmHeaderChange(Sender: TObject);
    procedure mmFooterChange(Sender: TObject);
    procedure edColumnCountChange(Sender: TObject);
    procedure cbDelimAtEOLClick(Sender: TObject);
    procedure cbSizeChange(Sender: TObject);
    procedure edVaLastChange(Sender: TObject);
    procedure cbUseSizeClick(Sender: TObject);
    procedure rbTCSharpClick(Sender: TObject);
    procedure edVaFirstChange(Sender: TObject);
  private
    FInternalChanging: boolean;
    CountToCopy: integer;

    FAllowVaLastChangeEvent: boolean;
    FAllowSizeChangeEvent: boolean;

    procedure Preview;

    // Return # of bytes copied.
    function ExecutCopy(Count: integer; out Text: string): integer;

    function GetCountToCopy: boolean;
  public
    Va0, Va1: TVa;
    procedure Show(Va0, Va1: TVa); overload;
  end;

var
  FormCopySpecial: TFormCopySpecial;

implementation

uses
  uCore.Strings;

{$R *.dfm}


const
  DEF_INDENT       = '  ';
  EMPTY_INDENT     = '';
  BYTES_IN_PREVIEW = 17;
  DELIMITER_COMMA  = ', ';
  DELIMITER_EMPTY  = '';

function VmRead(VA: TVa; buf: pointer; Size: uint32): uint32; inline;
begin
  Result := CoreGet().VM.Read(VA, buf, Size);
end;

function StepItem(VA: pva; step: integer): boolean; inline;
begin
  Result := CoreGet().Decoder.ItemStep(VA, step);
end;

function min(a, b: integer): integer; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function hexu(const VA: TVa): string; inline;
begin
  Result := format('0x%x', [VA]);
end;

{ TFormCopySpecial }

procedure TFormCopySpecial.btCopyClick(Sender: TObject);
var
  c: TClipboard;
  s: string;
  cnt: integer;
begin
  if not GetCountToCopy then
    exit;

  c := TClipboard.Create;
  try
    cnt := ExecutCopy(CountToCopy, s);
    c.AsText := s;
    mmPreview.Text := format('%s (0x%x byte(s))', [STextCopied, cnt]);
  finally
    c.Free;
  end;
end;

procedure TFormCopySpecial.cbDelimAtEOLClick(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.cbFooterClick(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.cbHeaderClick(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.cbIndentClick(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.cbSizeChange(Sender: TObject);
var
  tmp: TVa;
begin
  try
    FAllowVaLastChangeEvent := False;
    if FAllowSizeChangeEvent then
    begin
      if CoreGet().EvaluateVA(BSTR_IN(cbSize.Text), tmp, TNumberEvaluateFlag.EVAL_NONE) and (tmp <> 0) then
      begin
        Va1 := Va0 + tmp - 1;
        edVaLast.Text := hexu(Va1);
      end
      else
        edVaLast.Text := '';
      Preview;
    end;
  finally
    FAllowVaLastChangeEvent := True;
  end;
end;

procedure TFormCopySpecial.cbUseSizeClick(Sender: TObject);
begin
  edVaLast.Enabled := not cbUseSize.Checked;
  cbSize.Enabled := cbUseSize.Checked;
end;

procedure TFormCopySpecial.edByteFmtChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.edColumnCountChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.edDelimiterChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.edIndentChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.edNameFmtChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.edVaFirstChange(Sender: TObject);
var
  tmp, Size, Last: TVa;
begin
  try
    FAllowVaLastChangeEvent := False;
    FAllowSizeChangeEvent := False;
    if CoreGet.EvaluateVA(BSTR_IN(edVaFirst.Text), tmp) then
    begin
      Va0 := tmp;
      if cbUseSize.Checked then
      begin
        if CoreGet.EvaluateVA(BSTR_IN(cbSize.Text), Size) then
          edVaLast.Text := hexu(tmp + Size - 1)
        else
          edVaLast.Text := '';
      end
      else
      begin
        if CoreGet.EvaluateVA(BSTR_IN(edVaLast.Text), Last) and (Last >= tmp) then
          cbSize.Text := hexu(Last - tmp + 1)
        else
          cbSize.Text := '';
      end;
    end;
    Preview;
  finally
    FAllowVaLastChangeEvent := True;
    FAllowSizeChangeEvent := True;
  end;
end;

procedure TFormCopySpecial.edVaLastChange(Sender: TObject);
var
  tmp: TVa;
begin
  if FAllowVaLastChangeEvent then
  begin
    FAllowSizeChangeEvent := False;
    try
      if CoreGet.EvaluateVA(BSTR_IN(edVaLast.Text), tmp) and (tmp >= Va0) then
      begin
        Va1 := tmp;
        cbSize.Text := hexu(Va1 - Va0 + 1);
      end
      else
        cbSize.Text := '';
      Preview;
    finally
      FAllowSizeChangeEvent := True;
    end;
  end;
end;

function TFormCopySpecial.ExecutCopy(Count: integer; out Text: string): integer;
var
  Columns: integer;
  buf: array of byte;
  sz: integer;
  j: integer;
  delimiter: string;
  sIdName: string;
var
  lastItemAtAll: boolean;
  lastItemAtLine: boolean;
  bNeedDelimiterAtEol: boolean;
begin
  Text := '';
  Result := 0;

  if FInternalChanging then
    exit;

  if Count > 0 then
  begin
    SetLength(buf, Count);
    sz := VmRead(Va0, @buf[0], Count);
    Result := sz;

    Columns := edColumnCount.Value;

    delimiter := edDelimiter.Text;

    bNeedDelimiterAtEol := cbDelimAtEOL.Checked;

    // header
    sIdName := format(edNameFmt.Text, [Va0]);
    Text := Text + format(mmHeader.Text, [sIdName, sz]);

    for j := 0 to sz - 1 do
    begin
      lastItemAtAll := j = (sz - 1);
      lastItemAtLine := (j mod Columns) = (Columns - 1);

      // if start of new line (put line break to previous line)
      if (j mod Columns) = 0 then
      begin
        if (j <> 0) then
          Text := Text + sLineBreak;
        Text := Text + edIndent.Text;
      end;

      Text := Text + format(edByteFmt.Text, [buf[j]]);

      if (not lastItemAtLine) or (bNeedDelimiterAtEol) then
      begin
        // item separator
        if not lastItemAtAll then
          Text := Text + delimiter;
      end;
    end;

    // footer
    Text := Text + sLineBreak;
    Text := Text + mmFooter.Text;
  end;
end;

procedure TFormCopySpecial.Preview;
var
  s: string;
begin
  if FInternalChanging then
    exit;

  if GetCountToCopy then
  begin
    ExecutCopy(min(BYTES_IN_PREVIEW, CountToCopy), s);
    mmPreview.Text := s;
  end;
end;

procedure TFormCopySpecial.rbTCClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := DEF_INDENT;
  edByteFmt.Text := '0x%x';
  edDelimiter.Text := DELIMITER_COMMA;
  cbDelimAtEOL.Checked := True;
  mmHeader.Text := 'unsigned char %s[%d] = {' + sLineBreak;
  mmFooter.Text := '};';
  FInternalChanging := False;
  Preview;
end;

procedure TFormCopySpecial.rbTCSharpClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := DEF_INDENT;
  edByteFmt.Text := '0x%x';
  edDelimiter.Text := DELIMITER_COMMA;
  cbDelimAtEOL.Checked := True;
  mmHeader.Text := 'byte[] %s = {' + sLineBreak;
  mmFooter.Text := '};';
  FInternalChanging := False;
  Preview;
end;

procedure TFormCopySpecial.rbTNormClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := EMPTY_INDENT;
  edByteFmt.Text := '%x';
  edDelimiter.Text := DELIMITER_COMMA;
  cbDelimAtEOL.Checked := True;
  mmHeader.Text := '';
  mmFooter.Text := '';
  FInternalChanging := False;
  Preview;
end;

procedure TFormCopySpecial.rbTPasAsmDbClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := DEF_INDENT + 'db ';
  edByteFmt.Text := '$%x';
  edDelimiter.Text := DELIMITER_COMMA;
  cbDelimAtEOL.Checked := False;
  mmHeader.Text := 'procedure %s;' + sLineBreak + 'asm' + sLineBreak;
  mmFooter.Text := 'end;';
  FInternalChanging := False;
  Preview;
end;

procedure TFormCopySpecial.rbTPasClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := DEF_INDENT;
  edByteFmt.Text := '$%x';
  edDelimiter.Text := DELIMITER_COMMA;
  cbDelimAtEOL.Checked := True;
  mmHeader.Text := '%s: array [0 .. %d - 1] of byte = (' + sLineBreak;
  mmFooter.Text := ');';
  FInternalChanging := False;
  Preview;
end;

procedure TFormCopySpecial.rbTPatClick(Sender: TObject);
begin
  FInternalChanging := True;
  edIndent.Text := EMPTY_INDENT;
  edByteFmt.Text := '%2.2x';
  edDelimiter.Text := DELIMITER_EMPTY;
  cbDelimAtEOL.Checked := False;
  mmHeader.Text := '';
  mmFooter.Text := '';
  FInternalChanging := False;
  Preview;
end;

function TFormCopySpecial.GetCountToCopy: boolean;
var
  c: IVDCore;
  tmpVa0, tmpVa1: TVa;
begin
  c := CoreGet;

  // 1st va
  if (not c.EvaluateVA(BSTR_IN(edVaFirst.Text), tmpVa0)) or
    (not c.VM.Exists(tmpVa0)) then
  begin
    mmPreview.Text := format(SInvalidAddressString, [edVaFirst.Text]);
    exit(False);
  end;

  // last va
  if (not c.EvaluateVA(BSTR_IN(edVaLast.Text), tmpVa1)) or
    (not c.VM.Exists(tmpVa1)) then
  begin
    mmPreview.Text := format(SInvalidAddressString, [edVaLast.Text]);
    exit(False);
  end;

  Va0 := tmpVa0;
  Va1 := tmpVa1;

  if Va1 > Va0 then
    CountToCopy := (Va1 - Va0) + 1
  else
    CountToCopy := 1;

  exit(True);
end;

procedure TFormCopySpecial.mmFooterChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.mmHeaderChange(Sender: TObject);
begin
  Preview;
end;

procedure TFormCopySpecial.Show(Va0, Va1: TVa);
begin
  FAllowVaLastChangeEvent := True;
  FAllowSizeChangeEvent := True;
  cbUseSizeClick(nil);

  if Va1 < Va0 then
    Va1 := Va0;

  self.Va0 := Va0;
  self.Va1 := Va1;

  edVaFirst.Text := format('%s', [hexu(Va0)]);
  edVaLast.Text := format('%s', [hexu(Va1)]);
  cbSize.Text := format('%s', [hexu(Va1 - Va0 + 1)]);

  // Preview default: C.
  rbTCClick(nil);
  rbTC.Checked := True;

  Show;
end;

end.
