unit uFormExportDump;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  VDAPI;

type
  TFormExportDump = class(TForm)
    edVA0: TEdit;
    edVA1: TEdit;
    btExport: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    btAllVAs: TButton;
    procedure btExportClick(Sender: TObject);
    procedure btAllVAsClick(Sender: TObject);
  private
    function GetVAs(c: IVDCore; out va0, va1: TVA): boolean;
    procedure SetEditVa(va: TVA; IsStart: boolean);
  public
  end;

var
  FormExportDump: TFormExportDump;

procedure Invoke(va0, va1: TVA);

implementation

uses
  uChooseFileName,
  uCore.Strings;

{$R *.dfm}


procedure Invoke(va0, va1: TVA);
begin
  FormExportDump.SetEditVa(va0, true);
  FormExportDump.SetEditVa(va1, false);
  FormExportDump.Show;
end;

procedure TFormExportDump.btAllVAsClick(Sender: TObject);
var
  c: IVDCore;
  va0, va1: TVA;
begin
  edVA0.Clear;
  edVA1.Clear;

  c := CoreGet;
  if c.VM.GetFirstVA(@va0) then
    edVA0.Text := Format('0x%x', [va0]);
  if c.VM.GetLastVA(@va1) then
    edVA1.Text := Format('0x%x', [va1]);
end;

procedure TFormExportDump.btExportClick(Sender: TObject);
var
  c: IVDCore;
  va0, va1: TVA;
  fn: string;
begin
  c := CoreGet();
  if (c <> nil) and GetVAs(c, va0, va1) and ChooseFileName(fn, '', SExtDumpedText) then
    c.DumpDatabaseText(va0, va1, BSTR_IN(fn));
end;

function TFormExportDump.GetVAs(c: IVDCore; out va0, va1: TVA): boolean;
  function doEdit(e: TEdit; out va: TVA): boolean;
  begin
    if c.EvaluateVA(BSTR_IN(e.Text), va) then
      if c.VM.Exists(va) then
        exit(true);
    e.SelectAll;
    e.SetFocus;
    exit(false);
  end;

begin
  result := doEdit(edVA0, va0) and doEdit(edVA1, va1);
end;

procedure TFormExportDump.SetEditVa(va: TVA; IsStart: boolean);
var
  e: TEdit;
  c: IVDCore;
  ok: boolean;
begin
  if IsStart then
    e := edVA0
  else
    e := edVA1;

  if va <> BAD_VA then
  begin
    ok := true;
  end

  else

  begin
    // bad_va
    c := CoreGet;
    if IsStart then
      ok := c.VM.GetFirstVA(@va)
    else
      ok := c.VM.GetLastVA(@va);
  end;

  if ok then
    e.Text := Format('0x%x', [va]);
end;

end.
