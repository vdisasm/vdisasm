unit uFormFindBytes;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,

  Winapi.Windows,
  Winapi.Messages,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Vcl.ComCtrls,

  uUiUtils,

  VDAPI;

type
  TFormFindBytes = class(TForm)
    mmPattern: TMemo;
    Label1: TLabel;
    rbUp: TRadioButton;
    rbDown: TRadioButton;
    cbStrEnc: TComboBox;
    Label3: TLabel;
    cbStrCaseSens: TCheckBox;
    btFind: TButton;
    StatusBar1: TStatusBar;
    procedure rbUpClick(Sender: TObject);
    procedure rbDownClick(Sender: TObject);
    procedure mmPatternChange(Sender: TObject);
    procedure mmPatternKeyPress(Sender: TObject; var Key: Char);
    procedure btFindClick(Sender: TObject);
    procedure StatusBar1DrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
      const Rect: TRect);
    procedure FormShow(Sender: TObject);
  private
    FFirstSearch: boolean;
    FSearchString: boolean;
    FSearchStringText: string;
    FDirection: integer;
    FCurVA: TVA;
    FFound: boolean;
    function GetCodePage(out CodePage: integer): boolean;
    procedure DoFind;
  public
    procedure InvokeSearch();
  end;

var
  FormFindBytes: TFormFindBytes;

implementation

uses
  uCore.Strings;

{$R *.dfm}


procedure TFormFindBytes.btFindClick(Sender: TObject);
begin
  DoFind;
end;

procedure TFormFindBytes.DoFind;
var
  c: IVDCore;
  TmpVA, UserVA: TVA;
  CodePage: integer;
begin
  mmPattern.Enabled := False;
  try
    StatusBar1.Panels[0].Text := '';

    c := CoreGet();

    UserVA := c.GetUserVA;

    if FFirstSearch then
      FCurVA := UserVA
    else
    begin
      if UserVA = FCurVA then
        inc(FCurVA, FDirection)
      else
        FCurVA := UserVA;
    end;

    Self.Caption := format(SStartSearchFromVA, [FCurVA]);

    TmpVA := FCurVA;

    if FSearchString then
    begin
      if not GetCodePage(CodePage) then
        exit;
      FFound := c.Search.&String(@TmpVA, BAD_VA, BSTR_IN(FSearchStringText), CodePage,
        cbStrCaseSens.Checked, FDirection);
    end
    else
    begin
      FFound := c.Search.Bytes(@TmpVA, BAD_VA, BSTR_IN(mmPattern.Text), FDirection);
    end;

    if FFound then
    begin
      StatusBar1.Panels[0].Text := format(SFoundAtVA, [TmpVA]);
      FFirstSearch := False;
      FCurVA := TmpVA;
      c.ChangeVA(TmpVA, True);
    end
    else
    begin
      StatusBar1.Panels[0].Text := SNothingFound;
      FlashWindowAndBeep(Self);
    end;

    StatusBar1.Invalidate;
  finally
    mmPattern.Enabled := True;
    mmPattern.SetFocus;
  end;
end;

procedure TFormFindBytes.FormShow(Sender: TObject);
begin
  StatusBar1.Panels[0].Text := '';
end;

function TFormFindBytes.GetCodePage(out CodePage: integer): boolean;
begin
  if cbStrEnc.Text <> '' then
    Result := TryStrToInt(cbStrEnc.Text, CodePage)
  else
  begin
    CodePage := 0;
    Result := True;
  end;

  if not Result then
  begin
    cbStrEnc.Color := clRed;
    FocusControl(cbStrEnc);
  end
  else
    cbStrEnc.Color := clWhite;
end;

procedure TFormFindBytes.InvokeSearch();
begin
  FDirection := 1;
  FFirstSearch := True;
  mmPattern.Clear;
  Show;
  mmPattern.SetFocus;
end;

procedure TFormFindBytes.mmPatternChange(Sender: TObject);
var
  s: string;
begin
  FFirstSearch := True;

  s := mmPattern.Text;
  FSearchString := (s.Length > 1) and (s[1] = '"') and (s[Length(s)] = '"');
  if FSearchString then
    FSearchStringText := s.Substring(1, s.Length - 2);

  cbStrEnc.Enabled := FSearchString;
  cbStrCaseSens.Enabled := FSearchString;
end;

procedure TFormFindBytes.mmPatternKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Char(VK_RETURN):
      if not(GetKeyState(VK_SHIFT) < 0) then
      begin
        Key := #0;
        DoFind;
      end;
    Char(VK_ESCAPE):
      Close;
  end;
end;

procedure TFormFindBytes.rbDownClick(Sender: TObject);
begin
  FDirection := 1;
end;

procedure TFormFindBytes.rbUpClick(Sender: TObject);
begin
  FDirection := -1;
end;

procedure TFormFindBytes.StatusBar1DrawPanel(StatusBar: TStatusBar;
  Panel: TStatusPanel; const Rect: TRect);
begin
  case Panel.Index of
    0:
      begin
        if not FFound then
          StatusBar1.Font.Color := clRed
        else
          StatusBar1.Font.Color := clBlack;
        StatusBar1.Canvas.TextOut(Rect.Left, Rect.Top, Panel.Text);
      end;
  end;
end;

end.
