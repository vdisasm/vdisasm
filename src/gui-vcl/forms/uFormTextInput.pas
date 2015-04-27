unit uFormTextInput;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Forms,
  Vcl.Graphics,
  Vcl.StdCtrls,

  System.Classes,
  System.SysUtils,
  System.Variants;

type
  TFormTextInput = class(TForm)
    mmText: TMemo;
    btOK: TButton;
    btCancel: TButton;
    Label1: TLabel;
    StaticText1: TStaticText;
    procedure FormShow(Sender: TObject);
    procedure mmTextKeyPress(Sender: TObject; var Key: Char);
  private
  public
  end;

var
  FormTextInput: TFormTextInput;

function InputText(var Text: string; const Caption: string): boolean;

implementation

{$R *.dfm}


function InputText;
begin
  FormTextInput.Caption := Caption;
  FormTextInput.mmText.Text := Text;

  FormTextInput.mmText.SelectAll;

  if FormTextInput.ShowModal = mrCancel then
  begin
    Text := '';
    exit(false);
  end;

  Text := FormTextInput.mmText.Text;
  exit(true);
end;

procedure TFormTextInput.FormShow(Sender: TObject);
begin
  mmText.SetFocus;
end;

procedure TFormTextInput.mmTextKeyPress(Sender: TObject; var Key: Char);
begin
  case Key of
    Char(VK_RETURN):
      if not(GetKeyState(VK_SHIFT) < 0) then
      begin
        Key := #0;
        ModalResult := mrOk;
      end;
    Char(VK_ESCAPE):
      ModalResult := mrCancel;
  end;
end;

end.
