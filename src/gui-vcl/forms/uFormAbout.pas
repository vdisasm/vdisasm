unit uFormAbout;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Imaging.pngimage, Vcl.ExtCtrls,
  Vcl.StdCtrls;

type
  TFormAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    LinkLabel1: TLinkLabel;
    lbVer: TLabel;
    MemoAck: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure LinkLabel1LinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormAbout: TFormAbout;

implementation

uses
  VDLib.Platform,
  uCore.Strings,
  VDAPI;

{$R *.dfm}


procedure TFormAbout.FormCreate(Sender: TObject);
var
  ackFn: string;
begin
  lbVer.Caption := SVDisAsmVer;

  LinkLabel1.Caption := Format('<a href="%s">%s</a>', [SVDisAsmUrl, SVisitWebsite]);
  LinkLabel1.Left := (Label1.Width - LinkLabel1.Width) div 2 + Label1.Left;

  ackFn := IOGet.ExpandPath('%local%\ack.txt');
  if FileExists(ackFn) then
    MemoAck.Lines.LoadFromFile(ackFn);
end;

procedure TFormAbout.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;

procedure TFormAbout.LinkLabel1LinkClick(Sender: TObject; const Link: string;
  LinkType: TSysLinkType);
begin
  TVDWeb.OpenUrl(SVDisAsmUrl);
end;

end.
