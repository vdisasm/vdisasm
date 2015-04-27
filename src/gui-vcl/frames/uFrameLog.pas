unit uFrameLog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Menus;

type
  TFrameLog = class(TFrame)
    Memo1: TMemo;
    PopupMenu1: TPopupMenu;
    ClearAll1: TMenuItem;
    CopyAll1: TMenuItem;
    CopySelected1: TMenuItem;
    procedure ClearAll1Click(Sender: TObject);
    procedure CopyAll1Click(Sender: TObject);
    procedure CopySelected1Click(Sender: TObject);
  private
  public
    procedure TextClear;
    procedure TextLog(const Text: string);
    procedure TextBeginUpdate;
    procedure TextEndUpdate;
  end;

implementation

{$R *.dfm}


procedure TFrameLog.ClearAll1Click(Sender: TObject);
begin
  TextClear;
end;

procedure TFrameLog.TextClear;
begin
  Memo1.Clear;
end;

procedure TFrameLog.CopyAll1Click(Sender: TObject);
begin
  Memo1.SelectAll;
  Memo1.CopyToClipboard;
end;

procedure TFrameLog.CopySelected1Click(Sender: TObject);
begin
  Memo1.CopyToClipboard;
end;

procedure TFrameLog.TextLog(const Text: string);
begin
  // todo: optimize it as a read-only string list.
  Memo1.Text := Memo1.Text + Text;
  SendMessage(Memo1.Handle, EM_LINESCROLL, 0, Memo1.Lines.Count);
  Application.ProcessMessages;
end;

procedure TFrameLog.TextBeginUpdate;
begin
  Memo1.Lines.BeginUpdate;
end;

procedure TFrameLog.TextEndUpdate;
begin
  Memo1.Lines.EndUpdate;
end;

end.
