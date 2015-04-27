unit uFormLiveCall;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormLiveCall = class(TForm)
    mmParams: TMemo;
    Label1: TLabel;
    btCall: TButton;
  private
  public
  end;

function GetParams: string;

implementation

{$R *.dfm}


function GetParams: string;
var
  f: TFormLiveCall;
begin
  f := TFormLiveCall.Create(nil);
  try
    if f.ShowModal = mrCancel then
      result := ''
    else
      result := f.mmParams.Text;
  finally
    f.Free;
  end;
end;

end.
