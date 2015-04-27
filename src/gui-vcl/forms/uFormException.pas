unit uFormException;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TFormException = class(TForm)
    btBreak: TButton;
    btContinue: TButton;
    Label1: TLabel;
  private
  public
  end;

function InvokeExceptionDialog(): boolean;

implementation

{$R *.dfm}


function InvokeExceptionDialog(): boolean;
var
  f: TFormException;
begin
  f := TFormException.Create(Application);
  try
    f.ShowModal;
    result := true;
  finally
    f.Free;
  end;
end;

end.
