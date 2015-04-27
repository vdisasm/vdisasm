unit uFormStringScan;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.StdCtrls,

  System.Generics.Collections,
  System.UITypes,

  uFrameBaseListView,
  uFrameScanStrings,

  VDAPI;

type
  TVDStringListRec = record
    va: TVA;
    size: int;
    codepage: VDAPI.TCodePage;
  end;

  TVDStringList = TList<TVDStringListRec>;

  TFormStringScan = class(TForm)
    GroupBox1: TGroupBox;
    Scan: TButton;
    EditMinLen: TEdit;
    Label1: TLabel;
    FrameScanStrings1: TFrameScanStrings;
    procedure ScanClick(Sender: TObject);
  end;

var
  FormStringScan: TFormStringScan;

implementation

{$R *.dfm}


procedure TFormStringScan.ScanClick(Sender: TObject);
begin
  // Set params and force update as there's no external timer for this
  // list view.
  FrameScanStrings1.MinLen := StrToInt(EditMinLen.Text);
  FrameScanStrings1.UpdateUISync(True);
end;

end.
