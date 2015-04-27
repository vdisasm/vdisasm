unit uFrameBreakpoints;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, uFrameBaseListView, Vcl.ExtCtrls,
  Vcl.Menus, Vcl.ComCtrls;

type
  TFrameBreakpoints = class(TFrameBaseListView)
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrameBreakpoints: TFrameBreakpoints;

implementation

{$R *.dfm}

end.
