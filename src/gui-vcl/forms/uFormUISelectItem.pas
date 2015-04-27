unit uFormUISelectItem;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,

  VDAPI;

type

  TFormUISelectItem = class(TForm)
    lb1: TListBox;
    procedure lb1DblClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FSelector: IVDItemSelector;
    FResult: integer;
  protected
    procedure UpdateItems;
  public
    function SelectItem(Selector: IVDItemSelector): integer;
  end;

var
  FormUISelectItem: TFormUISelectItem;

implementation

{$R *.dfm}

{ TFormUISelectItem }

procedure TFormUISelectItem.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
    VK_RETURN:
      lb1DblClick(nil);
  end;
end;

procedure TFormUISelectItem.lb1DblClick(Sender: TObject);
begin
  if lb1.ItemIndex <> -1 then
  begin
    FResult := lb1.ItemIndex;
    Close;
  end;
end;

function TFormUISelectItem.SelectItem(Selector: IVDItemSelector): integer;
begin
  FSelector := Selector;
  FResult := -1;
  Caption := Selector.GetCaption;
  UpdateItems;
  ShowModal;
  Result := FResult;
end;

procedure TFormUISelectItem.UpdateItems;
var
  cnt: integer;
  i: integer;
begin
  cnt := FSelector.GetItemCount;
  lb1.Clear;
  if cnt = 0 then
    exit;
  lb1.Items.BeginUpdate;
  try
    for i := 0 to cnt - 1 do
    begin
      lb1.Items.Add(FSelector.GetItemText(i));
    end;
    lb1.ItemIndex := 0;
    lb1.TabStop := True;
  finally
    lb1.Items.EndUpdate;
  end;
end;

end.
