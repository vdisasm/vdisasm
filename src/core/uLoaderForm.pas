unit uLoaderForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.SysUtils,
  System.Variants,
  System.Classes,
  System.UITypes,

  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,

  uLoaderTask,

  Vcl.StdCtrls,
  Vcl.ComCtrls,

  VDAPI, Vcl.ExtCtrls;

type
  TLoaderForm = class(TForm)
    lvSec: TListView;
    Panel1: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    edEntry: TEdit;
    cbCPUs: TComboBox;
    btOk: TButton;
    btCancel: TButton;
    Splitter1: TSplitter;
    CheckBoxMapToInput: TCheckBox;
    Label1: TLabel;
    cbAddressSize: TComboBox;
    procedure lvSecData(Sender: TObject; Item: TListItem);
    procedure FormActivate(Sender: TObject);
    procedure lvSecCustomDrawItem(Sender: TCustomListView; Item: TListItem;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure CheckBoxMapToInputClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FTask: TVDLoaderTask;
    FCore: IVDCore;
    procedure DisplaySections;
    procedure DisplayCpus;
    procedure DisplayAddressSize;
    procedure DisplayAll;
    procedure UpdateMapToInputState(Map: Boolean);
  public
  end;

  // Display loader task and return True if user pressed OK or false if loading
  // was cancelled.
function DisplayLoaderTask(const Task: TVDLoaderTask): Boolean;

implementation

uses
  uSection;

{$R *.dfm}


function DisplayLoaderTask(const Task: TVDLoaderTask): Boolean;
var
  f: TLoaderForm;
  tmpVA: TVA;
begin
  f := TLoaderForm.Create(nil);
  try
    f.FTask := Task;
    f.FCore := CoreGet();

    // Show dialogue.
    Result := f.ShowModal() = mrOk; // show form

    // Get params back from dialogue.
    if Result then
    begin
      // cpu name
      Task.CpuName := f.cbCPUs.Text;

      // address size
      case f.cbAddressSize.ItemIndex of
        0:
          Task.AddressSize := 2;
        1:
          Task.AddressSize := 4;
        2:
          Task.AddressSize := 8;
      else
        ; // stays default
      end;

      // entry
      if f.FCore.EvaluateVA(BSTR_IN(f.edEntry.Text), tmpVA) then
        Task.Entry := tmpVA;
    end;
  finally
    f.free;
  end;
end;

{ TLoaderForm }

procedure TLoaderForm.CheckBoxMapToInputClick(Sender: TObject);
begin
  UpdateMapToInputState(CheckBoxMapToInput.Checked);
end;

procedure TLoaderForm.DisplayAddressSize;
var
  size: integer;
begin
  size := FTask.AddressSize;
  case size of
    2:
      cbAddressSize.ItemIndex := 0;
    4:
      cbAddressSize.ItemIndex := 1;
    8:
      cbAddressSize.ItemIndex := 2;
  else
    cbAddressSize.ItemIndex := -1;
  end;
end;

procedure TLoaderForm.DisplayAll;
begin
  DisplaySections;
  DisplayCpus;
  DisplayAddressSize;
  edEntry.Text := Format('0x%x', [FTask.Entry]);
end;

function DisplayCpus_cb(Name: BSTR_IN; CPU: IVDCpu; ud: Pointer): BOOL; stdcall;
begin
  TLoaderForm(ud).cbCPUs.Items.Add(Name);
  Result := True;
end;

procedure TLoaderForm.DisplayCpus;
begin
  cbCPUs.Clear;
  cbCPUs.Sorted := True;
  cbCPUs.Items.BeginUpdate;
  try
    FCore.CPUs.Enumerate(DisplayCpus_cb, self);
  finally
    cbCPUs.Items.EndUpdate;
  end;

  cbCPUs.ItemIndex := cbCPUs.Items.IndexOf(FTask.CpuName);
end;

procedure TLoaderForm.DisplaySections;
begin
  lvSec.Items.Count := FTask.SecList.Count;
end;

procedure TLoaderForm.FormActivate(Sender: TObject);
begin
  DisplayAll;
  btOk.SetFocus;
end;

procedure TLoaderForm.FormCreate(Sender: TObject);
begin
  // todo: CheckBoxMapToInput is now invisible, because it will fail due to
  // attempt to exclusively acess to same file. File manager must be introduced.
  CheckBoxMapToInput.Visible := False;
end;

procedure TLoaderForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    vkEscape:
      ModalResult := mrCancel;
  end;
end;

procedure TLoaderForm.lvSecCustomDrawItem(Sender: TCustomListView;
  Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  sec: TSecBase;
begin
  sec := FTask.SecList[Item.Index];
  if sec.IsConflict then
    Sender.Canvas.Font.Color := clRed;
end;

procedure TLoaderForm.lvSecData(Sender: TObject; Item: TListItem);
var
  sec: TSecBase;
begin
  sec := FTask.SecList[Item.Index];
  Item.Caption := Format('%d', [Item.Index]); // #
  Item.SubItems.Add(sec.Name);                // name
  Item.SubItems.Add(Format('0x%x-0x%x', [sec.VA, sec.VA + sec.VirtSize])); // va
  Item.SubItems.Add(Format('0x%x', [sec.VirtSize])); // vsize
  if sec is TSecFromFile then
  begin
    Item.SubItems.Add(Format('0x%x', [TSecFromFile(sec).Offset])); // rofs
    Item.SubItems.Add(Format('0x%x', [TSecFromFile(sec).RawSize])); // rsize
  end
  else
  begin
    Item.SubItems.Add(''); // rofs
    Item.SubItems.Add(''); // rsize
  end;
  Item.SubItems.Add(SectionFlagsToString(sec.Flags)); // flags
end;

procedure TLoaderForm.UpdateMapToInputState(Map: Boolean);
var
  i: integer;
  sec: TSecBase;
begin
  for i := 0 to FTask.SecList.Count - 1 do
  begin
    sec := FTask.SecList[i];
    if sec is TSecFromFile then
      TSecFromFile(sec).Mapped := Map;
  end;
  lvSec.Invalidate;
end;

end.
