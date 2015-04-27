unit uFormOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls,

  uFrameOptionsBase;

type
  TFrameOptionsBaseClass = class of TFrameOptionsBase;

  TFormOptions = class(TForm)
    pnlTree: TPanel;
    pnlBtns: TPanel;
    pnlCurrent: TPanel;
    Button1: TButton;
    Button2: TButton;
    Splitter1: TSplitter;
    TreeView1: TTreeView;
    procedure FormActivate(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    FFirstActivationDone: boolean;
    procedure ExpandAllItems;
    procedure NodeDeletion(Sender: TObject; Node: TTreeNode);
  public
    procedure AddOptions(const Path: string; FrameClass: TFrameOptionsBaseClass);
  end;

var
  FormOptions: TFormOptions;

implementation

{$R *.dfm}


uses
  uFrameOptionsDisplayColors;

type
  TNodeContainer = class
  public
    FrameClass: TFrameOptionsBaseClass;
    Frame: TFrame;
  end;

function FindTreeNodeChild(var n: TTreeNode; const Caption: string): boolean;
var
  v: string;
  i: integer;
begin
  if n <> nil then
  begin
    v := Caption.ToLower;
    for i := 0 to n.Count - 1 do
      if n[i].Text.ToLower = v then
      begin
        n := n[i];
        exit(True);
      end;
  end;
  exit(False);
end;

procedure TFormOptions.AddOptions(const Path: string; FrameClass: TFrameOptionsBaseClass);
var
  SubPath: TArray<string>;
  s: string;
  i: integer;
  nParent, nNew: TTreeNode;
  c: TNodeContainer;
begin
  SubPath := Path.Split(['/', '\']);
  if SubPath = nil then
    exit;

  TreeView1.Items.BeginUpdate;
  try
    nParent := nil;
    for i := 0 to High(SubPath) do
    begin
      s := SubPath[i];
      if not FindTreeNodeChild(nParent, s) then
      begin
        nNew := TreeView1.Items.AddChild(nParent, s);

        // For last node.
        if i = High(SubPath) then
        begin
          // Create container.
          c := TNodeContainer.Create;
          c.FrameClass := FrameClass;
          c.Frame := nil;

          // Bind container.
          nNew.Data := c;
        end;

        nParent := nNew;
      end;
    end;
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TFormOptions.ExpandAllItems;
var
  i: integer;
begin
  TreeView1.Items.BeginUpdate;
  try
    for i := 0 to TreeView1.Items.Count - 1 do
      TreeView1.Items[i].Expand(True);
  finally
    TreeView1.Items.EndUpdate;
  end;
end;

procedure TFormOptions.FormActivate(Sender: TObject);
begin
  if not FFirstActivationDone then
  begin
    FFirstActivationDone := True;

    TreeView1.OnDeletion := NodeDeletion;

    // Add items.
    TreeView1.Items.BeginUpdate;
    try
      AddOptions('Colors\Disasm', TFrameDisplayColors);
      // AddOptions('Colors\Hex', TFrameDisplayColors);
    finally
      TreeView1.Items.EndUpdate;
    end;

    ExpandAllItems;
  end;
end;

procedure TFormOptions.NodeDeletion(Sender: TObject; Node: TTreeNode);
begin
  TObject(Node.Data).Free;
  Node.Data := nil;
end;

procedure TFormOptions.TreeView1Change(Sender: TObject; Node: TTreeNode);
var
  ctr: TNodeContainer;
begin
  // We assume there's one one control (TFrame).
  if pnlCurrent.ControlCount <> 0 then
    pnlCurrent.Controls[0].Parent := nil;

  // Get container.
  ctr := Node.Data;
  if ctr = nil then
    exit;

  // If frame is not yet created we create it now.
  if ctr.Frame = nil then
  begin
    // Container w/o frame class doesn't make sense.
    if ctr.FrameClass = nil then
      exit;

    ctr.Frame := ctr.FrameClass.Create(self);
    ctr.Frame.Align := alClient;
  end;

  ctr.Frame.Parent := pnlCurrent;

end;

end.
