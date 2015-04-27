unit uFormTypeEditor;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.ToolWin,

{$IFDEF USE_SYNEDIT}
  SynEdit,
  SynHighlighterCpp,
{$ENDIF}
  VDAPI;

type

  TFormTypeEditor = class(TForm)
    gbBase: TGroupBox;
    StaticText1: TStaticText;
    edName: TEdit;
    StaticText2: TStaticText;
    edComment: TEdit;
    pc1: TPageControl;
    tsAlias: TTabSheet;
    tsArray: TTabSheet;
    tsRecord: TTabSheet;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    Edit1: TEdit;
    Edit2: TEdit;
    StaticText3: TStaticText;
    edAliasSrcName: TEdit;
    tsDefinition: TTabSheet;
    btDone: TButton;
    procedure FormCreate(Sender: TObject);
  public
{$IFDEF USE_SYNEDIT}
    DefText: TSynEdit;
    DefHL: TSynCppSyn;
{$ENDIF}
  end;

var
  FormTypeEditor: TFormTypeEditor;

procedure Invoke(const Lib: IVDTypeLibrary; const TypeName: string); overload;

implementation

uses
  uStrings;

{$R *.dfm}


procedure Invoke(const Lib: IVDTypeLibrary; const TypeName: string);
begin
  if Lib = nil then
  begin
    CoreGet().Log.WriteLn(SNoTypeLib);
    exit;
  end;
  FormTypeEditor.Show;
end;

procedure TFormTypeEditor.FormCreate(Sender: TObject);
begin
{$IFDEF USE_SYNEDIT}
  DefText := TSynEdit.Create(self);
  DefText.Parent := tsDefinition;
  DefText.Align := alClient;

  DefHL := TSynCppSyn.Create(self);
  DefText.Highlighter := DefHL;
{$ENDIF}
end;

end.
