unit uChooseFileName;

interface

function ChooseFileName(var FileName: string; const Dir: string = '';
  const Ext: string = ''): boolean;

implementation

uses
  System.SysUtils,
  Vcl.Dialogs;

function ChooseFileName;
var
  dlg: TSaveDialog;
  extension: string;
begin
  dlg := TSaveDialog.Create(nil);
  try
    dlg.InitialDir := Dir;
    dlg.FileName := FileName;

    if Ext.Chars[0] = '.' then
      extension := Ext.Substring(1)
    else
      extension := Ext;

    if Ext <> '' then
      dlg.DefaultExt := extension
    else
      dlg.DefaultExt := ExtractFileExt(FileName);
    if dlg.Execute then
    begin
      FileName := dlg.FileName;
      exit(True);
    end;
    exit(False);
  finally
    dlg.Free;
  end;
end;

end.
