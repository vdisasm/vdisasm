unit uCore.Zip;

interface

procedure ZipFolder(const ZipFileName, FolderPath: string);

implementation

uses
  System.IOUtils,
  System.SysUtils, // expand
  System.Zip;

procedure ZipFolder;
var
  fn: string;
begin
  fn := TPath.ChangeExtension(FolderPath, '.zipping');
  TZipFile.ZipDirectoryContents(fn, FolderPath, TZipCompression.zcStored);

  if TFile.Exists(ZipFileName) then
    TFile.Delete(ZipFileName);
  TFile.Move(fn, ZipFileName);
end;

end.
