unit uPlugin;

interface

uses
  VDAPI;

resourcestring
  SBinaryFile = 'Binary File';

type
  TBinaryLoader = class(TInterfacedObject, IVDLoaderPlugin)
  public
    function GetPossibleFormatCount: int; stdcall;
    procedure GetFormatDesc(Formats: IVDLoaderFormats); stdcall;
    procedure FillTask(Task: IVDLoaderTask; Id: int); stdcall;
    procedure TaskApplied; stdcall;
  end;

implementation

{ TBinaryLoader }

function TBinaryLoader.GetPossibleFormatCount: int;
begin
  Result := 1;
end;

procedure TBinaryLoader.GetFormatDesc(Formats: IVDLoaderFormats);
var
  Desc: TVDLoaderFormatDesc;
begin
  Desc.Text := SBinaryFile; // text
  Desc.Id := 0;             // don't care
  Formats.Add(Desc);
end;

procedure TBinaryLoader.FillTask(Task: IVDLoaderTask; Id: int);
var
  FileName: BSTR;
  FileSize: UInt64;
  FirstVA: TVA;
begin
  // We don't about Id because there's only one format.

  FileName := CoreGet.InputFile.FileName;
  FileSize := IOGet.FileSize(FileName);

  Task.AddSectionFromMappedFile(
    BSTR_IN(FileName), // src file
    'raw',             // section name
    0,                 // src offset
    FileSize,          // section size
    0,                 // rva of section
    TVDSectionFlag.Readable or TVDSectionFlag.Writable
    );

  Task.SetEntry(0);
end;

procedure TBinaryLoader.TaskApplied;
begin
  // no additional actions needed
end;

end.
