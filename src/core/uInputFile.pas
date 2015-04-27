{
  * Input File contain base info on Input File (being loader or already loaded).
  *
  * FileName, WorkingDir, Params are filled before calling loaders. Loaders in
  * turn can use FileName to make checks.
}
unit uInputFile;

interface

uses
  System.SysUtils,
  VDAPI;

type
  TVDInputFile = class(TInterfacedObject, IVDInputFile)
  private
    FFileName, FWorkingDir, FParams: string;
    FDirty: boolean;
  public
    class function InputFileToDbPath(const InputFileName: string): string; static;
    function SaveToDB(ACore: TObject): boolean;
    function LoadFromDB(ACore: TObject): boolean;
  public
    { VDAPI }
    function GetFileName: BSTR; stdcall;
    procedure SetFileName(Value: BSTR); stdcall;

    function GetParams: BSTR; stdcall;
    procedure SetParams(Value: BSTR); stdcall;

    function GetWorkingDir: BSTR; stdcall;
    procedure SetWorkingDir(Value: BSTR); stdcall;
  end;

implementation

uses
  uCore,
  uDB,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf;

{ TVDInputFile }

class function TVDInputFile.InputFileToDbPath(
  const InputFileName: string): string;
begin
  Result := InputFileName + '_vddb';
end;

// Getters.

function TVDInputFile.GetFileName: BSTR;
begin
  Result := FFileName;
end;

function TVDInputFile.GetParams: BSTR;
begin
  Result := FParams;
end;

function TVDInputFile.GetWorkingDir: BSTR;
begin
  Result := FWorkingDir;
end;

procedure TVDInputFile.SetFileName(Value: BSTR);
begin
  FFileName := Value;
  FDirty := True;
end;

procedure TVDInputFile.SetParams(Value: BSTR);
begin
  FParams := Value;
  FDirty := True;
end;

procedure TVDInputFile.SetWorkingDir(Value: BSTR);
begin
  FWorkingDir := Value;
  FDirty := True;
end;

function TVDInputFile.SaveToDB(ACore: TObject): boolean;
var
  c: TVDCore;
  tag: byte;
  data: TVDStreamIO;
begin
  if not FDirty then
    Exit(True);

  c := ACore as TVDCore;
  tag := byte(DBTAG_InputFile);
  data := TVDStreamIO.Create(TVDMemoryStream.Create);
  try
    data.WriteStr(FFileName);
    data.WriteStr(FWorkingDir);
    data.WriteStr(FParams);
    Result := c.DB.Put(TBytes.Create(tag), data.ToBytes) = BP_OK;
    if Result then
      FDirty := False;
  finally
    data.Free;
  end;
end;

function TVDInputFile.LoadFromDB(ACore: TObject): boolean;
var
  c: TVDCore;
  data: TVDStreamIO;
  Value: TBytes;
begin
  c := ACore as TVDCore;
  Result := c.DB.Get(TBytes.Create(DBTAG_InputFile), Value) = BP_OK;
  if Result then
  begin
    data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Value));
    try
      FFileName := data.ReadStr();
      FWorkingDir := data.ReadStr();
      FParams := data.ReadStr();
{$IFDEF DEBUG}
      c.GetLog.WriteLn('Input file info:');
      c.GetLog.WriteLn('  FileName: ' + FFileName);
      c.GetLog.WriteLn('  WorkingDir: ' + FWorkingDir);
      c.GetLog.WriteLn('  Params: ' + FParams);
{$ENDIF}
      FDirty := False;
    finally
      data.Free;
    end;
  end;
end;

end.
