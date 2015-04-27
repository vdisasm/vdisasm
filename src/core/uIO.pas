unit uIO;

interface

uses
  System.Generics.Collections,
  VDAPI;

type
  TPathVariables = TDictionary<string, string>;

  TVDIO = class(TInterfacedObject, IVDIO)
  private
    FVariables: TPathVariables; // aliases
    procedure InitPathVariables;
  public
    constructor Create;
    destructor Destroy; override;

    function Copy(SrcPath, DstPath: BSTR; Ofs: uint64 = 0;
      Size: uint64 = 0; TotalSize: uint64 = 0): BOOL; stdcall;

    function CreateEmptyFile(Path: BSTR; Size: uint64): BOOL; stdcall;

    procedure SetVariable(Name, Path: BSTR); stdcall;
    procedure DelVariable(Name: BSTR); stdcall;
    function ExpandPath(Path: BSTR): BSTR; stdcall;

    function FileExists(Path: BSTR): BOOL; stdcall;

    function FileSize(Path: BSTR): uint64; stdcall;

    function CreateDirectory(Path: BSTR): BOOL; stdcall;

  end;

function IOGet: IVDIO; stdcall;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils;

{ TVDIO }

procedure TVDIO.SetVariable(Name, Path: BSTR);
begin
  if FVariables.ContainsKey(Name) then
    FVariables[Name] := Path
  else
    FVariables.Add(Name, Path);
end;

function TVDIO.Copy(SrcPath, DstPath: BSTR; Ofs, Size, TotalSize: uint64): BOOL;
var
  src, dst, dstDir: string;
  fsSrc, fsDst: TFileStream;
  ReadSize: uint64;
begin
  src := ExpandPath(SrcPath);
  dst := ExpandPath(DstPath);
  try
    if TFile.Exists(src) then
    begin
      if Size = 0 then
        TFile.Copy(src, dst) // whole file
      else
      begin // partial
        // Open source.
        fsSrc := TFileStream.Create(src, fmOpenRead or fmShareDenyWrite);
        // Ensure dst dir exists.
        dstDir := ExtractFileDir(dst);
        if not TDirectory.Exists(dstDir) then
          TDirectory.CreateDirectory(dstDir);
        // Create dest.
        fsDst := TFileStream.Create(dst, fmCreate);

        // Set final size.
        if (TotalSize <> 0) then
          fsDst.Size := TotalSize
        else
          fsDst.Size := Size;
        fsDst.Seek(0, soFromBeginning);

        // Copy.
        try
          if not fsSrc.Seek(Ofs, TSeekOrigin.soBeginning) = Ofs then
            exit(false);

          // Calc # of bytes to copy.
          if (TotalSize <> 0) and (TotalSize < Size) then
            ReadSize := TotalSize
          else
            ReadSize := Size;

          fsDst.CopyFrom(fsSrc, ReadSize);

        finally
          fsSrc.Free;
          fsDst.Free;
        end;
      end;
    end
    else if TDirectory.Exists(src) then
      TDirectory.Copy(src, dst)
    else
      exit(false);
    exit(true);
  except
    exit(false);
  end;
end;

constructor TVDIO.Create;
begin
  FVariables := TPathVariables.Create;
  InitPathVariables;
end;

function TVDIO.CreateEmptyFile(Path: BSTR; Size: uint64): BOOL;
var
  fs: TFileStream;
  tmpPath, dstDir: string;
begin
  try
    tmpPath := ExpandPath(Path);
    dstDir := ExtractFileDir(tmpPath);

    if not TDirectory.Exists(dstDir) then
      TDirectory.CreateDirectory(dstDir);

    fs := TFileStream.Create(tmpPath, fmCreate);
    try
      fs.Size := Size;
      Result := fs.Size = Size;
    finally
      fs.Free;
    end;
  except
    Result := false;
  end;
end;

function TVDIO.CreateDirectory(Path: BSTR): BOOL;
var
  s: string;
begin
  s := ExpandPath(Path);
  TDirectory.CreateDirectory(s);
  Result := true;
end;

procedure TVDIO.DelVariable(Name: BSTR);
begin
  FVariables.Remove(Name);
end;

destructor TVDIO.Destroy;
begin
  FVariables.Free;
  inherited;
end;

function TVDIO.ExpandPath(Path: BSTR): BSTR;
var
  pair: TPair<string, string>;
var
  flags: TReplaceFlags;
begin
  flags := [rfReplaceAll, rfIgnoreCase];

  Result := Path;
  Result := StringReplace(Result, '\', PathDelim, flags);
  Result := StringReplace(Result, '/', PathDelim, flags);

  // Expand static aliases.
  for pair in FVariables do
    Result := StringReplace(Result, pair.Key, pair.Value, flags);
end;

function TVDIO.FileExists(Path: BSTR): BOOL;
begin
  Result := TFile.Exists(ExpandPath(Path));
end;

function TVDIO.FileSize(Path: BSTR): uint64;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(ExpandPath(Path), fmOpenRead);
  try
    Result := fs.Size;
  finally
    fs.Free;
  end;
end;

procedure TVDIO.InitPathVariables;
var
  app, rootdir: string;
begin
  // app = dir where core lib located.
  app := GetModuleName(HInstance);
  app := ExtractFileDir(app);

  // root: installation dir
  rootdir := TPath.GetFullPath(app + PathDelim + '..');
  FVariables.Add(TIOVar.rootdir, rootdir);

  FVariables.Add(TIOVar.Local, rootdir + PathDelim + 'local');
  FVariables.Add(TIOVar.TypeLibs, rootdir + PathDelim + 'typelibs');
  FVariables.Add(TIOVar.Bin, app);
  FVariables.Add(TIOVar.Temp, ExcludeTrailingPathDelimiter(TPath.GetTempPath));
  FVariables.Add(TIOVar.Plugins, app + PathDelim + 'plugins');
  FVariables.Add(TIOVar.Loaders, app + PathDelim + 'loaders');
  FVariables.Add(TIOVar.Debuggers, app + PathDelim + 'debuggers');
  FVariables.Add(TIOVar.CPU, app + PathDelim + 'cpu');
  FVariables.Add(TIOVar.TypeProviders, app + PathDelim + 'typeproviders');
end;

{$REGION 'Singleton'}


var
  SingletonIO: IVDIO = nil;

function IOGet: IVDIO; stdcall;
begin
  if SingletonIO = nil then
    SingletonIO := TVDIO.Create;
  Result := SingletonIO;
end;

exports
  IOGet;
{$ENDREGION 'Singleton'}


end.
