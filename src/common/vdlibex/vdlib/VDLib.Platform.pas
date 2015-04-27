{
  Platform adapter.
  Windows platform currently.
}
unit VDLib.Platform;

interface

type

  { TVDPlatform }

  TVDPlatform = class
  public
    class procedure DbgStop; static;
  end;

  { TVDWeb }

{$SCOPEDENUMS ON}

  TWebSearchProvider = (Google);
{$SCOPEDENUMS OFF}

  TVDWeb = class
    class procedure OpenUrl(const URL: string); static;
    class procedure OpenWebSearch(const Text: string;
      Provider: TWebSearchProvider = TWebSearchProvider.Google); static;
  end;

  { TVDLibrary }

  TModuleID = type NativeUInt;

  TVDLibrary = class
    class function Load(const FileName: string): TModuleID; static;
    class procedure Unload(ModuleID: TModuleID); static;
    class function GetSymbolByOrdinal(ModuldeId: TModuleID; Ordinal: cardinal): Pointer; static;
    class function GetSymbolByName(ModuldeId: TModuleID; const Name: string): Pointer; static;
  end;

  TVDPath = class
    class function IsDirectory(const Path: string): boolean; static;
    class function DirectoryExists(const Path: string): boolean; static;
  end;

implementation

uses
  System.Classes,
  System.SysUtils,
  WinApi.Windows,
  WinApi.ShellApi;

resourcestring
  SReparsePointsAreNotSupported = 'Reparse points are not supported.';

{ TVDPlatform }

class procedure TVDPlatform.DbgStop;
asm
  int 3
end;

{ TVDWeb }

class procedure TVDWeb.OpenUrl(const URL: string);
begin
  if URL <> '' then
    ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOW);
end;

class procedure TVDWeb.OpenWebSearch;
var
  URL: string;
begin
  URL := '';
  case Provider of
    TWebSearchProvider.Google:
      URL := format('http://www.google.com/search?q=%s', [Text]);
  end;
  TVDWeb.OpenUrl(URL);
end;

{ TVDLibrary }

class function TVDLibrary.GetSymbolByName(ModuldeId: TModuleID;
  const Name: string): Pointer;
begin
  Result := GetProcAddress(ModuldeId, PChar(Name));
end;

class function TVDLibrary.GetSymbolByOrdinal(ModuldeId: TModuleID;
  Ordinal: cardinal): Pointer;
begin
  Result := GetProcAddress(ModuldeId, PChar(Ordinal));
end;

function IsMZPE(const FileName: string): boolean;
const
  valid_pe: array [0 .. 3] of ansichar = 'PE'#0#0;
var
  fs: TFileStream;
  mz: array [0 .. 1] of ansichar;
  pe: array [0 .. 3] of ansichar;
  peofs: uint32;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if (fs.Read(mz[0], 2) = 2) then
      if (mz = 'MZ') then
        if (fs.Seek($3C, TSeekOrigin.soBeginning) = $3C) then
          if (fs.Read(peofs, 4) = 4) then
            if (fs.Seek(peofs, TSeekOrigin.soBeginning) = peofs) then
              if (fs.Read(pe[0], 4) = 4) then
                if (pe = valid_pe) then
                begin
                  exit(true);
                end;
    exit(False);
  finally
    fs.Free;
  end;
end;

class function TVDLibrary.Load(const FileName: string): TModuleID;
begin
  if not IsMZPE(FileName) then
    exit(0);
  Result := WinApi.Windows.LoadLibrary(PChar(FileName));
end;

class procedure TVDLibrary.Unload(ModuleID: TModuleID);
begin
  if ModuleID <> 0 then
    WinApi.Windows.FreeLibrary(ModuleID);
end;

{ TVDPath }

class function TVDPath.DirectoryExists(const Path: string): boolean;
var
  attr, err: DWORD;
begin
  attr := WinApi.Windows.GetFileAttributes(PChar(Path));
  if attr = WinApi.Windows.INVALID_FILE_ATTRIBUTES then
  begin
    err := GetLastError;
    if err = ERROR_FILE_NOT_FOUND then
      exit(False);
    RaiseLastOSError;
  end;
  if (attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then
    raise Exception.Create(SReparsePointsAreNotSupported);
  Result := (attr and FILE_ATTRIBUTE_DIRECTORY) <> 0;
end;

class function TVDPath.IsDirectory(const Path: string): boolean;
var
  attr: DWORD;
begin
  attr := WinApi.Windows.GetFileAttributes(PChar(Path));
  if attr = WinApi.Windows.INVALID_FILE_ATTRIBUTES then
    RaiseLastOSError;
  if (attr and FILE_ATTRIBUTE_REPARSE_POINT) <> 0 then
    raise Exception.Create(SReparsePointsAreNotSupported);
  Result := (attr and FILE_ATTRIBUTE_DIRECTORY) <> 0;
end;

end.
