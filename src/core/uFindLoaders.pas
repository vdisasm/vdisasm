{
  * This module makes list of loaders that can load given Input File.
}
unit uFindLoaders;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,
  uLoader.Formats,
  VDAPI;

type
  TLoaderInfo = class
  public
    LoaderFileName: string;
    PossibleFormatCount: int;
    Formats: IVDLoaderFormats;
    constructor Create(const LoaderFileName: string);
  end;

  TLoaderInfos = TObjectList<TLoaderInfo>;

  TLoaderInfoPair = TPair<TLoaderInfo, integer>; // info, ordinal

  TLoaderInfoRec = record
  public
    LoaderFileName: string;
    FormatId: integer;
  end;

function FindLoaderToAppy(const InputFileName: string; out LdrInfo: TLoaderInfoRec): boolean;

implementation

uses
  uCore,
  uCore.Strings,
  uPlugins;

type
  TLoaderSelector = class(TInterfacedObject, IVDItemSelector)
  private
    FFlat: TList<TLoaderInfoPair>;
  public
    property Flat: TList<TLoaderInfoPair> read FFlat;
  public
    constructor Create(const Infos: TLoaderInfos);
    destructor Destroy; override;

    function GetCaption: BSTR; stdcall;
    function GetItemCount: int; stdcall;
    function GetItemText(Index: int): BSTR; stdcall;
  end;

  { Functions }

function FindLoaderInfos_Callback(FileName: BSTR_IN; Plugin: IUnknown; Info: PVDPluginInfo; ud: pointer): BOOL; stdcall;
var
  LoaderInfo: TLoaderInfo;
  Ldr: IVDLoaderPlugin;
begin
  Result := True; // always continue

  if Supports(Plugin, IVDLoaderPlugin, Ldr) then
  begin
    // Create loader info and ask loader to add found formats info.
    LoaderInfo := TLoaderInfo.Create(FileName);
    Ldr.GetFormatDesc(LoaderInfo.Formats);
    LoaderInfo.PossibleFormatCount := Ldr.GetPossibleFormatCount;

    // If loader found no formats it is not used.
    if (LoaderInfo.Formats as TVDLoaderFormats).Items.Count = 0 then
    begin
      LoaderInfo.Free;
      exit;
    end;

    // We have at least one format.
    TLoaderInfos(ud).Add(LoaderInfo);
  end;
end;

procedure FindLoaderInfos(const InputFileName: string; Infos: TLoaderInfos);
var
  C: IVDCore;
  Mgr: TVDPluginManager;
  LdrPath: string;
begin
  Infos.Clear;
  C := CoreGet;
  Mgr := (C.PluginMgr as TVDPluginManager);
  LdrPath := IOGet.ExpandPath('%loaders%');
  Mgr.ScanPluginDir(BSTR_IN(LdrPath), FindLoaderInfos_Callback, 0, Infos);

  Infos.Sort(TComparer<TLoaderInfo>.Construct(
    function(const a, b: TLoaderInfo): integer
    begin
      Result := -(a.PossibleFormatCount - b.PossibleFormatCount);
    end))
end;

function FindLoaderToAppy(const InputFileName: string; out LdrInfo: TLoaderInfoRec): boolean;
var
  Infos: TLoaderInfos;
  Selector: IVDItemSelector;
  i: int;
  item: TLoaderInfoPair;
begin
  Infos := TLoaderInfos.Create;
  try

    // Scan for all available formats (in different loaders).
    // Each loader can provide few formats.
    uFindLoaders.FindLoaderInfos(InputFileName, Infos);

    case Infos.Count of
      0:
        exit(False);
    else
      begin
        Selector := TLoaderSelector.Create(Infos);
        i := CoreGet.UI.SelectFromList(Selector);
        if i = -1 then
          exit(False);

        item := (Selector as TLoaderSelector).Flat[i];
        LdrInfo.LoaderFileName := item.Key.LoaderFileName;
        LdrInfo.FormatId := (item.Key.Formats as TVDLoaderFormats).Items[item.Value].Id;

        // debug:
        // CoreGet.Log.WriteLn(LoaderFileName);
        exit(True);
      end;
    end;
  finally
    Infos.Free;
  end;
end;

{ TLoaderSelector }

constructor TLoaderSelector.Create(const Infos: TLoaderInfos);
var
  inf: TLoaderInfo;
  i: integer;
  pair: TLoaderInfoPair;
  Items: TList<TVDLoaderFormatDesc>;
begin
  FFlat := TList<TLoaderInfoPair>.Create;
  for inf in Infos do
  begin
    Items := (inf.Formats as TVDLoaderFormats).Items;
    for i := 0 to Items.Count - 1 do
    begin
      pair.Key := inf;
      pair.Value := i;
      FFlat.Add(pair);
    end;
  end;
end;

destructor TLoaderSelector.Destroy;
begin
  FFlat.Free;
  inherited;
end;

function TLoaderSelector.GetCaption: BSTR;
begin
  Result := SSelectLoader;
end;

function TLoaderSelector.GetItemCount: int;
begin
  Result := FFlat.Count;
end;

function TLoaderSelector.GetItemText(Index: int): BSTR;
var
  pair: TLoaderInfoPair;
  Items: TList<TVDLoaderFormatDesc>;
begin
  pair := FFlat[index];
  Items := (pair.Key.Formats as TVDLoaderFormats).Items;
  Result := Items[pair.Value].Text;
end;

{ TLoaderInfo }

constructor TLoaderInfo.Create(const LoaderFileName: string);
begin
  inherited Create;
  self.Formats := TVDLoaderFormats.Create;
  self.LoaderFileName := LoaderFileName;
end;

end.
