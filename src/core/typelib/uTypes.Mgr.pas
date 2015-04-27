unit uTypes.Mgr;

interface

uses
  System.Generics.Collections,
  System.SysUtils,

  VDAPI,

  uTypes.Lib;

type
  TTypeMgrItem = class
    Name: string;
    UID: TTypeUID;
    Provider: IVDTypeProviderBase;
    constructor Create(const Name: string; UID: TTypeUID; const Provider: IVDTypeProviderBase);
  end;

  TMapByName = TDictionary<string, TTypeMgrItem>;
  TMapByUID = TDictionary<TTypeUID, TTypeMgrItem>;
  TMapNameProvider = TDictionary<string, IVDTypeProviderBase>;

  TTypeName = string;
  TPluginPath = string;
  TPluginPathList = TList<TPluginPath>;
  TMapPluginsForTypes = TDictionary<TTypeName, TPluginPathList>;

  TVDTypeMgr = class(TInterfacedObject, IVDTypeMgr)
  private
    FMapByName: TMapByName;
    FMapByUID: TMapByUID;
    FLastUID: TTypeUID;

    procedure ItemNotification(Sender: TObject; const Item: TTypeMgrItem;
      Action: TCollectionNotification);

    function GetNewUID(const Name: string): TTypeUID; // inline;
    function WriteMappingToDb(UID: TTypeUID; const Name: string): boolean;
  private
    FMapPluginsForTypes: TMapPluginsForTypes;

    procedure MapPluginsForTypes_ValueNotify(Sender: TObject; const Item: TPluginPathList;
      Action: TCollectionNotification);
  public // for internal use
    property MapPluginsForTypes: TMapPluginsForTypes read FMapPluginsForTypes;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ClearMapping; // inline;
    procedure ClearAll;

    procedure AddItem(const Item: TTypeMgrItem);

    function Put(const Name: string; UID: TTypeUID): TTypeMgrItem;

  private
    // This function onlt additional for GetEx.
    // Try to fetch either Item or Item.Provider (if one is nil).
    // It must be called in GetEx (by name or uid), because GetEx can return
    // existing item but without type Provider. In this case type should be
    // loaded dynamically.
    // Item can be Nil or not.
    // Either Name or UID is null.
    function GetExHelper(var Item: TTypeMgrItem; Name: string): boolean;
  public
    // Try to get TTypeMgrItem by Name or UID. If it's not found try to load
    // it from plugin (which provides this type if any).
    procedure GetEx(const Name: string; out Item: TTypeMgrItem); overload;
    procedure GetEx(UID: TTypeUID; out Item: TTypeMgrItem); overload;

    function GetExistingOrAddNewUID(const Name: string; out UID: TTypeUID): TTypeMgrItem;

    function LoadFromDb: boolean;

{$IFDEF DEBUG}
    procedure Dump;
{$ENDIF}
    // Either Name is "" or UID = 0.
    function FindItemByNameOrUID(const Name: string; UID: TTypeUID): TTypeMgrItem;

    // Try to find Name type in registered types provided by plugin.
    // If name is found, try to load plugin and create provider. Type item
    // updated with found provider.
    // FoundProvider is result provider created (plugin gets loaded).
    // Result is type item.
    function TryLoadProviderFromPlugin(Name: BSTR_IN; out FoundProvider: IVDTypeProviderBase): TTypeMgrItem;

  public
    { VDAPI }

    // Get info of type already added to map (not postponed).
    function GetInfo(Name: BSTR_IN; out UID: TTypeUID; out Provider: IVDTypeProviderBase): BOOL; stdcall;

    // Get provider either from map or from postponed list.
    function GetProvider(Name: BSTR_IN): IVDTypeProviderBase; stdcall;

    procedure RegisterPluginForType(Name, PluginPath: BSTR_IN); stdcall;
  end;

implementation

uses
  uCore,
  uDb,
  uRegions.TypeData,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf,
  uPlugins,
  uTypes.Mgr.TypePlgItemSelector;

{ TVDTypeMgr }

{$IFDEF DEBUG}


procedure TVDTypeMgr.Dump;
var
  c: IVDCore;
  pair: TPair<TTypeUID, TTypeMgrItem>;
begin
  c := CoreGet;
  for pair in FMapByUID do
    c.Log.WriteLn(Format('%4.4d "%s"', [pair.Key, pair.Value.Name]));
end;
{$ENDIF}


function TVDTypeMgr.FindItemByNameOrUID(const Name: string;
  UID: TTypeUID): TTypeMgrItem;
begin
  if Name <> '' then
    FMapByName.TryGetValue(Name, Result)
  else if (UID <> 0) then
    FMapByUID.TryGetValue(UID, Result)
{$IFDEF DEBUG}
  else
    raise Exception.Create('WTH?');
{$ENDIF}
end;

function TVDTypeMgr.Put(const Name: string; UID: TTypeUID): TTypeMgrItem;
var
  Item: TTypeMgrItem;
begin
  if FMapByName.TryGetValue(Name, Item) then
  begin
    // If exists, modify.
    Item.Name := Name;
    Item.UID := UID;
  end
  else
  begin
    // If not exists, add new.
    Item := TTypeMgrItem.Create(Name, UID, nil);
    AddItem(Item);
  end;
  Result := Item;
end;

procedure TVDTypeMgr.RegisterPluginForType(Name, PluginPath: BSTR_IN);
var
  PlgPathList: TPluginPathList;
  bNewList: boolean;
begin
  // Get list.
  bNewList := False;
  if not FMapPluginsForTypes.TryGetValue(Name, PlgPathList) then
  begin
    PlgPathList := TPluginPathList.Create;
    bNewList := True;
  end;

{$IFDEF DEBUG}
  if not bNewList then
    if PlgPathList.IndexOf(PluginPath) <> -1 then
      raise Exception.Create('There is duplicate of plugin path for type.');
{$ENDIF}
  PlgPathList.Add(PluginPath);

  if bNewList then
    FMapPluginsForTypes.Add(Name, PlgPathList);
end;

procedure TVDTypeMgr.AddItem(const Item: TTypeMgrItem);
begin
  FMapByName.Add(Item.Name, Item);
  FMapByUID.Add(Item.UID, Item);
end;

procedure TVDTypeMgr.ClearMapping;
begin
  FMapByName.Clear;
  FMapByUID.Clear;
  FLastUID := 0;
end;

procedure TVDTypeMgr.ClearAll;
begin
  ClearMapping;

  // Don't really need to clear it? As map is filled on core start.
  // FMapPluginsForTypes.Clear;
end;

constructor TVDTypeMgr.Create;
begin
  FMapByName := TMapByName.Create;
  FMapByName.OnValueNotify := ItemNotification;
  FMapByUID := TMapByUID.Create;
  FMapPluginsForTypes := TMapPluginsForTypes.Create;
  FMapPluginsForTypes.OnValueNotify := MapPluginsForTypes_ValueNotify;
end;

destructor TVDTypeMgr.Destroy;
begin
  FreeAndNil(FMapByName);
  FreeAndNil(FMapByUID);
  FreeAndNil(FMapPluginsForTypes);
  inherited;
end;

function TVDTypeMgr.GetExHelper(var Item: TTypeMgrItem; Name: string): boolean;
var
  TypeProvider: IVDTypeProviderBase;
  FoundItem: TTypeMgrItem;
begin
  // Early exit.
  if Assigned(Item) and Assigned(Item.Provider) then
    Exit(True);

  CoreGet.UI.SetGuiRefreshState(False);
  try
    if (Name = '') then
    begin
      if not Assigned(Item) then
        exit(false) // raise Exception.Create('Error Message')
      else
        Name := Item.Name;
    end;

    // Try load and update provider.
    FoundItem := TryLoadProviderFromPlugin(BSTR_IN(Name), TypeProvider);
    if FoundItem = nil then
      Exit(False);

    if Item = nil then
      Item := FoundItem;

    Exit(True);
  finally
    CoreGet.UI.SetGuiRefreshState(True);
  end;
end;

procedure TVDTypeMgr.GetEx(const Name: string; out Item: TTypeMgrItem);
begin
  FMapByName.TryGetValue(Name, Item);
  GetExHelper(Item, Name);
end;

procedure TVDTypeMgr.GetEx(UID: TTypeUID; out Item: TTypeMgrItem);
var
  Name: string;
begin
  FMapByUID.TryGetValue(UID, Item);
  if Assigned(Item) then
    Name := Item.Name
  else
    Name := '';
  GetExHelper(Item, Name);
end;

function TVDTypeMgr.GetExistingOrAddNewUID(const Name: string; out UID: TTypeUID): TTypeMgrItem;
begin
  if FMapByName.TryGetValue(Name, Result) then
  begin
    UID := Result.UID;
    Exit;
  end;
  // New.
  UID := GetNewUID(Name);
  Result := Put(Name, UID);
end;

function TVDTypeMgr.GetNewUID(const Name: string): TTypeUID;
begin
  if FLastUID = 0 then
  begin
    if not LoadFromDb then
      raise Exception.Create('Failed to load type mapping.');
  end;
  inc(FLastUID);
  Result := FLastUID;

  if not WriteMappingToDb(FLastUID, Name) then
    raise Exception.Create('Failed to save UID for type.');
end;

function TVDTypeMgr.GetProvider(Name: BSTR_IN): IVDTypeProviderBase;
var
  Item: TTypeMgrItem;
begin
  GetEx(Name, Item);
  if Item <> nil then
    Result := Item.Provider
  else
    Result := nil;
end;

function TVDTypeMgr.GetInfo(Name: BSTR_IN; out UID: TTypeUID;
  out Provider: IVDTypeProviderBase): BOOL;
var
  Item: TTypeMgrItem;
begin
  GetEx(Name, Item);
  if (Item <> nil) then
  begin
    UID := Item.UID;
    Provider := Item.Provider;
    Exit(True);
  end;
  Exit(False);
end;

procedure TVDTypeMgr.ItemNotification(Sender: TObject; const Item: TTypeMgrItem;
  Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

// -----------------------------------------------------------------------------

function TVDTypeMgr.LoadFromDb: boolean;
var
  c: TVDCore;
  cursor: IBPlusTreeCursor;
  Key, Value: TBytes;
  data: TVDStreamIO;
  TypeText: string;
begin
  ClearMapping;

  c := TVDCore(CoreGet);
  cursor := c.DB.CursorCreateEx(TBytes.Create(DBTAG_TypeDataIdName), [kpEqual, kpGreater], True);
  if cursor = nil then
  begin
    FLastUID := 0;
    Exit(True);
  end;

  repeat
    Key := cursor.Key;
    if Length(Key) < 5 then // Tag[1] + UID[4]
      Exit(False);
    Value := cursor.Value;
    if Value = nil then // must not be ""
      Exit(False);
    // get UID
    data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Key));
    data.Skip;
    FLastUID := data.ReadU32;
    data.Free;
    // get type name
    data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Value));
    TypeText := data.ReadStr;
    data.Free;
    // add
    Put(TypeText, FLastUID);
  until not cursor.Next;
  Result := True;
end;

procedure TVDTypeMgr.MapPluginsForTypes_ValueNotify(Sender: TObject;
  const Item: TPluginPathList; Action: TCollectionNotification);
begin
  if Action = cnRemoved then
    Item.Free;
end;

function TVDTypeMgr.TryLoadProviderFromPlugin(Name: BSTR_IN;
  out FoundProvider: IVDTypeProviderBase): TTypeMgrItem;
var
  sName: string;
  sPath: string;
  PlgPathList: TPluginPathList;
  PlgInfo: TLoadedPluginInfo;
  Provider: IVDTypeProviderBase;
  UID: TTypeUID;
  TypeMgrItem: TTypeMgrItem;
  index: int;
  selector: IVDItemSelector;
  AlreadyLoaded: boolean;
begin
  Result := nil;
  FoundProvider := nil;
  sName := name;

  if FMapPluginsForTypes.TryGetValue(sName, PlgPathList) then
    if Assigned(PlgPathList) then
    begin
      if PlgPathList.Count = 1 then
        sPath := PlgPathList[0]
      else
      begin
        selector := TTypePluginItemSelector.Create(PlgPathList, sName);
        index := CoreGet().UI.SelectFromList(selector);
        if index <> -1 then
          sPath := PlgPathList[index]
        else
          raise Exception.CreateFmt('Need to choose plugin for type "%s"', [sName]);
      end;

      if (CoreGet().PluginMgr as TVDPluginManager).LoadPluginInternal(sPath, 0, PlgInfo, AlreadyLoaded) then
      begin
        if Assigned(PlgInfo.CreateTypeProvider) then
        begin
          PlgInfo.CreateTypeProvider(Name, Provider);
{$IFDEF DEBUG}
          if not Assigned(Provider) then
            raise Exception.CreateFmt('Type provider returned NIL on creation of "%s"', [Name]);
{$ENDIF}
          TypeMgrItem := GetExistingOrAddNewUID(Name, UID);
          TypeMgrItem.Provider := Provider;

          FoundProvider := Provider;
          Result := TypeMgrItem;
        end;
      end;
    end;
end;

function TVDTypeMgr.WriteMappingToDb(UID: TTypeUID;
  const Name: string): boolean;
var
  c: TVDCore;
  Key, Value: TVDStreamIO;
begin
  c := TVDCore(CoreGet);
  Key := TVDStreamIO.Create(TVDMemoryStream.Create);
  Value := TVDStreamIO.Create(TVDMemoryStream.Create);
  try
    Key.WriteTag(DBTAG_TypeDataIdName);
    Key.WriteU32(UID);
    Value.WriteStr(Name);
    Result := c.DB.Put(Key.ToBytes, Value.ToBytes) = BP_OK;
  finally
    Key.Free;
    Value.Free;
  end;
end;

{ TTypeItem }

constructor TTypeMgrItem.Create(const Name: string; UID: TTypeUID;
  const Provider: IVDTypeProviderBase);
begin
  self.Name := Name;
  self.UID := UID;
  self.Provider := Provider;
end;

end.
