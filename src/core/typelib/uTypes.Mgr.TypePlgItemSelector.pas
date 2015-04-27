unit uTypes.Mgr.TypePlgItemSelector;

interface

uses
  System.SysUtils,
  VDAPI,
  uCore.Strings,
  uTypes.Mgr;

type
  TTypePluginItemSelector = class(TInterfacedObject, IVDItemSelector)
  private
    FList: TPluginPathList;
    FTypeName: string;
  public
    constructor Create(List: TPluginPathList; const TypeName: string);
  public
    function GetCaption: BSTR; stdcall;
    function GetItemCount: int; stdcall;
    function GetItemText(Index: int): BSTR; stdcall;
  end;

implementation

{ TTypePluginItemSelector }

constructor TTypePluginItemSelector.Create(List: TPluginPathList; const TypeName: string);
begin
  inherited Create;
  FList := List;
  FTypeName := TypeName;
end;

function TTypePluginItemSelector.GetCaption: BSTR;
begin
  Result := Format(SChoosePluginForType, [FTypeName]);
end;

function TTypePluginItemSelector.GetItemCount: int;
begin
  Result := FList.Count;
end;

function TTypePluginItemSelector.GetItemText(Index: int): BSTR;
begin
  Result := FList[Index];
end;

end.
