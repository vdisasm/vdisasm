unit uLoader.Formats;

interface

uses
  System.Generics.Collections,
  VDAPI;

type
  TVDLoaderFormats = class(TInterfacedObject, IVDLoaderFormats)
  public
    Items: TList<TVDLoaderFormatDesc>;

    constructor Create;
    destructor Destroy; override;

    procedure Add(var Desc: TVDLoaderFormatDesc); stdcall;
  end;

implementation

{ TVDLoaderFormats }

constructor TVDLoaderFormats.Create;
begin
  inherited Create;
  Self.Items := TList<TVDLoaderFormatDesc>.Create;
end;

destructor TVDLoaderFormats.Destroy;
begin
  Items.Free;
  inherited;
end;

procedure TVDLoaderFormats.Add(var Desc: TVDLoaderFormatDesc);
begin
  Items.Add(Desc);
end;

end.
