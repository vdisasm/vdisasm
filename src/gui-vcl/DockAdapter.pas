unit DockAdapter;

interface

uses
  System.Classes,

  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.ImgList,

  Xml.XmlIntf,
  Xml.XmlDoc;

type
  TXAlign = TAlign;
  TXDockZone = TObject;
  TXDockManager = class;

  TXDockSite = class(TCustomControl)
  public
    Manager: TXDockManager;
    LoadDesignLayout: boolean;
    procedure DockControl(AControl: TControl; ADropOnZone: TXDockZone = nil;
      AZoneSide: TAlign = alClient);
  end;

  TXDockPanel = class(TPanel)
  public
    Images: TCustomImageList;
    ImageIndex: integer;
    ShowHeaderImage: boolean;
    Caption: string;
    Site: TXDockSite;
  end;

  PXDockPanel = ^TXDockPanel;

  IXXmlElement = IXMLNode;
  IXXmlDocument = IXMLDocument;

  TXDockGetCompId = procedure(Sender: TObject; AComponent: TComponent; var AId: string) of object;
  TXDockGetComp = procedure(Sender: TObject; const AId: string; var AComponent: TComponent) of object;
  TXDockStreamAppInfo = procedure(Sender: TObject; const Xml: IXXmlDocument) of object;

  TXDockManager = class(TComponent)
  private
    FOnGetComp: TXDockGetComp;
    FOnGetCompId: TXDockGetCompId;
    FOnWriteAppInfo: TXDockStreamAppInfo;
    FOnReadAppInfo: TXDockStreamAppInfo;
  public
    procedure LoadFromFile(const Path: string);
    procedure SaveToFile(const Path: string);

    property OnGetCompId: TXDockGetCompId read FOnGetCompId write FOnGetCompId;
    property OnGetComp: TXDockGetComp read FOnGetComp write FOnGetComp;
    property OnWriteAppInfo: TXDockStreamAppInfo read FOnWriteAppInfo write FOnWriteAppInfo;
    property OnReadAppInfo: TXDockStreamAppInfo read FOnReadAppInfo write FOnReadAppInfo;
  end;

implementation

{ TLMDDockSite }

procedure TXDockSite.DockControl(AControl: TControl;
  ADropOnZone: TXDockZone; AZoneSide: TAlign);
begin
end;

{ TLMDDockManager }

procedure TXDockManager.LoadFromFile(const Path: string);
begin
end;

procedure TXDockManager.SaveToFile(const Path: string);
begin
end;

end.
