unit uCpus;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  VDAPI;

type
  TMapNameCpu = TDictionary<string, IVDCpu>;
  TNameCpuPair = TPair<string, IVDCpu>;

  TVDCPUs = class(TInterfacedObject, IVDCpus)
  private
    FMapNameCpu: TMapNameCpu;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ClearAll;
  public
    procedure AddInternal(const Name: string);
  public
    { VDAPI }
    procedure reserved; stdcall;

    function Get(Name: BSTR_IN; out Value: IVDCpu): BOOL; stdcall;
    procedure Enumerate(cb: TVDCpuEnumFunc; ud: pointer); stdcall;
  end;

  // Helper to get CPU from type name.
  // Result is True if type found and it is CPU.
  // If CodeType = nil then default CPU is taken.
function GetCpuFromName(const c: IVDCore; CodeType: BSTR_IN; out CPU: IVDCpu): boolean;

implementation

uses
  uCore,
  uTypes.Mgr;

function GetCpuFromName(const c: IVDCore; CodeType: BSTR_IN; out CPU: IVDCpu): boolean;
var
  ct: string;
  TypeUID: TTypeUID;
  TypeProviderBase: IVDTypeProviderBase;
begin
  if not Assigned(CodeType) then
    ct := string(CoreGet.GetData.CodeType)
  else
    ct := CodeType;

  if not c.TypeMgr.GetInfo(BSTR_IN(ct), TypeUID, TypeProviderBase) then
  begin
    c.Log.WriteLn(Format('Couldn''t find type: "%s"', [ct]));
    Exit(False);
  end;

  if not Supports(TypeProviderBase, IVDCpu, CPU) then
  begin
    c.Log.WriteLn(Format('Found type is not CPU: "%s"', [ct]));
    Exit(False);
  end;

  Exit(True);
end;

{ TVDCPUs }

procedure TVDCPUs.AddInternal(const Name: string);
begin
  if not FMapNameCpu.ContainsKey(Name) then
    FMapNameCpu.Add(Name, nil);
end;

procedure TVDCPUs.ClearAll;
begin
  FMapNameCpu.Clear;
end;

constructor TVDCPUs.Create;
begin
  inherited;
  FMapNameCpu := TMapNameCpu.Create();
end;

destructor TVDCPUs.Destroy;
begin
  FMapNameCpu.Free;
  inherited;
end;

procedure TVDCPUs.Enumerate(cb: TVDCpuEnumFunc; ud: pointer);
var
  Pair: TNameCpuPair;
begin
  if Assigned(cb) then
    for Pair in FMapNameCpu do
      if not cb(BSTR_IN(Pair.Key), Pair.Value, ud) then
        break;
end;

function TVDCPUs.Get(Name: BSTR_IN; out Value: IVDCpu): BOOL; stdcall;
var
  Prv: IVDTypeProviderBase;
begin
  Result := FMapNameCpu.TryGetValue(Name, Value);
  if Assigned(Value) then
    Exit;

  // CPU not found, try to find type which supports cpu interface.
  Prv := (CoreGet() as TVDCore).TypeMgr.GetProvider(Name);
  if Supports(Prv, IVDCpu) then
  begin
    Value := Prv as IVDCpu;
    FMapNameCpu.Add(Name, Value);
    Exit(True);
  end;
end;

procedure TVDCPUs.reserved;
begin

end;

end.
