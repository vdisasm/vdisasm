unit uStream.VAStream;

interface

uses
  System.SysUtils,
  VDAPI,
  uStream;

type
  TVDVAStream = class(TInterfacedObject, IVDBaseStream)
  private
    FVM: IVDVirtualMemory;
    FPosition: TVA;
  public
    constructor Create(FirstVA, LastVA: TVA; ReadOnly: boolean);
  public
    function GetPosition: UInt64; stdcall;
    procedure SetPosition(Value: UInt64); stdcall;
    function GetSize: UInt64; stdcall;
    function SetSize(Value: UInt64): BOOL; stdcall;
    function Read(var Buffer; Size: UInt32): UInt32; stdcall;
    function Write(const Buffer; Size: UInt32): UInt32; stdcall;
  end;

implementation

constructor TVDVAStream.Create(FirstVA, LastVA: TVA; ReadOnly: boolean);
var
  c: IVDCore;
begin
  c := CoreGet;

  if FirstVA = BAD_VA then
    if not c.VM.GetFirstVA(@FirstVA) then
      raise Exception.Create('Can''t get FirstVA');

  if LastVA = BAD_VA then
    if not c.VM.GetLastVA(@LastVA) then
      raise Exception.Create('Can''t get LastVA');

  inherited Create();

  FVM := c.VM;

  SetPosition(FirstVA);
end;

function TVDVAStream.GetPosition: UInt64;
begin
  Result := FPosition;
end;

function TVDVAStream.GetSize: UInt64;
begin
  raise Exception.Create('GetSize not applicable');
end;

function TVDVAStream.Read(var Buffer; Size: UInt32): UInt32;
begin
  Result := FVM.Read(FPosition, @Buffer, Size);
  inc(FPosition, Result);
end;

procedure TVDVAStream.SetPosition(Value: UInt64);
begin
  if not FVM.Exists(Value) then
    raise Exception.Create('VA position does not exist.');
  FPosition := Value;
end;

function TVDVAStream.SetSize(Value: UInt64): BOOL;
begin
  raise Exception.Create('SetSize not applicable');
end;

function TVDVAStream.Write(const Buffer; Size: UInt32): UInt32;
begin
  Result := FVM.Write(FPosition, @Buffer, Size);
  inc(FPosition, Result);
end;

end.
