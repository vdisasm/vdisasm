unit uBytePattern;

interface

uses
  VDAPI;

type
  TVDBytePattern = class(TInterfacedObject, IVDBytePattern)
  private
    FMask: array of Byte;
    FData: array of Byte;
  public
    constructor Create(SrcBuf: PByte; SrcSize: UInt32; Mask: Byte);
  public
    // Get count of pattern bytes.
    function GetSize: UInt; stdcall;
    // Get pointer to pattern bytes.
    function GetData: PByte; stdcall;
    // Get byte value used as pattern mask.
    function GetMask: PByte; stdcall;
  end;

implementation

{ TVDBytePattern }

constructor TVDBytePattern.Create(SrcBuf: PByte; SrcSize: UInt32; Mask: Byte);
begin
  if SrcSize <> 0 then
  begin
    SetLength(FMask, SrcSize);
    FillChar(FMask, SrcSize, $FF);

    SetLength(FData, SrcSize);
    Move(SrcBuf^, FData[0], SrcSize);
  end;
end;

function TVDBytePattern.GetData: PByte;
begin
  Result := @FData[0];
end;

function TVDBytePattern.GetMask: PByte;
begin
  Result := @FMask[0];
end;

function TVDBytePattern.GetSize: UInt;
begin
  Result := Length(FData);
end;

end.
