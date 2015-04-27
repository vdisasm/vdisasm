unit uStream.MemoryStream;

interface


uses
  System.Classes,
  System.SysUtils,
  VDAPI,
  uStream;

type
  TDerivedMemoryStream = class(TMemoryStream);

  TVDMemoryStream = class(TInterfacedObject, IVDBaseStream, IVDMemoryStream)
  private
    FMs: TDerivedMemoryStream;
  public
    constructor Create;

    constructor CreateFromMemory(Mem: PByte; Size: UInt32);

    // Map stream to bytes.
    constructor CreateFromBytes(var Bytes: TBytes);

    destructor Destroy; override;

    procedure LoadFromFile(const FileName: string);
    procedure SaveToFile(const FileName: string);
    procedure LoadFromStream(const Stream: TVDMemoryStream);
  public

    { IVDBaseStream }

    function GetPosition: UInt64; stdcall;
    procedure SetPosition(Value: UInt64); stdcall;
    function GetSize: UInt64; stdcall;
    function SetSize(Value: UInt64): BOOL; stdcall;
    function Read(var Buffer; Size: UInt32): UInt32; stdcall;
    function Write(const Buffer; Size: UInt32): UInt32; stdcall;

    { IVDMemoryStream }

    function GetMemory: Pointer; stdcall;

    // property Memory: Pointer read GetMemory;
  end;

implementation

constructor TVDMemoryStream.Create;
begin
  inherited Create;
  FMs := TDerivedMemoryStream.Create;
end;

destructor TVDMemoryStream.Destroy;
begin
  FreeAndNil(FMs);
  inherited;
end;

constructor TVDMemoryStream.CreateFromBytes(var Bytes: TBytes);
begin
  CreateFromMemory(@Bytes[0], Length(Bytes));
end;

constructor TVDMemoryStream.CreateFromMemory(Mem: PByte; Size: UInt32);
begin
  Create;
  FMs.SetPointer(Mem, Size);
end;

function TVDMemoryStream.GetMemory: Pointer;
begin
  Result := FMs.Memory;
end;

procedure TVDMemoryStream.LoadFromFile(const FileName: string);
var
  fn: string;
begin
  fn := IOGet.ExpandPath(FileName);
  FMs.LoadFromFile(fn);
end;

procedure TVDMemoryStream.LoadFromStream(const Stream: TVDMemoryStream);
begin
  FMs.LoadFromStream(Stream.FMs);
  FMs.Position := 0;
end;

procedure TVDMemoryStream.SaveToFile(const FileName: string);
var
  fn: string;
begin
  fn := IOGet.ExpandPath(FileName);
  FMs.SaveToFile(fn);
end;

function TVDMemoryStream.GetPosition: UInt64;
begin
  Result := FMs.Position;
end;

function TVDMemoryStream.GetSize: UInt64;
begin
  Result := FMs.Size;
end;

function TVDMemoryStream.Read(var Buffer; Size: UInt32): UInt32;
begin
  Result := FMs.Read(Buffer, Size);
end;

procedure TVDMemoryStream.SetPosition(Value: UInt64);
begin
  FMs.Position := Value;
end;

function TVDMemoryStream.SetSize(Value: UInt64): BOOL;
begin
  FMs.Size := Value;
  Result := FMs.Size = Value;
end;

function TVDMemoryStream.Write(const Buffer; Size: UInt32): UInt32;
begin
  Result := FMs.Write(Buffer, Size);
end;

end.
