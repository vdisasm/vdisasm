unit uUserData;

interface

uses
  VDAPI;

type
  TVDUserData = class(TInterfacedObject, IVDUserData)
    // Put Key/Value pair.
    function Put(Key: Pointer; KeySize: UInt32; Data: Pointer; DataSize: UInt32): BOOL; stdcall;
    // DataSize [in/out] size of Data buffer, out is size of whole data.
    // If result is False, Key not found.
    function Get(Key: Pointer; KeySize: UInt32; Data: Pointer; DataSize: PUInt): BOOL; stdcall;
    // Result is True if key found and deleted.
    function Del(Key: Pointer; KeySize: UInt32): BOOL; stdcall;
  end;

implementation

uses
  uCore,
  uDB,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf;

function Init(Key: Pointer; KeySize: UInt32; out c: TVDCore; out KeyStream: IVDMemoryStream): boolean;
var
  io: TVDStreamIO;
begin
  Result := False;
  KeyStream := nil;
  if (KeySize = 0) then
    exit;
  c := (CoreGet as TVDCore);
  if c = nil then
    exit;
  if not c.IsDatabaseOpened then
    exit;
  KeyStream := TVDMemoryStream.Create;
  io := TVDStreamIO.Create(KeyStream);
  try
    io.WriteTag(DBTAG_UserData);
    io.WriteBuf(Key, KeySize);
    exit(True);
  finally
    io.Free;
  end;
end;

{ TVDUserData }

function TVDUserData.Del(Key: Pointer; KeySize: UInt32): BOOL;
var
  c: TVDCore;
  s: IVDMemoryStream;
begin
  Result := False;
  if Init(Key, KeySize, c, s) then
  begin
    Result := c.DB.DeleteRaw(s.GetMemory, s.GetSize) = BP_OK;
  end;
end;

function TVDUserData.Get(Key: Pointer; KeySize: UInt32; Data: Pointer;
  DataSize: PUInt): BOOL;
var
  c: TVDCore;
  s: IVDMemoryStream;
  RealSize: integer;
begin
  Result := False;
  if (DataSize <> nil) then
    if (DataSize^ <> 0) then
      if Init(Key, KeySize, c, s) then
      begin
        RealSize := c.DB.GetRaw(s.GetMemory, s.GetSize, Data, DataSize^);
        DataSize^ := RealSize;
        Result := RealSize <> 0;
      end;
end;

function TVDUserData.Put(Key: Pointer; KeySize: UInt32; Data: Pointer;
  DataSize: UInt32): BOOL;
var
  c: TVDCore;
  s: IVDMemoryStream;
begin
  Result := False;
  if DataSize <> 0 then
    if Init(Key, KeySize, c, s) then
    begin
      Result := c.DB.PutRaw(s.GetMemory, s.GetSize, Data, DataSize) = BP_OK;
    end;
end;

end.
