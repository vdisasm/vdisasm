unit uFunctions;

interface

uses
  VDAPI;

type
  TVDFunctions = class(TInterfacedObject, IVDFunctions)
  public
    function IsFunctionStart(VA: TVA): BOOL; stdcall;
    function Add(VA: TVA): BOOL; stdcall;
  end;

implementation

uses
  System.SysUtils,
  uCore,
  uDB,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf;

// stream must be freed by caller
function CreateFuncKey(const c: IVDCore; VA: TVA): TVDStreamIO;
var
  RelVA: TRelVA;
begin
  result := nil;
  if c.VM.AbsToRelVA(VA, RelVA) then
  begin
    result := TVDStreamIO.Create(TVDMemoryStream.Create);
    result.WriteTag(DBTAG_Func);
    result.WriteRelVA(RelVA);
  end;
end;

{ TVDFunctions }

function TVDFunctions.IsFunctionStart(VA: TVA): BOOL;
var
  Key: TVDStreamIO;
  c: IVDCore;
begin
  c := CoreGet;
  Key := CreateFuncKey(c, VA);
  if not Assigned(Key) then
    Exit(False);

  try
    result := (c as TVDCore).DB.ContainsKey(Key.ToBytes);
  finally
    Key.Free;
  end;
end;

function TVDFunctions.Add(VA: TVA): BOOL;
var
  Key: TVDStreamIO;
  c: IVDCore;
  Bytes: TBytes;
begin
  c := CoreGet;
  Key := CreateFuncKey(c, VA);
  if not Assigned(Key) then
    Exit(False);

  try
    Bytes := Key.ToBytes;

    // if func already exists?
    if (c as TVDCore).DB.ContainsKey(Bytes) then
    begin
      c.Log.WriteLn(Format('Function at %x already exists', [VA]));
      Exit(False);
    end;

    result := (c as TVDCore).DB.Put(Bytes, nil) = BP_OK;

  finally
    Key.Free;
  end;
end;

end.
