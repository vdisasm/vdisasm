unit uDB.VA;

interface

uses
  VDAPI,
  uDB,
  uStream,
  uStream.MemoryStream;

function CreateStreamFromTagAndRelVA(c: IVDCore; Tag: TDBTagCast; const VA: TVA;
  { out/opt } OutRelVA: PRelVA = nil): IVDStreamIO;

implementation

function CreateStreamFromTagAndRelVA(c: IVDCore; Tag: TDBTagCast;
  const VA: TVA; OutRelVA: PRelVA): IVDStreamIO;
var
  RelVA: TRelVA;
begin
  // Tag, RelVA
  if not c.GetVM.AbsToRelVA(VA, RelVA) then
    Exit(nil);
  if Assigned(OutRelVA) then
    OutRelVA^ := RelVA;
  Result := TVDStreamIO.Create(TVDMemoryStream.Create);
  Result.WriteU8(Tag);
  Result.WriteRelVA(RelVA);
end;

end.
