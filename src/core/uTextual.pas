unit uTextual;

interface

uses
  System.SysUtils,
  uUpdateable,
  uDB,
  uStream,
  uStream.MemoryStream,
  VDAPI;

type

  { TVDTextual }

  TVDTextual = class(TVDUpdateable, IVDTextual)
  protected
    FTag: TDBTagCast;

    // If not Null, will also write Text -> VA reference (useful for names).
    // Like secondary index.
    FBackRefTag: TDBTagCast;

    function PrepareKey(c: IVDCore; const VA: TVA; out Key: IVDStreamIO; OutRelVA: PRelVA = nil): boolean;
    function KeyToData(var Bytes: TBytes; out VA: TVA): boolean;
    function ValueToData(var Bytes: TBytes; out Text: BSTR; out Flags: TTextualFlags): boolean;
  public
    function Get(VA: TVA; out Text: BSTR; OutFlags: PTextualFlags = nil): BOOL; stdcall;
    function Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL; virtual; stdcall;
    function Del(VA: TVA): BOOL; stdcall;
    procedure Enumerate(cb: TTextualEnumFunc; ud: Pointer); stdcall;
  end;

  { TVDNames }

  TVDNames = class(TVDTextual, IVDNames)
  private
    function TryPut(c: IVDCore; VA: TVA; Text: BSTR; Flags: TTextualFlags;
      AppendVA: boolean): boolean;
    procedure SafeNotifyChanged;
  public
    constructor Create;
    function GetByText(Text: BSTR; out VA: TVA): BOOL; stdcall;
    function Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL; override;
    function Del(VA: TVA): BOOL; stdcall;
  end;

  { TVDComments }

  TVDComments = class(TVDTextual)
    constructor Create;
  end;

implementation

uses
  uCore,
  BPlusTree.Intf,
  uDB.VA;

{
  Key:  Tag, RelVA
  Data: Flags, Text
}

{ TVDTextual }

function TVDTextual.PrepareKey(c: IVDCore; const VA: TVA; out Key: IVDStreamIO;
  OutRelVA: PRelVA): boolean;
begin
  Key := CreateStreamFromTagAndRelVA(c, FTag, VA, OutRelVA);
  Result := Assigned(Key);
end;

procedure TVDTextual.Enumerate(cb: TTextualEnumFunc; ud: Pointer);
var
  c: TVDCore;
  K, V: TBytes;
  cur: IBPlusTreeCursor;
  t_text: BSTR;
  t_flags: TTextualFlags;
  t_va: TVA;
begin
  if not Assigned(cb) then
    exit;
  c := CoreGet() as TVDCore;
  cur := c.DB.CursorCreateEx(TBytes.Create(Byte(FTag)), [kpGreater], true);
  if cur <> nil then
    while true do
    begin
      K := cur.Key;
      V := cur.Value;
      if ValueToData(V, t_text, t_flags) then
        if KeyToData(K, t_va) then
          if not cb(t_va, t_text, t_flags, ud) then
            exit;
      if not cur.Next then
        exit;
    end;
end;

function TVDTextual.ValueToData(var Bytes: TBytes; out Text: BSTR;
  out Flags: TTextualFlags): boolean;
var
  Value: TVDStreamIO;
begin
  Value := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Bytes));
  try
    if Value.Size < 6 then // flags(4) + len(2)
      exit(False);
    Flags := Value.ReadU32; // flags
    Text := Value.ReadStr;  // text
    exit(true);
  finally
    Value.Free;
  end;
end;

function TVDTextual.Del(VA: TVA): BOOL;
var
  c: TVDCore;
  Key: IVDStreamIO;
  Text: BSTR;
begin
  Result := False;
  c := TVDCore(CoreGet);
  if not PrepareKey(c, VA, Key) then
    exit;

  if FBackRefTag <> DBTAG_Null then
  begin
    if not Get(VA, Text) then
      raise Exception.Create('Error Message');
  end;
  // Base.
  Result := c.DB.Delete((Key as TVDStreamIO).ToBytes) = BP_OK;
  // Back-Ref.
  if FBackRefTag <> DBTAG_Null then
  begin
    Key.Clear;
    Key.WriteU8(FBackRefTag);
    (Key as TVDStreamIO).WriteStr(Text, False);
    Result := c.DB.Delete((Key as TVDStreamIO).ToBytes) = BP_OK;
  end;
end;

function TVDTextual.Get(VA: TVA; out Text: BSTR; OutFlags: PTextualFlags): BOOL;
var
  c: TVDCore;
  Key: IVDStreamIO;
  Bytes: TBytes;
  tmpFlags: TTextualFlags;
begin
  c := TVDCore(CoreGet);
  if not PrepareKey(c, VA, Key) then
    exit(False);

  if c.DB.Get((Key as TVDStreamIO).ToBytes, Bytes) <> BP_OK then
    exit(False);
  Result := ValueToData(Bytes, Text, tmpFlags);
  if Result and (OutFlags <> nil) then
    OutFlags^ := tmpFlags;
end;

function TVDTextual.KeyToData(var Bytes: TBytes; out VA: TVA): boolean;
var
  s: TVDStreamIO;
  RelVA: TRelVA;
begin
  if length(Bytes) < (1 + sizeof(TRelVA)) then
    exit(False);

  s := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Bytes));
  try
    s.Skip();
    s.ReadRelVA(RelVA);
    Result := CoreGet().VM.RelToAbsVA(RelVA, VA);
  finally
    s.Free;
  end;
end;

function TVDTextual.Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL;
var
  c: TVDCore;
  RelVA: TRelVA;
  Key, Value: IVDStreamIO;
begin
  Result := False;
  c := TVDCore(CoreGet);
  if PrepareKey(c, VA, Key, @RelVA) then
  begin
    Value := TVDStreamIO.Create(TVDMemoryStream.Create);
    // Prepare value.
    Value.WriteU32(Flags);
    (Value as TVDStreamIO).WriteStr(Text);
    // Put (base).
    Result := c.DB.Put(
      (Key as TVDStreamIO).ToBytes,
      (Value as TVDStreamIO).ToBytes) = BP_OK;
    // Back-ref.
    if Result then
      if FBackRefTag <> DBTAG_Null then
      begin
        Key.Clear;
        (Key as TVDStreamIO).WriteTag(FBackRefTag);
        (Key as TVDStreamIO).WriteStr(Text, False);
        Value.Clear;
        Value.WriteRelVA(RelVA);
        Result := c.DB.Put(
          (Key as TVDStreamIO).ToBytes,
          (Value as TVDStreamIO).ToBytes) = BP_OK;
      end;
  end;
end;

{ TVDNames }

constructor TVDNames.Create;
begin
  inherited;
  FTag := DBTAG_Name;
  FBackRefTag := DBTAG_NameBackRef;
end;

function TVDNames.Del(VA: TVA): BOOL;
begin
  Result := inherited Del(VA);
  if Result then
    SafeNotifyChanged;
end;

function TVDNames.GetByText(Text: BSTR; out VA: TVA): BOOL;
var
  c: TVDCore;
  Key, Value: TVDStreamIO;
  ValueBytes: TBytes;
  RelVA: TRelVA;
  DB: IBPlusTree;
begin
  c := (CoreGet as TVDCore);
  DB := c.DB;

  if not Assigned(DB) then
    exit(False);

  Key := TVDStreamIO.Create(TVDMemoryStream.Create);
  try
    Key.WriteTag(FBackRefTag);
    Key.WriteStr(Text, False);

    Result := DB.Get(Key.ToBytes, ValueBytes) = BP_OK;
    if Result then
    begin
      Value := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(ValueBytes));
      try
        Value.ReadRelVA(RelVA);
        Result := c.GetVM.RelToAbsVA(RelVA, VA);
      finally
        Value.Free;
      end;
    end;
  finally
    Key.Free;
  end;
end;

function TVDNames.Put(VA: TVA; Text: BSTR; Flags: TTextualFlags): BOOL;
var
  TmpVA: TVA;
  c: IVDCore;
begin
  Result := False;
  if GetByText(Text, TmpVA) then
  begin
    // modify existing name
    if (TmpVA <> VA) then
    begin
      c := CoreGet();
      // Duplicate.
      c.Log.WriteLn(Format('Name duplicate for 0x%x at 0x%x. Renaming.', [VA, TmpVA]));
      // Try append rel.va.
      Result := TryPut(c, VA, Text, Flags, true);
      if not Result then
        c.Log.WriteLn('Failed to append unique address to name.');
    end;
  end
  else
    // put new name
    Result := inherited Put(VA, Text, Flags);

  if Result then
    SafeNotifyChanged;
end;

procedure TVDNames.SafeNotifyChanged;
begin
  if FUpdateCount = 0 then
    CoreGet().Msg.Broadcast(MSG_NAMES_CHANGED);
end;

function TVDNames.TryPut(c: IVDCore; VA: TVA; Text: BSTR; Flags: TTextualFlags;
  AppendVA: boolean): boolean;
var
  s: string;
  RelVA: TRelVA;
begin
  if not AppendVA then
  begin
    Result := inherited Put(VA, Text, Flags);
    exit;
  end;

  // Append rel.va.
  if not c.VM.AbsToRelVA(VA, RelVA) then
    exit(False);
  s := Format('%s_%x_%x', [Text, RelVA.SecID, RelVA.SecOfs]);
  Result := inherited Put(VA, s, Flags);
end;

{ TVDComments }

constructor TVDComments.Create;
begin
  inherited;
  FTag := DBTAG_Comment;
end;

end.
