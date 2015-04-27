unit uRememberedVAs;

interface

uses
  System.Generics.Collections,
  System.SysUtils,
  glinkedlist,
{$IFDEF DIAG_VACHANGE}
  ConsoleUtils,
{$ENDIF}
  VDAPI;

const
  REMEBERED_VA_MAX = 32; // Must be > 2

{$IF REMEBERED_VA_MAX < 3}
{$MESSAGE FATAL 'REMEBERED_VA_MAX MUST BE >= 3'}
{$IFEND}


type
  TVAPosLinkedList = TLinkedList<TVDVAPos>;

  TRememberedVAs = class
  private
    FList: TVAPosLinkedList;

    // If it point to Nil, list must be rotated left.
    FCurItem: TVAPosLinkedList.PItem;
    FCurIndex: integer;
    FDirty: boolean;
  public

    constructor Create;
    destructor Destroy; override;

    procedure Init;

    // Put VA at current position. If list is full, remove 1st VA and add VA
    // at end.
    procedure Put(const Value: TVDVAPos);

    // Move to next VA if possible.
    function Next(out Value: TVDVAPos): boolean;

    // Move to previous VA if possible.
    function Prev(out Value: TVDVAPos): boolean;

    function GetCurrent(out Value: TVDVAPos): boolean;
    procedure SetCurrent(const Value: TVDVAPos);

    function SaveToDb: boolean;
    function LoadFromDb: boolean;

{$IFDEF DIAG_VACHANGE}
    procedure Dump(const Caption: string);
{$ENDIF}
  end;

implementation

uses
  uCore,
  uDB,
  uStream,
  uStream.MemoryStream,
  BPlusTree.Intf;

{ TRememberedVAs }

constructor TRememberedVAs.Create;
begin
  inherited Create;
  FList := TVAPosLinkedList.Create;
  Init;
end;

function TRememberedVAs.Next(out Value: TVDVAPos): boolean;
begin
  if (FCurItem = nil) or (FCurItem.Next = nil) or
    (FCurItem.Next.Data.ScrVA = BAD_VA) then
    Exit(False);
  FCurItem := FCurItem.Next;
  Inc(FCurIndex);
  Value := FCurItem.Data;
  Exit(True);
end;

function TRememberedVAs.Prev(out Value: TVDVAPos): boolean;
begin
  if (FCurItem = nil) or (FCurItem.Prev = nil) then
    Exit(False);
  FCurItem := FCurItem.Prev;
  Dec(FCurIndex);
  Value := FCurItem.Data;
  Exit(True);
end;

procedure TRememberedVAs.Put(const Value: TVDVAPos);
begin
  // Move next.
  if not Assigned(FCurItem) then
  begin
    FCurItem := FList.First;
    FCurIndex := 0;
  end
  else
  begin
    FCurItem := FCurItem.Next;
    if Assigned(FCurItem) then
      Inc(FCurIndex)
    else
    begin
      FList.RotateLeft;
      FCurItem := FList.Last;
    end;
  end;
  // Put.
  FCurItem.Data := Value;
  FDirty := True;
{$IFDEF DIAG_VACHANGE}
  Dump('Put');
{$ENDIF}
end;

function TRememberedVAs.GetCurrent(out Value: TVDVAPos): boolean;
begin
  if FCurItem <> nil then
  begin
    Value := FCurItem.Data;
    Exit(True);
  end;
  Exit(False);
end;

destructor TRememberedVAs.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TRememberedVAs.Init;
var
  i: integer;
  dummy: TVDVAPos;
begin
  dummy.ScrVA := BAD_VA;
  dummy.X := 0;
  dummy.CurVA := BAD_VA;

  FList.Clear;
  for i := 0 to REMEBERED_VA_MAX - 1 do
    FList.InsertLast(dummy);

  FCurItem := nil;
  FCurIndex := -1;

  FDirty := True;
end;

function TRememberedVAs.LoadFromDb: boolean;
var
  Data: TVDStreamIO;
  Value: TBytes;
  i: integer;
  item: TVAPosLinkedList.PItem;
begin
  Init;
  Result := (CoreGet as TVDCore).DB.Get(TBytes.Create(DBTAG_RemeberedVAs), Value) = BP_OK;
  if Result then
  begin
    Data := TVDStreamIO.Create(TVDMemoryStream.CreateFromBytes(Value));
    try
      FCurIndex := Int32(Data.ReadU32);
      FCurItem := nil;
      item := FList.First;
      for i := 0 to REMEBERED_VA_MAX - 1 do
      begin
        item.Data.ScrVA := Data.ReadVA;
        item.Data.CurVA := Data.ReadVA;
        item.Data.X := Int16(Data.ReadU16);

        if i = FCurIndex then
          FCurItem := item;

        item := item.Next;
      end;
      FDirty := False;
    finally
      Data.Free;
    end;
  end;
{$IFDEF DIAG_VACHANGE}
  Dump('LoadFromDb');
{$ENDIF}
end;

function TRememberedVAs.SaveToDb: boolean;
var
  Data: TVDStreamIO;
  pos: TVDVAPos;
begin
  if not FDirty then
    Exit(True);

  Data := TVDStreamIO.Create(TVDMemoryStream.Create);
  try
    Data.WriteU32(UInt32(FCurIndex));

    for pos in FList do
    begin
      Data.WriteVA(pos.ScrVA);
      Data.WriteVA(pos.CurVA);
      Data.WriteU16(uint16(pos.X));
    end;

    Result := (CoreGet as TVDCore).DB.Put(TBytes.Create(DBTAG_RemeberedVAs), Data.ToBytes) = BP_OK;

    if Result then
      FDirty := False;
  finally
    Data.Free;
  end;

{$IFDEF DIAG_VACHANGE}
  Dump('SaveToDb');
{$ENDIF}
end;

procedure TRememberedVAs.SetCurrent(const Value: TVDVAPos);
begin
  FCurItem^.Data := Value;
  FDirty := True;
{$IFDEF DIAG_VACHANGE}
  Dump('SetCurrent');
{$ENDIF}
end;

{$IFDEF DIAG_VACHANGE}


procedure TRememberedVAs.Dump(const Caption: string);
var
  pos: TVDVAPos;
  s, scursor: string;
  i: integer;
begin
  ConsoleUtils.Cls;
  writeln(Caption);
  writeln('Dirty = ', FDirty);
  i := 0;
  for pos in FList do
  begin
    if i = FCurIndex then
      scursor := '> '
    else
      scursor := '  ';
    s := Format('%s scr:%x cur:%x x:%d', [scursor, pos.ScrVA, pos.CurVA, pos.X]);
    writeln(s);
    Inc(i);
  end;
end;
{$ENDIF}


end.
