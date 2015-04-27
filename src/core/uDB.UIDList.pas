{
  List of unique-keys with arbitary values.
  UID #0 - store LastId

  On top of the class can be built:
  - job list
  - problem list
}
unit uDB.UIDList deprecated;

interface

uses
  System.SysUtils,
  uDB;

// todo: uDB.UIDList

const
  BAD_UID = 0;

type
  TDBUIDList = class
  private
    // set by parent class
    // FTag: TDBTag;

    FLastId: UInt32;
    procedure LoadLastIdFromDb;
    procedure Put(Id: UInt32; const Value: TBytes);
  public
    function GetLastId: UInt32;

    // Add value with new unique id
    procedure Add(const Value: TBytes);
  end;

implementation

{ TDBUIDList }

procedure TDBUIDList.Add(const Value: TBytes);
begin
  if FLastId = BAD_UID then
    LoadLastIdFromDb;
end;

function TDBUIDList.GetLastId: UInt32;
begin
  result:=0;
end;

procedure TDBUIDList.LoadLastIdFromDb;
begin

end;

procedure TDBUIDList.Put(Id: UInt32; const Value: TBytes);
begin

end;

end.
