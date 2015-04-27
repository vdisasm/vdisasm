{
  *  Parent of updateable classes.
  *
  *  Call BeginUpdate before adding items and EndUpdate after it.
  *  DoEndUpdate is called on update end (put final code here).
  *
  *  Typical usage is:
  *
  *  try
  *    some.BeginUpdate;
  *  finally
  *    some.EndUpdate;
  *  end;
}
unit uUpdateable;

interface

uses
  VDAPI;

type
  TVDUpdateable = class(TInterfacedObject, IVDUpdateable)
  protected
    FUpdateCount: integer;
    procedure DoBeginUpdate; virtual; // called once on first BeginUpdate
    procedure DoEndUpdate; virtual;   // called once on last EndUpdate
  public
    { VDAPI, called by user }
    procedure BeginUpdate; stdcall;
    procedure EndUpdate; stdcall;
  end;

implementation

{ TVDUpdateable }

procedure TVDUpdateable.DoBeginUpdate;
begin
  // override
end;

procedure TVDUpdateable.DoEndUpdate;
begin
  // override
end;

procedure TVDUpdateable.BeginUpdate;
begin
  if FUpdateCount = 0 then
    DoBeginUpdate;
  inc(FUpdateCount)
end;

procedure TVDUpdateable.EndUpdate;
begin
  dec(FUpdateCount);
  if FUpdateCount = 0 then
    DoEndUpdate;
end;

end.
