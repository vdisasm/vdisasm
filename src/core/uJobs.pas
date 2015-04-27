unit uJobs;

interface

uses
  System.Generics.Collections,

  uUpdateable,

  VDAPI;

type
  TCustomJob = class
  end;

  TCustomVAJob = class(TCustomJob)
  protected
    FVA: TVA;
  public
    constructor Create(VA: TVA);
  end;

  TMakeFuncJob = class(TCustomVAJob)
  end;

  TJobList = TObjectList<TCustomJob>;

  TVDJobs = class(TVDUpdateable, IVDJobs)
  protected
    FList: TJobList;
    FDirty: boolean;
  public
    constructor Create;
    destructor Destroy; override;
  protected
    { IVDUpdateable }
    procedure SafeNotification;
    procedure DoEndUpdate; override;
  public
    { IVDJobs }
    procedure AddMakeCode(VA: TVA); stdcall;
    procedure AddMakeFunc(VA: TVA); stdcall;

    function GetCount: int; stdcall;
    function Get(Index: int): IVDJob; stdcall;
  end;

implementation

{
  J,Id; Data
}

uses
  System.SysUtils;

{ TVDJobs }

procedure TVDJobs.AddMakeCode(VA: TVA);
begin
  // CoreGet.Log.WriteLn(Format('Add job "MakeCode" at %x', [VA]));
  FDirty := True;
end;

procedure TVDJobs.AddMakeFunc(VA: TVA);
begin
  // CoreGet.Log.WriteLn(Format('Add job "MakeFunc" at %x', [VA]));
  // AddMakeCode(VA);

  // CoreGet().Functions.Add(VA);
  FList.Add(TMakeFuncJob.Create(VA));

  FDirty := True;
end;

constructor TVDJobs.Create;
begin
  inherited Create;
  FList := TJobList.Create;
end;

destructor TVDJobs.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TVDJobs.DoEndUpdate;
begin
  inherited;
  SafeNotification;
end;

function TVDJobs.Get(Index: int): IVDJob;
begin
  Result := nil;
end;

function TVDJobs.GetCount: int;
begin
  Result := FList.Count;
end;

procedure TVDJobs.SafeNotification;
begin
  if FUpdateCount = 0 then
    CoreGet().Msg.Broadcast(MSG_JOBS_CHANGED);
end;

{ TCustomVAJob }

constructor TCustomVAJob.Create(VA: TVA);
begin
  inherited Create;
  self.FVA := VA;
end;

end.
