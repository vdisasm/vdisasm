unit uUI;

interface

uses
  VDAPI;

type
  TVDUserInterface = class(TInterfacedObject, IVDUserInterface)
  public
    function SelectFromList(Selector: IVDItemSelector): Int; stdcall;
    procedure RepaintView(ViewType: UInt); stdcall;
    procedure SetGuiRefreshState(Enabled: BOOL); stdcall;
    function GetSelectedRange([ref] VA0, VA1: TVA; Flags: UInt32): BOOL; stdcall;
  end;

implementation

uses
  uCore;

{ TVDUserInterface }

procedure TVDUserInterface.RepaintView(ViewType: UInt);
begin
  CoreGet.Msg.Broadcast(MSG_UI_REPAINT_VIEW, Pointer(ViewType));
end;

function TVDUserInterface.SelectFromList(Selector: IVDItemSelector): Int;
var
  rec: TVDItemSelectorRecord;
begin
  rec.Selector := Selector;
  rec.ResultIndex := -1;
  CoreGet.Msg.Broadcast(TVDMessage.MSG_UI_SELECT_ITEM, @rec);
  Result := rec.ResultIndex;
end;

procedure TVDUserInterface.SetGuiRefreshState(Enabled: BOOL);
var
  c: IVDCore;
begin
  c := CoreGet;
  if Enabled then
    c.Msg.Broadcast(MSG_CHANGE_GUI_REFRESH_STATE, Pointer(1))
  else
    c.Msg.Broadcast(MSG_CHANGE_GUI_REFRESH_STATE, Pointer(0));
end;

function TVDUserInterface.GetSelectedRange([ref] VA0, VA1: TVA; Flags: UInt32): BOOL;
var
  range: TVDSelVARange;
  c: IVDCore;
  tmpVA0, tmpVA1: TVA;
  sec: IVDSection;
begin
  range.VA0 := BAD_VA;
  range.VA1 := BAD_VA;

  c := CoreGet;

  c.Msg.Broadcast(MSG_UI_GET_SELRANGE, @range);

  Result := (range.VA0 <> BAD_VA) and (range.VA1 <> BAD_VA);
  if Result then
  begin
    VA0 := range.VA0;
    VA1 := range.VA1;
    exit;
  end;

  if (Flags and TUISelRangeFlags.DEFAULT_SELECT_ALL_VA) <> 0 then
  begin
    if c.VM.GetFirstVA(@tmpVA0) and c.VM.GetLastVA(@tmpVA1) then
    begin
      VA0 := tmpVA0;
      VA1 := tmpVA1;
      Result := true;
    end;
    exit;
  end;

  if (Flags and TUISelRangeFlags.DEFAULT_SELECT_SECTION) <> 0 then
  begin
    sec := c.VM.Sections.Find(c.GetUserVA);
    if Assigned(sec) then
    begin
      VA0 := sec.GetStartVA;
      VA1 := sec.GetLastVA;
      Result := true;
    end;
    exit;
  end;
end;

end.
