unit uUiUtils;

interface

uses
  WinApi.Windows,
  Vcl.Forms;

procedure FlashWindowAndBeep(wnd: HWND; doBeep: boolean = True); overload;
procedure FlashWindowAndBeep(form: TForm; doBeep: boolean = True); overload;

procedure PopupNotification(const bounds: TRect; const caption: string);

implementation

procedure FlashWindowAndBeep(wnd: HWND; doBeep: boolean);
var
  inf: FLASHWINFO;
begin
  if doBeep then
    MessageBeep(0);

  inf.cbSize := sizeof(inf);
  inf.HWND := wnd;
  inf.dwFlags := FLASHW_ALL;
  inf.uCount := 3;
  inf.dwTimeout := 100;

  FlashWindowEx(inf);
end;

procedure FlashWindowAndBeep(form: TForm; doBeep: boolean = True);
begin
  FlashWindowAndBeep(form.Handle, doBeep);
end;

procedure PopupNotification;
begin

end;

end.
