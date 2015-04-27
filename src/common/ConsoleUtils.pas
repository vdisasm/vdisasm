unit ConsoleUtils;

interface

procedure Cls;

implementation

uses
  WinApi.Windows;

procedure Cls;
var
  hConsole: HWND;
  ScreenBufInfo: TConsoleScreenBufferInfo;
  coord: TCoord;
  dwConSize, dwTmp: dword;
begin
  // http://support.microsoft.com/kb/99261

  // get console handle
  hConsole := GetStdHandle(STD_OUTPUT_HANDLE);

  // get the number of character cells in the current buffer
  if not GetConsoleScreenBufferInfo(hConsole, ScreenBufInfo) then
    exit;

  dwConSize := ScreenBufInfo.dwSize.X * ScreenBufInfo.dwSize.Y;

  coord.X := 0;
  coord.Y := 0;

  // fill the entire screen with blanks
  if not FillConsoleOutputCharacter(hConsole, ' ', dwConSize, coord, dwTmp) then
    exit;

  // get the current text attribute
  if not GetConsoleScreenBufferInfo(hConsole, ScreenBufInfo) then
    exit;

  // now set the buffer's attributes accordingly
  if not FillConsoleOutputAttribute(hConsole, ScreenBufInfo.wAttributes, dwConSize, coord, dwTmp) then
    exit;

  // put the cursor at (0, 0)
  SetConsoleCursorPosition(hConsole, coord);
end;

end.
