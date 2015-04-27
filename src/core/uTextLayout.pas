unit uTextLayout;

interface

uses
  System.Classes,
  System.SysUtils,
  VDLib.Strings.ReadOnly,
  VDAPI;

type
  TLines = TReadOnlyStrings;

  TVDTextLayout = class(TInterfacedObject, IVDTextLayout)
  private
    FStringList: TStringList;
    FLines: TLines;
    FFlags: TVDTextFlags;
    FColumnId: Int16;
    FIndent: integer;
    FIndentStr: string;
    procedure AddTextExInternal(Text: BSTR_IN; TagId: TTag; MaxLines: Int); inline;
  public
    constructor Create(Flags: TVDTextFlags);
    destructor Destroy; override;

    function IsPlain: boolean; inline;
  public
    procedure Clear; stdcall;
    procedure Skip(CntChars: uint8 = 1); stdcall;
    procedure AddChar(Value: WideChar; TagId: TTag = TTag.TAGID_NONE); stdcall;
    procedure AddText(Text: BSTR_IN; TagId: TTag); stdcall;
    procedure AddTextEx(Text: BSTR_IN; TagId: TTag; MaxLines: Int); stdcall;
    procedure AddColumn; stdcall;
    procedure AddField(Id: uint8); stdcall;
    procedure Append(Text: BSTR); stdcall;
    procedure LineBreak; stdcall;
    function GetColumnId: Int; stdcall;
    function GetLineCount: Int; stdcall;
    function Get: BSTR; stdcall;
    function GetLine(Index: Int): BSTR; stdcall;
    procedure IndentEnter; stdcall;
    procedure IndentLeave; stdcall;
  end;

implementation

{ TVDTextLayout }

procedure TVDTextLayout.AddChar(Value: WideChar; TagId: TTag);
var
  str: string;
begin
  str := Value;
  AddTextExInternal(BSTR_IN(str), TagId, 0);
end;

procedure TVDTextLayout.AddColumn;
begin
  if IsPlain then
    FLines.Append(' ')
  else
    FLines.Append(WideChar(byte(TTag.TAG_TAB_COLUMN) shl 8)); // Tagged.

  inc(FColumnId);
end;

procedure TVDTextLayout.AddField(Id: uint8);
var
  w: word;
begin
  if not IsPlain then
  begin
    w := (byte(TTag.TAG_FIELD_ID) shl 8) or Id;
    FLines.AppendChar(WideChar(w));
  end;
end;

procedure TVDTextLayout.AddText(Text: BSTR_IN; TagId: TTag);
begin
  AddTextExInternal(Text, TagId, 0);
end;

procedure TVDTextLayout.AddTextEx(Text: BSTR_IN; TagId: TTag; MaxLines: Int);
begin
  AddTextExInternal(Text, TagId, MaxLines);
end;

procedure TVDTextLayout.AddTextExInternal(Text: BSTR_IN; TagId: TTag;
  MaxLines: Int);
var
  L: string;
  i: integer;
  bLastLine, bMaxLinesReached: boolean;
begin
  if (Text = nil) or (Text = '') then
    exit;

  // Parse text -> split lines
  FStringList.Text := Text;

  for i := 0 to FStringList.Count - 1 do
  begin
    L := FStringList[i];

    if FIndent <> 0 then
      if not FLines.IsInMiddleOfLine then
        L := FIndentStr + L;

    if IsPlain then
    begin
      FLines.Append(L);
    end
    else
    begin
      FLines.Append(WideChar(byte(TTag.TAG_ID) shl 8 + byte(TagId)));
      FLines.Append(WideChar(byte(TTag.TAG_TEXT_LEN) shl 8 + length(L)));
      FLines.Append(L);
    end;

    bLastLine := i = (FStringList.Count - 1);
    bMaxLinesReached := (MaxLines <> 0) and (i = (MaxLines - 1));

    if (not bLastLine) and (not bMaxLinesReached) then
      Self.LineBreak;

    if bMaxLinesReached then
    begin
      if (not bLastLine) then
        AddText('...', TTag.TAGID_NONE);
      break;
    end;
  end;
end;

procedure TVDTextLayout.Append(Text: BSTR);
begin
  FLines.Append(Text);
end;

procedure TVDTextLayout.Clear;
begin
  FLines.Clear;
  FColumnId := 0;
end;

procedure TVDTextLayout.Skip(CntChars: uint8);
begin
  if IsPlain then
    AddTextExInternal(BSTR_IN(string.Create(' ', CntChars)), TTag.TAG_NONE, 0)
  else
    FLines.Append(WideChar(byte(TTag.TAG_SKIP) shl 8 + CntChars));
end;

procedure TVDTextLayout.LineBreak;
begin
  FLines.Append(#10);
  FColumnId := 0;
end;

constructor TVDTextLayout.Create(Flags: TVDTextFlags);
begin
  FFlags := Flags;
  FLines := TLines.Create;
  FStringList := TStringList.Create;
end;

destructor TVDTextLayout.Destroy;
begin
  FStringList.Free;
  FLines.Free;
  inherited;
end;

function TVDTextLayout.GetColumnId: Int;
begin
  Result := FColumnId;
end;

function TVDTextLayout.GetLine(Index: Int): BSTR;
begin
  Result := FLines[Index];
end;

function TVDTextLayout.GetLineCount: Int;
begin
  Result := FLines.Count;
end;

function TVDTextLayout.Get: BSTR;
begin
  Result := FLines.Text;
end;

procedure TVDTextLayout.IndentEnter;
begin
  inc(FIndent, 2);
  FIndentStr := string.Create(' ', FIndent);
end;

procedure TVDTextLayout.IndentLeave;
begin
  dec(FIndent, 2);
  FIndentStr := string.Create(' ', FIndent);
end;

function TVDTextLayout.IsPlain: boolean;
begin
  Result := (FFlags and TVDTextFlag.Tags) = 0;
end;

end.
