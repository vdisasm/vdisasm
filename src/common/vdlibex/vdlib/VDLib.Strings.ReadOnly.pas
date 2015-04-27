{
  Read-only string.
}
unit VDLib.Strings.ReadOnly;

interface

uses
  System.Generics.Collections,
  System.StrUtils,
  System.SysUtils;

const
  INDENT_VALUE = 2;

type
  TReadOnlyStrings = class
  type
    TOnChange = procedure(Sender: TObject) of object;
  private type
    TStringItem = record
      At, Size: integer;
    end;

    TStringItems = TList<TStringItem>;
  private
    FText: string;
    FItems: TStringItems;
    FOnChange: TOnChange;
    FNotFinishedLineAt: integer;
    function GetCount: integer; inline;
    function GetLine(I: integer): string;
    procedure AddItem(At, Size: integer);
    procedure DoChange; inline;
    function GetLastItem(out Item: TStringItem): boolean; inline;
    function SetLastItem(const Item: TStringItem): boolean; inline;
  private
    FIndent: integer;
    FIndentString: string;
    function GetIndent: integer;
    procedure SetIndent(const Value: integer);
    function GetIsInMiddleOfLine: boolean; inline;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Append(const Str: string); // inline;
    procedure AppendChar(c: char);
    procedure Clear; inline;

    procedure IndentEnter;
    procedure IndentLeave;

    property Count: integer read GetCount;
    property Lines[I: integer]: string read GetLine; default;
    property OnChange: TOnChange read FOnChange write FOnChange;
    property Text: string read FText;
    property Indent: integer read GetIndent write SetIndent;
    property IsInMiddleOfLine: boolean read GetIsInMiddleOfLine;
  end;

implementation

const
  FIRST_INDEX    = 1;
  EOL            = [#13, #10];
  BAD_CHAR_INDEX = -1;

  { TVDStrings }

procedure TReadOnlyStrings.AddItem(At, Size: integer);
var
  I: TStringItem;
begin
  I.At := At;
  I.Size := Size;
  FItems.Add(I);
end;

procedure TReadOnlyStrings.Append(const Str: string);
var
  P: PChar;
  Ofs, Len: integer;
  Itm: TStringItem;
  tStr: string;
begin
  if Str = '' then
    Exit;

  tStr := ReplaceStr(Str, #13, '');

  if FNotFinishedLineAt <> BAD_CHAR_INDEX then
  begin
    Ofs := FNotFinishedLineAt;
  end
  else
  begin
    // It's new line.
    if FIndentString <> '' then
      FText := FText + FIndentString;
    Ofs := Length(FText) + FIRST_INDEX;
    AddItem(Ofs, 0);
  end;

  FText := FText + tStr;
  Len := Length(FText);

  // Search EOF or EOL.
  P := @FText[Ofs];
  while True do
  begin
    // if EOL...
    if CharInSet(P^, EOL) then
    begin
      // Update last item.
      GetLastItem(Itm);
      Itm.Size := Ofs - Itm.At;
      SetLastItem(Itm);

      // while (P^ in EOL) and (Ofs <= Len) do
      begin
        inc(Ofs);
        inc(P);
      end;

      if Ofs > Len then
      begin
        FNotFinishedLineAt := BAD_CHAR_INDEX;
        break;
      end
      else
      begin
        FNotFinishedLineAt := Ofs;
        continue;
      end;

    end;

    // if EOF...
    if Ofs > Len then
    begin
      // Update last item.
      GetLastItem(Itm);
      Itm.Size := Ofs - Itm.At;
      SetLastItem(Itm);

      FNotFinishedLineAt := Ofs;

      break;
    end;
    inc(Ofs);
    inc(P);
  end;

  DoChange;
end;

procedure TReadOnlyStrings.AppendChar(c: char);
begin
  FText := FText + c;
end;

procedure TReadOnlyStrings.Clear;
begin
  FItems.Clear;
  FText := '';
  FNotFinishedLineAt := BAD_CHAR_INDEX;
  DoChange;
end;

constructor TReadOnlyStrings.Create;
begin
  FItems := TStringItems.Create;
  FNotFinishedLineAt := BAD_CHAR_INDEX;
end;

destructor TReadOnlyStrings.Destroy;
begin
  FItems.Free;
  inherited;
end;

procedure TReadOnlyStrings.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

function TReadOnlyStrings.GetCount: integer;
begin
  result := FItems.Count;
end;

function TReadOnlyStrings.GetLine(I: integer): string;
begin
  result := Copy(FText, FItems[I].At, FItems[I].Size);
end;

function TReadOnlyStrings.GetIndent: integer;
begin
  result := FIndent;
end;

function TReadOnlyStrings.GetIsInMiddleOfLine: boolean;
begin
  result := FNotFinishedLineAt <> -1;
end;

procedure TReadOnlyStrings.SetIndent(const Value: integer);
begin
  if Value <> FIndent then
  begin
    FIndent := Value;
    FIndentString := string.Create(' ', Value);
  end;
end;

procedure TReadOnlyStrings.IndentEnter;
begin
  Indent := Indent + INDENT_VALUE;
end;

procedure TReadOnlyStrings.IndentLeave;
begin
  Indent := Indent - INDENT_VALUE;
end;

function TReadOnlyStrings.SetLastItem(const Item: TStringItem): boolean;
begin
  if FItems.Count > 0 then
  begin
    FItems[FItems.Count - 1] := Item;
    Exit(True);
  end;
  Exit(False);
end;

function TReadOnlyStrings.GetLastItem(out Item: TStringItem): boolean;
begin
  if FItems.Count > 0 then
  begin
    Item := FItems[FItems.Count - 1];
    Exit(True);
  end;
  Exit(False);
end;

end.
