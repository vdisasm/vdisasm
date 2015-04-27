{
  Implements IVDSearch
}
unit uCore.Search;

interface

uses
  System.Character,
  System.SysUtils,
  VDAPI,
  uVMSearch;

type
  TVDSearch = class(TInterfacedObject, IVDSearch)
  public
    function Pattern(
      VA: PVA;
      EndVA: TVA;
      Pattern: IVDBytePattern;
      Direction: Int): BOOL; stdcall;

    function Bytes(
      VA: PVA;
      EndVA: TVA;
      PatternText: BSTR_IN;
      Direction: Int): BOOL; stdcall;

    function &String(
      VA: PVA;
      EndVA: TVA;
      Text: BSTR_IN;
      CodePage: TCodePage;
      CaseSensitive: BOOL; Direction: Int): BOOL; stdcall;
  end;

implementation


{ TVDSearch }

function TVDSearch.Bytes(VA: PVA; EndVA: TVA; PatternText: BSTR_IN;
  Direction: Int): BOOL;
begin
  Result := uVMSearch.FindPatternBytes(CoreGet(), VA, EndVA, PatternText, Direction);
end;

function TVDSearch.Pattern(VA: PVA; EndVA: TVA; Pattern: IVDBytePattern;
  Direction: Int): BOOL;
begin
  Result := uVMSearch.FindPattern(CoreGet(), VA, EndVA, Pattern, Direction);
end;

function TVDSearch.&String(VA: PVA; EndVA: TVA; Text: BSTR_IN;
  CodePage: TCodePage; CaseSensitive: BOOL; Direction: Int): BOOL;
begin
  Result := uVMSearch.FindString(CoreGet(), VA, EndVA, Text, CodePage, CaseSensitive, Direction);
end;

end.
