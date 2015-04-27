unit uDumpExports;

interface

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  System.SysUtils,

  VDAPI;

procedure DumpDatabaseExports(c: IVDCore; const FileName: string);

implementation

uses
  Xml.XmlIntf,
  Xml.XmlDoc;

type
  TItem = record
    ordinal: integer;
    name: string;
  end;

  TItems = TList<TItem>;

  TCtx = record
    doc: IXMLDocument;
    items: TItems;
  end;

  // PCtx = ^TCtx;

function ExportSymbolsEnumFunc(
  VA: TVA;
  SymbolName: BSTR_IN;
  ordinal: TVDSymbolOrdinal;
  ud: Pointer): BOOL; stdcall;
var
  item: TItem;
begin
  if ordinal <> 0 then
  begin
    item.ordinal := ordinal;
    item.name := SymbolName;
    TItems(ud).Add(item);
  end;
  result := True;
end;

procedure DumpDatabaseExports;
(*
var
  ctx: TCtx;
  item: TItem;
  n, nRoot, nDesc, nItems: TXmlNode;
  fn: string;
*)
begin
(*
  c := CoreGet();

  ctx.items := TItems.Create;
  ctx.doc := TNativeXml.CreateEx(nil, True, False, True, 'exports');
  try
    ctx.doc.XmlFormat := xfReadable;

    nRoot := ctx.doc.Root;

    nDesc := nRoot.NodeNew('desc');
    fn := c.InputFile.FileName;
    fn := ExtractFileName(fn);
    nDesc.WriteAttributeString('filename', utf8string(fn));

    nItems := nRoot.NodeNew('items');

    // collect items
    c.ExportSymbols.Enumerate(BAD_VA, BAD_VA, ExportSymbolsEnumFunc, ctx.items);

    // sort items
    ctx.items.Sort(TComparer<TItem>.Construct(
      function(const l, r: TItem): integer
      begin
        if l.ordinal > r.ordinal then
          result := 1
        else if l.ordinal < r.ordinal then
          result := -1
        else
          result := 0;
      end
      ));

    // write items
    for item in ctx.items do
    begin
      n := nItems.NodeNew('');
      n.Value := utf8string(item.name);
      n.WriteAttributeInteger('#', item.ordinal);
    end;

    ctx.doc.SaveToFile(FileName);
  finally
    ctx.items.Free;
    ctx.doc.Free;
  end;
*)
end;

end.
