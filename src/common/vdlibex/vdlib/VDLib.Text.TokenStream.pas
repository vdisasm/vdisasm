unit VDLib.Text.TokenStream;

interface

uses
  VDLib.Text.Stream,
  VDLib.Trie;

type
  {
    * Abstract stream for reading token.
  }
  TAbstractTokenStream = class(TTextStreamEx)
  public const
    BAD_TOKEN = 0;
  public type
    TTokenId = type integer;
    TTokenFlag = (token_dont_need_whitespace);
    TTokenFlags = set of TTokenFlag;
  protected type
    TTokenData = record
      id: TTokenId;
      flags: TTokenFlags;
    end;

    TTokenTrie = TCharTrie<TTokenData>;
  protected
    FTokenTrie: TTokenTrie;
  public
    // Read/Peek token with text.
    function ReadToken(Region: TAbstractTextStream.PTextRegion; out TokenId: TTokenId): boolean; overload;
    function ReadToken(out Text: string; out TokenId: TTokenId): boolean; overload;

    function PeekToken(out Text: string; out TokenId: TTokenId): boolean; overload;

    function PeekToken: TTokenId; overload;

    // Try read token using existing trie.
    // Always try longest possible match.
    // Stop at whitespace.
    function TryReadTokenByTrie(out TokenId: TTokenId): boolean;
    function TryPeekTokenByTrie(out TokenId: TTokenId): boolean;
  public
    constructor Create;
    destructor Destroy; override;

    { To override }

    function ReadToken: TTokenId; overload; virtual; abstract;
  end;

implementation

{ TAbstractTokenStream }

function TAbstractTokenStream.ReadToken(Region: TAbstractTextStream.PTextRegion;
  out TokenId: TTokenId): boolean;
var
  tok: TTokenId;
  pos0, pos1: TTextStreamEx.TOffset;
begin
  pos0 := self.Position; // before token
  tok := ReadToken();
  pos1 := self.Position; // after token

  if tok = BAD_TOKEN then
  begin
    self.Position := pos0;
    exit(false);
  end;

  TokenId := tok;

  if Assigned(Region) then
  begin
    Region^.Offset := pos0;
    Region^.Size := pos1 - pos0;
  end;

  exit(True);
end;

function TAbstractTokenStream.ReadToken(out Text: string; out TokenId: TTokenId): boolean;
var
  rgn: TTextRegion;
begin
  Result := ReadToken(@rgn, TokenId);
  if Result then
    Text := ReadTextFromRegion(rgn)
  else
    Text := '';
end;

function TAbstractTokenStream.PeekToken(out Text: string; out TokenId: TTokenId): boolean;
var
  pos0: TOffset;
begin
  pos0 := self.Position;
  try
    Result := ReadToken(Text, TokenId);
  finally
    self.Position := pos0;
  end;
end;

function TAbstractTokenStream.PeekToken: TTokenId;
var
  pos0: TOffset;
begin
  pos0 := self.Position;
  try
    Result := ReadToken;
  finally
    self.Position := pos0;
  end;
end;

constructor TAbstractTokenStream.Create;
begin
  inherited Create;
  FTokenTrie := TTokenTrie.Create;
end;

destructor TAbstractTokenStream.Destroy;
begin
  FTokenTrie.Free;
  inherited;
end;

function TAbstractTokenStream.TryPeekTokenByTrie(out TokenId: TTokenId): boolean;
var
  OldPos: TOffset;
begin
  OldPos := Position;
  Result := TryReadTokenByTrie(TokenId);
  Position := OldPos;
end;

function TAbstractTokenStream.TryReadTokenByTrie(
  out TokenId: TTokenId): boolean;
var
  ofs: integer;
  c: char;
  node, bestNode: TTokenTrie.TNode;
  bestNodeOfs: integer;
  data: TTokenData;
begin
  ofs := 0;
  node := FTokenTrie.Root;
  bestNode := nil;
  bestNodeOfs := 0;
  while Assigned(node) and Peek(c, ofs) and (not IsWhiteSpace(c)) do
  begin
    inc(ofs);

    node := FTokenTrie.FindSubNode(node, c);

    if Assigned(node) then
    begin
      if node.HasData then
      begin
        bestNode := node;
        bestNodeOfs := ofs;
      end;

      // If it's stopper (i.e. trie has no more sub-brances) there is no sense
      // to scan any further.
      if node.IsStopper then
        break;
    end;
  end;

  // If we couldn't find any node.
  if not Assigned(bestNode) then
    exit(false);

  if not bestNode.GetData(data) then
    exit(false);

  // If token need whitespaces only.
  if not(token_dont_need_whitespace in data.flags) then
  begin
    // If there is no whitespace or line feed after found token text it means
    // found text is just a part of some word, not separate token.
    // For example:
    // 'unit': valid (if whitespace is next)
    // 'unitless': invalid, it just starts with 'unit'
    Peek(c, ofs);
    if not(IsWhiteSpace(c) or IsDelimiter(c) or IsLineFeed(c)) then
      exit(false);
  end;

  // Node found. Skip token and return result.
  Skip(bestNodeOfs);

  TokenId := data.id;
  Result := True;
end;

end.
