{
  Generic double linked list for Delphi and FPC.

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to
  deal in the Software without restriction, including without limitation the
  rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
  sell copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
  IN THE SOFTWARE.
}

unit glinkedlist;

{$IFDEF FPC}
{$MODE DELPHI}
{$ENDIF}

interface

{$IFNDEF FPC}


uses
  System.Generics.Collections,
  System.SysUtils;
{$ELSE}

uses
  SysUtils;

type
  // Delphi compatible types.
  TCollectionNotification = (cnAdded, cnRemoved, cnExtracted);
  TCollectionNotifyEvent<T> = procedure(Sender: TObject; const Item: T;
    Action: TCollectionNotification) of object;
{$ENDIF}


type

  { TLinkedList }

  TLinkedList<T> = class
  type
    PItem = ^TItem;

    TItem = record
    private
      List: TLinkedList<T>; // owner
    public
      Data: T;
      Prev: PItem;
      Next: PItem;
      function IsFirst: boolean; inline;
      function IsLast: boolean; inline;
      function IsSingle: boolean; inline;
      function InsertAfter(const Value: T): PItem; inline;
      function InsertBefore(const Value: T): PItem; inline;
    end;

    TTraverseFunc = function(Item: PItem; ud: pointer): boolean;
  private
    FCount: integer;
    FFirst, FLast: PItem;
    FOnNotify: TCollectionNotifyEvent<T>;
  protected
    procedure DoNotify(const Item: T; Action: TCollectionNotification);
    procedure Traverse(cb: TTraverseFunc; ud: pointer);

    // Following Link/Unlink functions do not modify Count or call Notification.
    procedure LinkAfter(Pos, Item: PItem); inline;
    procedure LinkBefore(Pos, Item: PItem); inline;
    procedure Unlink(Item: PItem); inline;

    // Go from first to last item and list count.
    // It can be used to check if count field is correct and
    // in some operations like Split.
    function CalculateCount: integer;
  public
    destructor Destroy; override;

    procedure Clear;
    procedure Delete(Item: PItem);

    // Insert Value to start of the list.
    function InsertFirst(const Value: T): PItem;

    // Insert Value to end of the list.
    function InsertLast(const Value: T): PItem;

    function InsertAfter(Item: PItem; const Value: T): PItem;
    function InsertBefore(Item: PItem; const Value: T): PItem;

    // First item moved to end.
    procedure RotateLeft;

    // Last item moved to begin.
    procedure RotateRight;

    // Split linked list by Item into already existing list.
    // Right part of splitted list will be stored in NewList.
    procedure Split(Item: PItem; NewList: TLinkedList<T>); overload;

    // Split into new list.
    function Split(Item: PItem): TLinkedList<T>; overload;

    property Count: integer read FCount;
    property First: PItem read FFirst;
    property Last: PItem read FLast;
    property OnNotify: TCollectionNotifyEvent<T> read FOnNotify write FOnNotify;

  type

    { TEnumerator }

    TEnumerator = class
    private
      FList: TLinkedList<T>;
      FCurrent: PItem;
    protected
      function DoGetCurrent: T;
      function DoMoveNext: boolean;
    public
      constructor Create(AList: TLinkedList<T>);
      function MoveNext: boolean;
      property Current: T read DoGetCurrent;
    end;

  function GetEnumerator: TEnumerator; reintroduce;

  end;

implementation

{ TLinkedList<T>.TItem }

function TLinkedList<T>.TItem.InsertAfter(const Value: T): PItem;
begin
  Result := List.InsertAfter(@self, Value);
end;

function TLinkedList<T>.TItem.InsertBefore(const Value: T): PItem;
begin
  Result := List.InsertBefore(@self, Value);
end;

function TLinkedList<T>.TItem.IsFirst: boolean;
begin
  Result := not Assigned(Prev);
end;

function TLinkedList<T>.TItem.IsLast: boolean;
begin
  Result := not Assigned(Next);
end;

function TLinkedList<T>.TItem.IsSingle: boolean;
begin
  Result := IsFirst and IsLast;
end;

{ TLinkedList<T> }

destructor TLinkedList<T>.Destroy;
begin
  Clear;
  Inherited;
end;

procedure TLinkedList<T>.DoNotify(const Item: T; Action: TCollectionNotification);
begin
  if Assigned(FOnNotify) then
    FOnNotify(self, Item, Action);
end;

procedure TLinkedList<T>.Clear;
var
  Next: PItem;
  OldValue: T;
begin
  if (FCount <> 0) then
  begin
    while Assigned(FFirst) do
    begin
      OldValue := FFirst^.Data;
      Next := FFirst^.Next;
      Dispose(FFirst);
      FFirst := Next;
      if FFirst = nil then
        FLast := nil;
      dec(FCount);
      DoNotify(OldValue, cnRemoved);
    end;
  end;
end;

procedure TLinkedList<T>.Delete(Item: PItem);
begin
  if Assigned(Item) then
  begin
    Unlink(Item);
    dec(FCount);
    DoNotify(Item^.Data, cnRemoved);
    Dispose(Item);
  end;
end;

procedure TLinkedList<T>.Traverse(cb: TTraverseFunc; ud: pointer);
var
  Cur, Next: PItem;
begin
  if Assigned(cb) then
  begin
    Cur := First;
    while Assigned(Cur) do
    begin
      Next := Cur^.Next;
      if not cb(Cur, ud) then
        break;
      Cur := Next;
    end;
  end;
end;

procedure TLinkedList<T>.Unlink(Item: PItem);
begin
  if Item^.IsFirst then
    FFirst := Item^.Next
  else
    Item^.Prev^.Next := Item^.Next;

  if Item^.IsLast then
    FLast := Item^.Prev
  else
    Item^.Next^.Prev := Item^.Prev;
end;

function TLinkedList<T>.CalculateCount: integer;
var
  Item: PItem;
begin
  Result := 0;
  Item := self.First;
  while Assigned(Item) do
  begin
    inc(Result);
    Item := Item.Next;
  end;
end;

function TLinkedList<T>.InsertFirst(const Value: T): PItem;
begin
  if FCount <> 0 then
    Exit(InsertBefore(FFirst, Value));

  // List is empty: add first item.
  new(Result);
  Result^.List := self;
  Result^.Data := Value;

  Result^.Prev := nil;
  Result^.Next := nil;
  FFirst := Result;
  FLast := Result;

  inc(FCount);
  DoNotify(Value, cnAdded);
end;

function TLinkedList<T>.InsertAfter(Item: PItem; const Value: T): PItem;
begin
  if Assigned(Item) then
  begin
    new(Result);
    Result^.List := self;
    Result^.Data := Value;
    LinkAfter(Item, Result);
    inc(FCount);
    DoNotify(Value, cnAdded);
    Exit;
  end;
  Exit(nil);
end;

function TLinkedList<T>.InsertBefore(Item: PItem; const Value: T): PItem;
begin
  if Assigned(Item) then
  begin
    new(Result);
    Result^.List := self;
    Result^.Data := Value;
    LinkBefore(Item, Result);
    inc(FCount);
    DoNotify(Value, cnAdded);
    Exit;
  end;
  Exit(nil);
end;

function TLinkedList<T>.InsertLast(const Value: T): PItem;
begin
  if FCount = 0 then
    Result := InsertFirst(Value)
  else
    Result := InsertAfter(FLast, Value);
end;

procedure TLinkedList<T>.LinkAfter(Pos, Item: PItem);
var
  PosNext: PItem;
begin
  PosNext := Pos^.Next;
  Pos^.Next := Item;
  if Assigned(PosNext) then
    PosNext^.Prev := Item
  else
    FLast := Item;
  Item^.Prev := Pos;
  Item^.Next := PosNext;
end;

procedure TLinkedList<T>.LinkBefore(Pos, Item: PItem);
var
  PosPrev: PItem;
begin
  PosPrev := Pos^.Prev;
  Pos^.Prev := Item;
  if Assigned(PosPrev) then
    PosPrev^.Next := Item
  else
    FFirst := Item;
  Item^.Prev := PosPrev;
  Item^.Next := Pos;
end;

procedure TLinkedList<T>.RotateLeft;
var
  tmp: PItem;
begin
  if FCount > 1 then
  begin
    tmp := FFirst;
    Unlink(tmp);
    LinkAfter(FLast, tmp);
  end;
end;

procedure TLinkedList<T>.RotateRight;
var
  tmp: PItem;
begin
  if FCount > 1 then
  begin
    tmp := FLast;
    Unlink(tmp);
    LinkBefore(FFirst, tmp);
  end;
end;

procedure TLinkedList<T>.Split(Item: PItem; NewList: TLinkedList<T>);
begin
  if Item = self.First then
    raise Exception.Create('List cannot be split by first item.');

  NewList.FFirst := Item;
  NewList.FLast := self.FLast;
  NewList.FCount := NewList.CalculateCount;

  self.FLast := Item.Prev;
  Item.Prev.Next := nil;
  Item.Prev := nil;
  dec(self.FCount, NewList.FCount);
end;

function TLinkedList<T>.Split(Item: PItem): TLinkedList<T>;
begin
  result := TLinkedList<T>.Create;
  Split(Item, result);
end;


constructor TLinkedList<T>.TEnumerator.Create(AList: TLinkedList<T>);
begin
  inherited Create;
  FList := AList;
  FCurrent := nil;
end;

function TLinkedList<T>.TEnumerator.MoveNext: boolean;
begin
  Result := DoMoveNext;
end;

function TLinkedList<T>.TEnumerator.DoGetCurrent: T;
begin
  Result := FCurrent.Data;
end;

function TLinkedList<T>.TEnumerator.DoMoveNext: boolean;
begin
  if not Assigned(FCurrent) then
  begin
    FCurrent := FList.First;
    Result := Assigned(FCurrent);
    Exit;
  end;
  Result := Assigned(FCurrent.Next);
  if Result then
    FCurrent := FCurrent.Next;
end;

function TLinkedList<T>.GetEnumerator: TEnumerator;
begin
  Result := TEnumerator.Create(self);
end;

end.
