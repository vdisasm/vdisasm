unit VDLib.Math.ValuePairs;

interface

type
  TValuePair<T> = class
  public type
    TCompare = reference to function(const A, B: T): integer;
  public
    class procedure Swap(var A, B: T); inline; static;
    class procedure Order(var A, B: T; Compare: TCompare); inline; static;
  end;

implementation

{ TValuePair<T> }

class procedure TValuePair<T>.Order(var A, B: T; Compare: TCompare);
var
  tmp: T;
begin
  if Compare(A, B) > 0 then
  begin
    tmp := A;
    A := B;
    B := tmp;
  end;
end;

class procedure TValuePair<T>.Swap(var A, B: T);
var
  tmp: T;
begin
  tmp := A;
  A := B;
  B := tmp;
end;

end.
