unit uTypes.Pointers;

interface

uses
  VDAPI,
  uTypes.Base;

type
  TVDPointerType = class(TVDType)
  private
    FPointedType: TVDType; // nil for void pointers
  public
    constructor Create(Lib: IVDTypeLibrary; const Name: string; BitSize: TVDBitSize; PointedType: TVDType = nil);
    function IsVoidPtr: boolean; inline;
    property PointedType: TVDType read FPointedType;
  end;

implementation

{ TPointerType }

constructor TVDPointerType.Create(Lib: IVDTypeLibrary; const Name: string; BitSize: TVDBitSize; PointedType: TVDType);
begin
  inherited Create(Lib, Name, BitSize);
  FTypeKind := TYPEKIND_Pointer;
  FPointedType := PointedType;
end;

function TVDPointerType.IsVoidPtr: boolean;
begin
  Result := FPointedType = nil;
end;

end.
