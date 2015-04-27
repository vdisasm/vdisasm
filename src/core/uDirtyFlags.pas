unit uDirtyFlags;

interface

type
  TDirtyFlag =
    (
    DirtyCoreData,
    DirtySectionsData
    );

  TDirtyFlags = set of TDirtyFlag;

implementation

end.
