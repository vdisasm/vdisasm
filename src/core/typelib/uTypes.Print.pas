unit uTypes.Print;

interface

uses
  uTypes.Base;

function PrintType(const T: TVDType): string;

implementation

uses
  System.SysUtils;

function PrintRecordField(const F: TRecordFieldInfo; PrintFieldOffsets: boolean): string;
var
  bytOfs: uint32;
  bitOfs: byte;
begin
  result := '';
  result := result + '  ';
  if PrintFieldOffsets then
  begin
    bytOfs := F.BitOffset div 8;
    bitOfs := F.BitOffset mod 8;
    if bitOfs = 0 then
      result := result + Format('@%4.4d   ', [bytOfs])
    else
      result := result + Format('@%4.4d:%d ', [bytOfs, bitOfs]);
  end;
  result := result + F.Info.&Type.Name;
  if F.Info.Name <> '' then
    result := result + ' ' + F.Info.Name;
  if F.Info.Comment <> '' then
    result := result + ' // ' + F.Info.Comment;
  result := result + sLineBreak;
end;

function PrintRecordFields(const Fields: TRecordFields; PrintFieldOffsets: boolean): string;
var
  F: TRecordFieldInfo;
begin
  result := '';
  // fields
  for F in Fields do
    result := result + PrintRecordField(F, PrintFieldOffsets);
end;

function PrintRecord(const T: TRecordType; PrintInherited: boolean; PrintFieldOffsets: boolean): string;
begin
  result := '';
  // record comment
  if T.Comment <> '' then
    result := result + '// ' + T.Comment + sLineBreak;
  // rec line
  result := result + 'rec ' + T.Name;
  if T.InheritedRecord <> nil then
    result := result + ':' + T.InheritedRecord.Name;
  result := result + sLineBreak;
  // inherited fields
  if PrintInherited and (T.InheritedRecord <> nil) then
  begin
    result := result + '  // ' + T.InheritedRecord.Name + sLineBreak;
    result := result + PrintRecordFields(T.InheritedRecord.Fields, PrintFieldOffsets);
    if T.Fields.Count <> 0 then
      result := result + '  // ' + T.Name + sLineBreak;
  end;
  // own fields
  result := result + PrintRecordFields(T.Fields, PrintFieldOffsets);
  // end
  result := result + 'end' + sLineBreak;
end;

function PrintArray(const T: TArrayType): string;
begin
  result := '';
  result := result + 'arr ' + T.FieldType.Name + ' ';
  if T.Name <> '' then
    result := result + T.Name + ' ';
  if T.LowerBound = 0 then
    result := result + Format('[%d]', [T.Count])
  else
    result := result + Format('[%d..%d]', [T.LowerBound, T.UpperBound]);
  if T.Comment <> '' then
    result := result + ' // ' + T.Comment;
  result := result + sLineBreak;
end;

function PrintType(const T: TVDType): string;
begin
  if (T is TRecordType) then
    result := PrintRecord(TRecordType(T), True, True)
  else if (T is TArrayType) then
    result := PrintArray(TArrayType(T))
  else
    result := 'unsupported: ' + T.ToString;
end;

end.
