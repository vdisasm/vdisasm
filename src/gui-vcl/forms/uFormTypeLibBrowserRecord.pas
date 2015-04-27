unit uFormTypeLibBrowserRecord;

interface

uses
  System.Generics.Collections,

  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,

  VDAPI;

type
  TFieldRecord = record
    VA: TVA;
    BitOfs: Byte;
    Name: string;
    TypeName: string;
    Value: string;
    procedure Clear;
  end;

  TFieldList = TList<TFieldRecord>;

  TFormTypeLibBrowserRecord = class(TForm)
    lvFields: TListView;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure lvFieldsAdvancedCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; Stage: TCustomDrawStage;
      var DefaultDraw: Boolean);
    procedure lvFieldsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvFieldsData(Sender: TObject; Item: TListItem);
    procedure lvFieldsDblClick(Sender: TObject);
  private
    FCore: IVDCore; // tmp
    FCurVA: TVA;
    FBitOfs: uint32;
    FDecodeVA: Boolean;
    FBaseFlag: NativeUInt;
    FRecTypeName: string;

    FFieldList: TFieldList;

    function GetRec: IVDRecordType;
    procedure DoFieldChanged;
    // Update value text for item with Index.
    procedure RefreshCurrentFieldValue(Index: integer);
  protected
    procedure DoDisplay;
  public
    OnFieldChanged: TNotifyEvent;

    function NeedShowValues: Boolean; inline;
    procedure TryInsertField;
    procedure RenameField;

    procedure TryEditFieldValue(const Rec: TFieldRecord);
  end;

  // var
  // FormTypeLibBrowserRecord: TFormTypeLibBrowserRecord;

function DisplayRecord(
  const Rec: IVDRecordType;
  VA: TVA; MousePos: TPoint;
  VA_IS_OFFSET: Boolean // no need decoding
  ): TFormTypeLibBrowserRecord;

implementation

uses
  uCore.Strings,
  uEditField;

const
  FL_INHERITED = 1 shl 0;

  SI_NAME  = 0;
  SI_TYPE  = 1;
  SI_VALUE = 2;

{$R *.dfm}

function EnumField(BitOfs: TVDBitSize; Name, Comment: BSTR;
  FieldType: IVDType; ud: pointer): BOOL; stdcall;
var
  Provider: IVDTypeProviderBase;
  f: TFormTypeLibBrowserRecord;
  procedure AddFieldValue(const Name: string);
  var
    L: IVDVATextLayout;
//    Flags: NativeUInt;
    VA: TVA;
    BitOfs: Byte;
    Rec: TFieldRecord;
  begin
//    Flags := f.FBaseFlag;

    Rec.Clear;

    VA := f.FCurVA + (f.FBitOfs div 8);
    BitOfs := f.FBitOfs mod 8;

    Rec.VA := VA;
    Rec.BitOfs := BitOfs;

    // name
    Rec.Name := Name;

    // type
    Rec.TypeName := FieldType.Name;

    // value
    if f.FDecodeVA then
    begin
      Provider := f.FCore.TypeMgr.GetProvider(BSTR_IN(FieldType.Name));
      if Assigned(Provider) then
      begin
        // value
        L := CreateVATextLayout(TVDTextFlag.Plain);
        { Size := } Provider.Decode(VA, TVDDataEncoding.Text, pointer(L));
        Rec.Value := L.Get;
        L := nil;
      end
      else
        Rec.Value := Format('? (%s)', [FieldType.Name]);
    end
    else
      Rec.Value := '';

    f.FFieldList.Add(Rec);

    inc(f.FBitOfs, FieldType.Bitsize);
  end;

begin
  f := TFormTypeLibBrowserRecord(ud);
  AddFieldValue(Name);
  Result := True;
end;

// todo: rec maybe not needed
function DisplayRecord(const Rec: IVDRecordType; VA: TVA; MousePos: TPoint;
  VA_IS_OFFSET: Boolean): TFormTypeLibBrowserRecord;
begin
  Result := TFormTypeLibBrowserRecord.Create(Application);
  Result.Left := MousePos.X;
  Result.Top := MousePos.Y;
  Result.caption := Rec.Name;

  Result.FRecTypeName := Rec.Name;
  Result.FCore := CoreGet();
  Result.FCurVA := VA;
  Result.FBitOfs := 0;

  Result.FDecodeVA := not VA_IS_OFFSET;

  Result.DoDisplay;

  Result.Show;
end;

{ TFormTypeLibBrowserRecord }

procedure TFormTypeLibBrowserRecord.DoDisplay;
var
  inh: IVDRecordType;
  Rec: IVDRecordType;
begin
  Rec := GetRec;
  if Rec = nil then
    exit;

  lvFields.Items.BeginUpdate;
  lvFields.Clear;
  FFieldList.Clear;
  try
    inh := Rec.GetInherited;
    if inh <> nil then
    begin
      FBaseFlag := FL_INHERITED;
      inh.EnumFields(EnumField, self);
    end;

    FBaseFlag := 0;
    Rec.EnumFields(EnumField, self);

    lvFields.Items.Count := FFieldList.Count;
  finally
    lvFields.Items.EndUpdate;
  end;
end;

procedure TFormTypeLibBrowserRecord.DoFieldChanged;
begin
  // current field changed
  if Assigned(lvFields.Selected) then
  begin
    RefreshCurrentFieldValue(lvFields.Selected.Index);
    if Assigned(OnFieldChanged) then
      OnFieldChanged(self);
  end;
end;

procedure TFormTypeLibBrowserRecord.FormCreate(Sender: TObject);
begin
  FFieldList := TFieldList.Create;
end;

procedure TFormTypeLibBrowserRecord.FormDestroy(Sender: TObject);
begin
  FFieldList.Free;
end;

procedure TFormTypeLibBrowserRecord.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_ESCAPE:
      Close;
  end;
end;

function TFormTypeLibBrowserRecord.GetRec: IVDRecordType;
begin
  Result := CoreGet.TypeLib.FindType(nil, BSTR_IN(FRecTypeName), False) as IVDRecordType;
end;

procedure TFormTypeLibBrowserRecord.lvFieldsAdvancedCustomDrawItem(
  Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  Flags: NativeUInt;
begin
  Flags := NativeUInt(Item.Data);
  if (Flags and FL_INHERITED) <> 0 then
  begin
    Sender.Canvas.Font.Color := clGrayText;
  end;
end;

procedure TFormTypeLibBrowserRecord.lvFieldsData(Sender: TObject;
  Item: TListItem);
var
  Rec: TFieldRecord;
begin
  Rec := FFieldList[Item.Index];
  Item.caption := IntToHex(Rec.VA, 1);
  Item.SubItems.Add(Rec.Name);
  Item.SubItems.Add(Rec.TypeName);
  Item.SubItems.Add(Rec.Value);
end;

procedure TFormTypeLibBrowserRecord.lvFieldsDblClick(Sender: TObject);
var
  Rec: TFieldRecord;
begin
  if Assigned(lvFields.Selected) then
  begin
    Rec := FFieldList[lvFields.Selected.Index];
    TryEditFieldValue(Rec);
  end;
end;

procedure TFormTypeLibBrowserRecord.lvFieldsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
  Rec: TFieldRecord;
begin
  if Assigned(lvFields.Selected) then
  begin
    Rec := FFieldList[lvFields.Selected.Index];

    case Key of
      VK_RETURN:
        TryEditFieldValue(Rec);
      VK_INSERT:
        TryInsertField;
      Ord('N'):
        RenameField;
    end;
  end;
end;

function TFormTypeLibBrowserRecord.NeedShowValues: Boolean;
begin
  Result := FCurVA <> BAD_VA;
end;

procedure TFormTypeLibBrowserRecord.RefreshCurrentFieldValue(Index: integer);
var
  Rec: TFieldRecord;
  c: IVDCore;
  prov: IVDTypeProviderBase;
  L: IVDVATextLayout;
begin
  c := CoreGet;
  Rec := FFieldList[Index];
  prov := c.TypeMgr.GetProvider(BSTR_IN(Rec.TypeName));
  if prov = nil then
  begin
    c.Log.WriteLn('Failed to find type provider');
    exit;
  end;
  L := CreateVATextLayout(TVDTextFlag.Plain);
  if prov.Decode(Rec.VA, TVDDataEncoding.Text, pointer(L)) = 0 then
  begin
    c.Log.WriteLn('Decoding failed');
    exit;
  end;
  Rec.Value := L.Get;
  FFieldList[index] := Rec;
  lvFields.Invalidate;
end;

procedure TFormTypeLibBrowserRecord.RenameField;
var
  s: string;
  r: IVDRecordType;
begin
  if lvFields.Selected = nil then
    exit;

  r := GetRec;
  if r = nil then
  begin
    CoreGet().Log.WriteLn('Failed to find record. ');
    exit;
  end;

  s := lvFields.Selected.SubItems[SI_NAME];
  if InputQuery(SFieldName, SFieldName, s) and (s <> '') then
  begin
    // todo: TFormTypeLibBrowserRecord.RenameField
    DoDisplay;
  end;
end;

procedure TFormTypeLibBrowserRecord.TryEditFieldValue(const Rec: TFieldRecord);
begin
  if uEditField.TryEditFieldText(Rec.VA, 0, Rec.TypeName) then
    DoFieldChanged;
end;

procedure TFormTypeLibBrowserRecord.TryInsertField;
var
  arr: array of string;
begin
  if lvFields.Selected = nil then
    exit;
  SetLength(arr, 2);
  if InputQuery(SNewField, [SFieldName, SFieldType], arr) and
    (arr[0] <> '') and (arr[1] <> '') then
  begin
    raise Exception.Create('not implemented');
  end;
end;

{ TFieldRecord }

procedure TFieldRecord.Clear;
begin
  self.VA := 0;
  self.BitOfs := 0;
  self.Name := '';
  self.TypeName := '';
  self.Value := '';
end;

end.
