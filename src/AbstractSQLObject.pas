{*******************************************************}
{               Abstract SQL Object                     }
{*******************************************************}
{               Author: Antonio Borges                  }
{     https://github.com/asgBorges/AbstractSQLObject    }
{*******************************************************}

unit AbstractSQLObject;

interface

uses SysUtils, Classes, System.TypInfo, Uni, Variants, DB, DBAccess, DBClient,
     System.Generics.Collections, Rtti;

type
  TForEachExtendedListFunc = reference to procedure(const AExtendedList: TRttiInstanceType; AChildObject: TObject);

  TAbstractSQLObject = class;
  TAbstractSQLObjectClass = class of TAbstractSQLObject;

  TExtendedList<T: class> = class(TObjectList<T>)
  private
    FOwner: TAbstractSQLObject;
    FChildTable: String;
    FJoinColumnsFrom: TStringList; //Armazena o noma das colunas de Join da tabela Pai
    FJoinColumnsTo: TStringList; //Armazena o noma das colunas de Join tabela Filha
    FSortColumns: TStringList;
    FSQL: String;
  public
    constructor Create(ATableOwner: TAbstractSQLObject;
                       AJoinColumnsFrom: Array of String;
                       AJoinColumnsTo: Array of String;
                       ASortColumns: Array of String); overload;
    destructor Destroy; override;

    procedure Load;
    function Novo: T;
    procedure Delete(Item: T);
    function FindBy(AColumns: Array of String;
                    AValues: Array of Variant): T;
    function GetMaxInt(AColumnName: String): Int64;
    function GetLinkedJoinColumnTo(AColumnNameFrom: String): String;
  end;

  TAbstractSQLObjectValue = class;

  TAbstractSQLObjectValue = class(TObject)
  private
    FChanged: Boolean;
    FColumnType: TFieldType;
    FValue: Variant;
    FPrimaryKey: Boolean;
    FSequenceName: String;
    function GetIsEmpty: Boolean;
    procedure SetValue(AValue: Variant);

    property ColumnType: TFieldType read FColumnType write FColumnType;
    property Value: Variant read FValue write SetValue;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsChanged: Boolean read FChanged;
    property PrimaryKey: Boolean read FPrimaryKey write FPrimaryKey;
    property SequenceName: String read FSequenceName write FSequenceName;
  public
    procedure Sum(AValue: Double);
  end;

  TAbstractSQLObject = class(TObject)
  private
    FModificado: Boolean; //controla se algum campo foi modificado
    FNovoRegistro: Boolean; //controla se é um novo registro ou se foi carregado do banco
    FConnection: TUniConnection;
    FValues: TStringList;
    FMarkedForDelete: Boolean;
    function GetColumnIndex(AColumnName: String): Integer;
    function GetVariantValueToSql(AVariant: Variant): String;
    procedure ForEachExtendedList(ACallBack: TForEachExtendedListFunc);
    procedure PropagatePrimaryKeyValue(const AColumnName: String; AValue: Variant);
    procedure FillPrimaryKeys;
    procedure FillColumnsWithSequence;
  protected
    procedure AddColumn(AColumnName: String; AColumnType: TFieldType; APrimaryKey: Boolean = False; ASequenceName: String = '');
    procedure SaveChilds;
    procedure LoadChilds;
  public
    constructor Create(AConnection: TUniConnection);
    destructor Destroy; override;

    function AddChild<T: class>(AJoinColumnsFrom: Array of String;
                                AJoinColumnsTo: Array of String;
                                ASortColumns: Array of String): TExtendedList<T>;
    procedure SetSequence(AColumnName, ASequenceName: String);
    function GetTableName: String; virtual; abstract;
    procedure CreateColumns; virtual; abstract;

    procedure SetValue(const AColumnName: String; AValue: Variant);
    function GetValue(const AColumnName: String): Variant;
    function GetColumn(const AColumnName: String): TAbstractSQLObjectValue;

    property Connection: TUniConnection read FConnection;
    property Values[const AColumnName: string]: Variant read GetValue write SetValue; default;
    property Columns[const AColumnName: string]: TAbstractSQLObjectValue read GetColumn;
    property NovoRegistro: Boolean read FNovoRegistro write FNovoRegistro;

    function Save(AIsChild: Boolean = False; ASaveChilds: Boolean = True): Boolean;
    function Load(AColumnNames: Array of String; AValues: Array of Variant; ALoadChilds: Boolean = True): Boolean; virtual;

    function IsNullOrEmpty(const AColumnName: String): Boolean;
    function GetSequence(ASequenceName: String): Int64;

    property MarkedForDelete: Boolean read FMarkedForDelete write FMarkedForDelete default False;
  end;

function DateStringToDate(dt: String) : TDate;
procedure FreeObjects(ListObjects: TStrings);

implementation

uses StrUtils;

procedure FreeObjects(ListObjects: TStrings);
var
  int_Aux: Integer;
  obj_Aux: TObject;
begin
  if Assigned(ListObjects) then
  begin
    for int_Aux := 0 to ListObjects.Count - 1 do
    begin
      obj_Aux := ListObjects.Objects[int_Aux];
      if Assigned(obj_Aux) then
      begin
        FreeAndNil(obj_Aux);
      end;
    end;
    ListObjects.Clear;
  end;
end;

function DateStringToDate(dt: String) : TDate;
begin
  if Trim(dt)='' then
  begin
    Result := 0;
  end else
  begin
    Result := EncodeDate(
      StrToInt(Copy(dt, 1, 4)),
      StrToInt(Copy(dt, 5, 2)),
      StrToInt(Copy(dt, 7, 2))
    );
  end;
end;

{ TAbstractSQLObject }

procedure TAbstractSQLObject.AddColumn(AColumnName: String;
  AColumnType: TFieldType; APrimaryKey: Boolean; ASequenceName: String);
var
  vColumn: TAbstractSQLObjectValue;
begin
  vColumn := TAbstractSQLObjectValue.Create;
  vColumn.ColumnType := AColumnType;
  vColumn.PrimaryKey := APrimaryKey;
  vColumn.SequenceName := ASequenceName;
  FValues.AddObject(AColumnName, vColumn);
end;

constructor TAbstractSQLObject.Create(AConnection: TUniConnection);
begin
  FModificado := False;
  FNovoRegistro := True;

  FConnection := AConnection;
  FValues := TStringList.Create;

  inherited Create;

  CreateColumns;
end;

destructor TAbstractSQLObject.Destroy;
begin
  FreeObjects(FValues);
  FreeAndNil(FValues);

  inherited;
end;


procedure TAbstractSQLObject.FillColumnsWithSequence;
var
  I: Integer;
  vValue: TAbstractSQLObjectValue;
begin
  for I := 0 to FValues.Count-1 do
  begin
    vValue := TAbstractSQLObjectValue(FValues.Objects[I]);
    if (vValue.IsEmpty and (vValue.SequenceName <> '')) then
    begin
      vValue.Value := GetSequence(vValue.SequenceName);
      FModificado := True;
    end;
  end;
end;

procedure TAbstractSQLObject.FillPrimaryKeys;
var
  I: Integer;
  vValue: TAbstractSQLObjectValue;
begin
  for I := 0 to FValues.Count-1 do
  begin
    vValue := TAbstractSQLObjectValue(FValues.Objects[I]);
    if vValue.PrimaryKey then
    begin
      PropagatePrimaryKeyValue(FValues[I], vValue.Value);
    end;
  end;
end;

procedure TAbstractSQLObject.ForEachExtendedList(
  ACallBack: TForEachExtendedListFunc);
var
  vTypeInfo: PTypeInfo;
  vTypeData: PTypeData;
  vPropList: PPropList;
  vPropInfo: PPropInfo;

  I: Integer;
  vChildObject: TObject;
  vChildClass: TClass;

  vExtendedList: TRttiInstanceType;
begin
  try
    vTypeInfo := Self.ClassInfo;
    vTypeData := GetTypeData(vTypeInfo);
    New(vPropList);
    try
      GetPropList(vTypeInfo, tkProperties, vPropList, False);

      for I := 0 to vTypeData^.PropCount-1 do
      //for I := vTypeData^.PropCount-1 downto 0 do
      begin
        vPropInfo := vPropList^[i];
        case vPropInfo^.PropType^.Kind of
          tkClass: begin
                     vChildObject := GetObjectProp(Self, vPropInfo);
                     vChildClass := GetObjectPropClass(Self, vPropInfo);
                     if (Copy(vChildObject.ClassName, 1, 13) = 'TExtendedList') then
                     begin
                       vExtendedList := (TRttiContext.Create.GetType(vChildClass) as TRttiInstanceType);
                       ACallBack(vExtendedList, vChildObject);
                     end;
                   end;
        end;
      end;
    finally
      Dispose(vPropList);
    end;
  except
    on E:Exception do
    begin
      raise;
    end;
  end;
end;

function TAbstractSQLObject.Save(AIsChild: Boolean; ASaveChilds: Boolean): Boolean;
var
  vQuery: TUniQuery;
  vColumns: String;
  vInsertValues: String;
  vUpdateValues: String;
  vWhere: String;

  vProcessColumnNames: TStringList;
  vColumn: TAbstractSQLObjectValue;
  I: Integer;
begin
  if not FConnection.InTransaction then
  begin
    FConnection.StartTransaction;
  end;
  try
    vProcessColumnNames := TStringList.Create;
    vQuery := TUniQuery.Create(nil);
    try
      vQuery.Connection := FConnection;

      //Propaga os respectivos valores das PKs recursivamente nas tabelas filhas associadas
      FillPrimaryKeys;
      //Preenche primeiramente todas as colunas que dependem de Sequence
      FillColumnsWithSequence;

      if FModificado then
      begin
        vUpdateValues := '';
        for I := 0 to FValues.Count-1 do
        begin
          vColumn := TAbstractSQLObjectValue(FValues.Objects[I]);
          if vColumn.IsChanged then
          begin
            vProcessColumnNames.AddObject(FValues[I], vColumn);
            if not FNovoRegistro then
            begin
              if not vColumn.PrimaryKey then
              begin
                vUpdateValues := vUpdateValues + Format(', %s = :%s',[FValues[I], FValues[I]]);
              end else
              begin
                vWhere := vWhere + Format(' and %s = :%s',[FValues[I], FValues[I]]);
              end;
            end;
          end;
        end;

        if FMarkedForDelete then
        begin
          Delete(vWhere, 1, 4);
          vQuery.SQL.Text := Format('delete from %s where %s ',
                                    [GetTableName, vWhere]);
        end else
        if FNovoRegistro then
        begin
          vColumns := vProcessColumnNames.CommaText;
          vInsertValues := StringReplace(':' + vColumns, ',',',:',[rfReplaceAll]);

          vQuery.SQL.Text := Format('insert into %s (%s) values (%s)',
                                    [GetTableName, vColumns, vInsertValues]);
        end else
        begin
          Delete(vUpdateValues, 1, 1);
          Delete(vWhere, 1, 4);
          vQuery.SQL.Text := Format('update %s set %s where %s ',
                                    [GetTableName, vUpdateValues, vWhere]);
        end;

        for I := 0 to vProcessColumnNames.Count-1 do
        begin
          vColumn := TAbstractSQLObjectValue(vProcessColumnNames.Objects[I]);

          vQuery.ParamByName(vProcessColumnNames[I]).Value := vColumn.Value;
        end;

        vQuery.Execute;
      end;

      if ASaveChilds then
      begin
        SaveChilds;
      end;

      FModificado := False;
      FNovoRegistro := False; //Muda o comportamento para um registro já existente
      Result := True;

      if FConnection.InTransaction and not AIsChild then
      begin
        FConnection.Commit;
      end;
    finally
      FreeAndNil(vProcessColumnNames);
      FreeAndNil(vQuery);
    end;
  except
    on E:Exception do
    begin
      if FConnection.InTransaction then
      begin
        FConnection.Rollback;
      end;

      E.Message := Format('%s -> %s', [GetTableName, E.Message]);

      raise;
    end;
  end;
end;

procedure TAbstractSQLObject.SaveChilds;
begin
  ForEachExtendedList(procedure(const AExtendedList: TRttiInstanceType; AChildObject: TObject)
                      var
                        I: Integer;
                        vArray: TValue;
                        vAbstractObject: TValue;
                      begin
                        vArray := AExtendedList.GetMethod('ToArray').Invoke(AChildObject, []);
                        for I := 0 to vArray.GetArrayLength-1 do
                        begin
                          vAbstractObject := vArray.GetArrayElement(I);
                          TRttiContext.Create.GetType(vAbstractObject.AsObject.ClassType).GetMethod('Save').Invoke(vAbstractObject, [True, True]);
                        end;
                      end);
end;

function TAbstractSQLObject.GetColumn(
  const AColumnName: String): TAbstractSQLObjectValue;
var
  vIndex: Integer;
begin
  vIndex := GetColumnIndex(AColumnName);
  Result := TAbstractSQLObjectValue(FValues.Objects[vIndex]);
end;

function TAbstractSQLObject.GetColumnIndex(AColumnName: String): Integer;
begin
  Result := FValues.IndexOf(AColumnName);

  if (Result = -1) then
  begin
    raise Exception.CreateFmt('Coluna não encontrada: %s', [AColumnName]);
  end;
end;

function TAbstractSQLObject.GetSequence(ASequenceName: String): Int64;
var
  vQuery: TUniQuery;
begin
  vQuery := TUniQuery.Create(nil);
  try
    vQuery.Connection := Self.Connection;
    if (FConnection.ProviderName = 'PostgreSQL') then
    begin
      vQuery.SQL.Text := Format('Select NextVal(%s) as vsequencia ',[QuotedStr(ASequenceName)]);
    end else
    begin
      vQuery.SQL.Text := Format('Select %s.NextVal as vsequencia from Dual',[ASequenceName]);;
    end;
    vQuery.Open;

    Result := vQuery.Fields[0].AsLargeInt;
  finally
    FreeAndNil(vQuery);
  end;
end;

procedure TAbstractSQLObject.SetSequence(AColumnName, ASequenceName: String);
var
  vIndex: Integer;
  vValue: TAbstractSQLObjectValue;
begin
  vIndex := GetColumnIndex(AColumnName);
  vValue := TAbstractSQLObjectValue(FValues.Objects[vIndex]);
  vValue.FSequenceName := ASequenceName;
end;

procedure TAbstractSQLObject.SetValue(const AColumnName: String; AValue: Variant);
var
  vIndex: Integer;
  vValue: TAbstractSQLObjectValue;
begin
  //Não permite mudar um valor de um campo para vazio
  if not (VarIsNull(AValue)) and
     not (VarIsEmpty(AValue)) and
    (Trim(VarToStr(AValue)) <> '') then
  begin
    vIndex := GetColumnIndex(AColumnName);
    vValue := TAbstractSQLObjectValue(FValues.Objects[vIndex]);

    if not VarSameValue(AValue, vValue.Value) then
    begin
      if (vValue.PrimaryKey) and not FNovoRegistro then
      begin
        raise Exception.CreateFmt('Não é possível modificar um campo que faz parte da PK' + #13#10 +
                                  'Tabela: %s, Campo: %s',[GetTableName,AColumnName]);
      end;

      case vValue.ColumnType of
        ftInteger: vValue.Value := Integer(AValue);
        ftDate:
          begin
            if (TVarData(AValue).VType in [varDate, varDouble]) then
            begin
              vValue.Value := AValue;
            end else
            begin
              vValue.Value := DateStringToDate(AValue);
            end;
          end;
        ftFloat:   vValue.Value := Double(AValue);
        ftBoolean: vValue.Value := Boolean(AValue);
      else
        vValue.Value := AValue;
      end;

      FModificado := True;
    end;
  end;
end;

function TAbstractSQLObject.GetValue(const AColumnName: String): Variant;
var
  vIndex: Integer;
  vValue: TAbstractSQLObjectValue;
begin
  vIndex := GetColumnIndex(AColumnName);
  vValue := TAbstractSQLObjectValue(FValues.Objects[vIndex]);

  if (VarIsClear(vValue.Value) or VarIsEmpty(vValue.Value) or VarIsNull(vValue.Value)) then
  begin
    case vValue.ColumnType of
      ftString: Result := '';
      ftInteger, ftFloat: Result := 0;
//      ftDate:    Result := TDate(vValue.Value);
//      ftBoolean: Result := Boolean(vValue.Value);
    else
      Result := vValue.Value;
    end;
//    if (vValue.ColumnType in [ftInteger, ftFloat]) then
//    begin
//      Result := 0;
//    end else
//    begin
//      Result := vValue.Value;
//    end;
  end else
  begin
    case vValue.ColumnType of
      ftInteger: Result := Integer(vValue.Value);
      ftDate:    Result := TDate(vValue.Value);
      ftFloat:   Result := Double(vValue.Value);
      ftBoolean: Result := Boolean(vValue.Value);
    else
      Result := vValue.Value;
    end;
  end;
end;

function TAbstractSQLObject.GetVariantValueToSql(AVariant: Variant): String;
begin
  if VarIsStr(AVariant) then
  begin
    Result := QuotedStr(String(AVariant));
  end else
  if VarType(AVariant) in [varByte, varSmallint, varShortInt, varWord, varLongWord, varInt64, varInteger] then
  begin
    Result := String(AVariant);
  end else
  if VarType(AVariant) in [varDate] then
  begin
    Result := QuotedStr(FormatDateTime('YYYY-MM-DD', AVariant));
  end else
  if VarType(AVariant) in [varDouble, varSingle, varCurrency] then
  begin
    Result := FormatFloat('0.', AVariant);
  end;
end;

function TAbstractSQLObject.IsNullOrEmpty(const AColumnName: String): Boolean;
var
  vIndex: Integer;
begin
  vIndex := GetColumnIndex(AColumnName);
  Result := TAbstractSQLObjectValue(FValues.Objects[vIndex]).IsEmpty;
end;

function TAbstractSQLObject.Load(AColumnNames: Array of String; AValues: Array of Variant;
  ALoadChilds: Boolean): Boolean;
var
  vQuery: TUniQuery;
  vColumns: String;
  vColumn: TAbstractSQLObjectValue;
  I: Integer;
  vWhereSQL: String;
begin
  Result := False;

  vWhereSQL := '';
  for I := 0 to Length(AColumnNames)-1 do
  begin
    if (VarType(AValues[I]) in [varEmpty, varNull]) or (VarIsStr(AValues[I]) and (Trim(AValues[I]) = '')) then
    begin
      Continue;
    end;

    vWhereSQL := vWhereSQL + Format(' and %s = %s ', [AColumnNames[I], GetVariantValueToSql(AValues[I])]);
  end;
  Delete(vWhereSQL, 1, 4);

  vQuery := TUniQuery.Create(nil);
  try
    vColumns := FValues.CommaText;

    vQuery.Connection := FConnection;
    vQuery.SQL.Text := Format('select %s from %s where %s ', [vColumns, GetTableName, vWhereSQL]);
    vQuery.Open;

    if not vQuery.IsEmpty then
    begin
      for I := 0 to FValues.Count-1 do
      begin
        vColumn := TAbstractSQLObjectValue(FValues.Objects[I]);
        vColumn.Value := vQuery.FieldByName(FValues[I]).Value;
      end;

      if ALoadChilds then
      begin
        LoadChilds;
      end;

      Result := True;
    end;

    FNovoRegistro := False;
    FModificado := False;
  finally
    FreeAndNil(vQuery);
  end;
end;


procedure TAbstractSQLObject.LoadChilds;
begin
  ForEachExtendedList(procedure(const AExtendedList: TRttiInstanceType; AChildObject: TObject)
                      begin
                        AExtendedList.GetMethod('Load').Invoke(AChildObject,[]);
                      end);
end;

procedure TAbstractSQLObject.PropagatePrimaryKeyValue(
  const AColumnName: String; AValue: Variant);
begin
  ForEachExtendedList(procedure(const AExtendedList: TRttiInstanceType; AChildObject: TObject)
                      var
                        I: Integer;
                        vColumn: String;
                        vArray: TValue;
                        vAbstractObject: TValue;
                        vChildValue: Variant;
                      begin
                        vColumn := AExtendedList.GetMethod('GetLinkedJoinColumnTo').Invoke(AChildObject, [AColumnName]).AsString;

                        if (vColumn <> '') then
                        begin
                          vArray := AExtendedList.GetMethod('ToArray').Invoke(AChildObject, []);
                          for I := 0 to vArray.GetArrayLength-1 do
                          begin
                            vAbstractObject := vArray.GetArrayElement(I);
                            vChildValue := TRttiContext.Create.GetType(vAbstractObject.AsObject.ClassType).GetMethod('GetValue').Invoke(vAbstractObject, [vColumn]).AsVariant;
                            TRttiContext.Create.GetType(vAbstractObject.AsObject.ClassType).GetMethod('SetValue').Invoke(vAbstractObject, [vColumn, TValue.FromVariant(AValue)]);
                          end;
                        end;
                      end);
end;

function TAbstractSQLObject.AddChild<T>(
  AJoinColumnsFrom: Array of String;
  AJoinColumnsTo: Array of String;
  ASortColumns: Array of String): TExtendedList<T>;
begin
  Result := TExtendedList<T>.Create(Self, AJoinColumnsFrom, AJoinColumnsTo, ASortColumns);
end;

{ TAbstractSQLObjectValue }

function TAbstractSQLObjectValue.GetIsEmpty: Boolean;
begin
  Result := (VarIsEmpty(FValue)) or (Trim(VarToStr(FValue)) = '');
end;

procedure TAbstractSQLObjectValue.SetValue(AValue: Variant);
begin
  FValue := AValue;
  FChanged := True;
end;

procedure TAbstractSQLObjectValue.Sum(AValue: Double);
begin
  FValue := Double(FValue) + AValue;
  FChanged := True;
end;

{ TExtendedList<T> }

constructor TExtendedList<T>.Create(ATableOwner: TAbstractSQLObject;
  AJoinColumnsFrom, AJoinColumnsTo, ASortColumns: array of String);
var
  _SelfClassType: T;
  I: Integer;
  vJoin: String;
  vWhere: String;
  vSort: String;
  V : TValue;
  vTableTo: String;

  vQualifiedClassName: String;
  vRTTIContext: TRttiContext;
  vRTTIChildType: TRttiInstanceType;
begin
  if (Length(AJoinColumnsFrom) <> Length(AJoinColumnsTo)) then
  begin
    raise Exception.Create('O número de colunas no Join deve ser a mesma');
  end;

  vQualifiedClassName := Copy(Self.ClassName, 15, Pos('>',Self.ClassName)-15);
  vRTTIChildType := (vRTTIContext.FindType(vQualifiedClassName) as TRttiInstanceType);

  FOwner := ATableOwner;
  FJoinColumnsFrom := TStringList.Create;
  FJoinColumnsTo := TStringList.Create;
  FSortColumns := TStringList.Create;

  V := vRTTIChildType.GetMethod('Create').Invoke(vRTTIChildType.MetaclassType, [FOwner.Connection]);
  FChildTable := TAbstractSQLObject(V.AsObject).GetTableName;

  try
    vJoin  := '';
    vWhere := '';
    vSort  := '';
    for I := 0 to Length(AJoinColumnsTo)-1 do
    begin
      FJoinColumnsTo.Add(AJoinColumnsTo[I]);
      FJoinColumnsFrom.Add(AJoinColumnsFrom[I]);
      vJoin := vJoin + Format(' and a.%s = b.%s ', [AJoinColumnsTo[I], AJoinColumnsFrom[I]]);
      vWhere := vWhere + Format(' and b.%s = :%s ', [AJoinColumnsFrom[I], AJoinColumnsFrom[I]]);
    end;
    System.Delete(vJoin, 1, 4);
    System.Delete(vWhere, 1, 4);

    if (FSortColumns.Count > 0) then
    begin
      vSort := 'order by a.' + StringReplace(FSortColumns.CommaText,',',',a.',[rfReplaceAll]);
    end;

    FSQL := Format('select distinct a.* '+
                   'from %s a '+
                   'inner join %s b '+
                   'on %s '+
                   'where %s %s ',
                   [FChildTable,
                    FOwner.GetTableName,
                    vJoin, vWhere, vSort]);

    inherited Create(True);
  finally
    TAbstractSQLObject(V.AsObject).Free;
  end;
end;

procedure TExtendedList<T>.Delete(Item: T);
begin
  (Item as TAbstractSQLObject).FModificado := True;
  (Item as TAbstractSQLObject).MarkedForDelete := True;
  (Item as TAbstractSQLObject).NovoRegistro := False;
end;

destructor TExtendedList<T>.Destroy;
begin
  FreeAndNil(FJoinColumnsFrom);
  FreeAndNil(FJoinColumnsTo);
  FreeAndNil(FSortColumns);

  inherited;
end;

function TExtendedList<T>.FindBy(AColumns: array of String;
  AValues: array of Variant): T;
var
  Item: T;
  I, J: Integer;
  vFound: Boolean;
begin
  Result := nil;
  for I:= 0 to Count-1 do
  begin
    Item := Items[I];

    for J := Low(AColumns) to High(AColumns) do
    begin
      if not (TAbstractSQLObject(Item)[AColumns[J]] = AValues[J]) then
      begin
        vFound := False;
        Break;
      end;
      vFound := True;
    end;

    if vFound then
    begin
      Result := Item;
      Break;
    end;
  end;
end;

function TExtendedList<T>.GetLinkedJoinColumnTo(AColumnNameFrom: String): String;
var
  vIndex: Integer;
begin
  Result := '';

  vIndex := FJoinColumnsFrom.IndexOf(AColumnNameFrom);
  if (vIndex >= 0) then
  begin
    Result := FJoinColumnsTo[vIndex];
  end;
end;

function TExtendedList<T>.GetMaxInt(AColumnName: String): Int64;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Self.Count-1 do
  begin
    if Int64(TAbstractSQLObject(Items[I])[AColumnName]) > Result then
    begin
      Result := Int64(TAbstractSQLObject(Items[I])[AColumnName]);
    end;
  end;
end;

procedure TExtendedList<T>.Load;
var
  I: Integer;
  vQuery: TUniQuery;
  vColumnName: String;
  vColumnValue: TAbstractSQLObjectValue;
  V: TValue;
  vClass: TAbstractSQLObject;

  vQualifiedClassName: String;
  vRTTIContext: TRttiContext;
  vRTTIChildType: TRttiInstanceType;
begin
  Clear;

  vQuery := TUniQuery.Create(nil);
  try
    vQuery.Connection := FOwner.Connection;
    vQuery.SQL.Text := FSQL;
    for I := 0 to vQuery.ParamCount-1 do
    begin
      vColumnName := vQuery.Params[I].Name;
      vQuery.Params[I].Value := FOwner[vColumnName];
    end;
    vQuery.Open;

    if not vQuery.IsEmpty then
    begin
      vQualifiedClassName := Copy(Self.ClassName, 15, Pos('>',Self.ClassName)-15);
      vRTTIChildType := (vRTTIContext.FindType(vQualifiedClassName) as TRttiInstanceType);

      while not vQuery.Eof do
      begin
        V := vRTTIChildType.GetMethod('Create').Invoke(vRTTIChildType.MetaclassType, [FOwner.Connection]);
        vClass := TAbstractSQLObject(V.AsObject);
        vClass.FNovoRegistro := False;

        for I := 0 to vClass.FValues.Count-1 do
        begin
          vColumnValue := TAbstractSQLObjectValue(vClass.FValues.Objects[I]);
          vColumnValue.Value := vQuery.FieldByName(vClass.FValues[I]).Value;
        end;
        vClass.LoadChilds;
        Add(vClass);

        vQuery.Next;
      end;
    end;
  finally
    FreeAndNil(vQuery);
  end;
end;

function TExtendedList<T>.Novo: T;
var
  vQualifiedClassName: String;
  vRTTIContext: TRttiContext;
  vRTTIChildType: TRttiInstanceType;
  V: TValue;
begin
  vQualifiedClassName := Copy(Self.ClassName, 15, Pos('>',Self.ClassName)-15);
  vRTTIChildType := (vRTTIContext.FindType(vQualifiedClassName) as TRttiInstanceType);

  V := vRTTIChildType.GetMethod('Create').Invoke(vRTTIChildType.MetaclassType, [FOwner.Connection]);
  TAbstractSQLObject(V.AsObject).FModificado := True;
  Result := T(V.AsObject);
  Add(Result);
end;

end.
