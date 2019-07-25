unit Smartobj;

interface

{$mode delphi}{$H+}

type
  TSmartObj<T: class, constructor> = record
  strict private
    FFreeTheValue: IInterface;
    function GetValue: T;
  private
    type
      TFreeTheValue = class(TInterfacedObject)
      private
        fObjectToFree: TObject;
      public
        constructor Create(anObjectToFree: TObject);
        destructor Destroy; override;
      end;
  public
    FValue: T default;
    constructor Create(AValue: T); overload;
    procedure Create; overload;
    {$IFDEF VER3_1_1}
    class operator Implicit(AValue: T): TSmartObj<T>;
    class operator Implicit(smart: TSmartObj <T>): T;
    {$ENDIF}
    property Value: T read GetValue;
  end;

implementation

{ TSmartPointer<T> }

constructor TSmartObj<T>.Create(AValue: T);
begin
  FValue := AValue;
  FFreeTheValue := TFreeTheValue.Create(FValue);
end;

procedure TSmartObj<T>.Create;
begin
  Create(T.Create);
end;

function TSmartObj<T>.GetValue: T;
begin
  if not Assigned(FFreeTheValue) then
   Self := TSmartObj<T>.Create(T.Create);
  Result := FValue;
end;


{$IFDEF VER3_1_1}
class operator TSmartObj<T>.Implicit(smart: TSmartObj<T>): T;
begin
  Result := Smart.Value;
end;

class operator TSmartObj<T>.Implicit(AValue: T): TSmartObj<T>;
begin
  Result := TSmartObj<T>.Create(AValue);
end;
{$ENDIF}

constructor TSmartObj<T>.TFreeTheValue.Create(anObjectToFree: TObject);
begin
  fObjectToFree := anObjectToFree;
end;

destructor TSmartObj<T>.TFreeTheValue.Destroy;
begin
  fObjectToFree.Free;
  inherited;
end;


end.

