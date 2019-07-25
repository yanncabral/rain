unit objlist;

{$mode delphi}{$H+}{$modeswitch advancedrecords}

interface

uses smartobj, fgl;

type

  { TObjList }

  TObjList = record
    Lista: TSmartObj<TFPGList<TObject>> default;
    class function Create: TObjList; static;
    end;

implementation

{ TObjList }

class function TObjList.Create: TObjList;
begin
  result.Lista := TFPGList<TObject>.Create;
end;

end.

