unit interpreter;

{$mode objfpc}{$modeswitch advancedrecords}

interface

uses token, parser, ast, fgl, SmartObj, sysutils;

type
  { TInterpreter }
  TGlobalScope = specialize TFPGMap<string, TAST>;

  TInterpreter = object
  private
    Parser: TParser;
    GlobalScope: specialize TSmartObj<TGlobalScope>;
  public
    procedure Error;
    class function New(const inputText: string): TInterpreter; static;
    function Interpret: extended;
    function visit(node: TAST): extended;
    function visit_TBinaryOperator(node: TAST): extended;
    function visit_TNumberToken(node: TAST): extended;
    function visit_TUnaryOperator(node: TAST): extended;
    procedure visit_Compound(node: TAST);
    procedure visit_Assign(node: TAST);
    function visit_Var(node: TAST): TAST;
    procedure visit_NoOp(node: TAST);
    procedure visit_program(node: TAST);
    procedure visit_Block(node: TAST);
    procedure visit_VarDecl(node: TAST);
    procedure visit_Type(node: TAST);
  end;

implementation

procedure TInterpreter.Error;
begin
  raise Exception.Create('Invalid syntax in Interpreter');
end;

class function TInterpreter.New(const inputText: string): TInterpreter;
begin
  Result.Parser := TParser.New(inputText);
  Result.GlobalScope := TGlobalScope.Create;
end;

{TInterpreter}
function TInterpreter.visit_TBinaryOperator(node: TAST): extended;
begin
  case node.Op.TokenType of
    tkPlus: Result := self.visit(node.Left) + self.visit(node.right);
    tkMinus: Result := self.visit(node.left) - self.visit(node.right);
    tkMul: Result := self.visit(node.left) * self.visit(node.right);
    tkSlash: Result := self.visit(node.left) / self.visit(node.right);
    tkDiv: Result := trunc(self.visit(node.left) / self.visit(node.right));
  end;
end;

function TInterpreter.visit_TNumberToken(node: TAST): extended;
begin
  Result := StrToFloat(node.Value);
end;

function TInterpreter.visit(node: TAST): extended;
begin
  case lowercase(node.ClassName) of
    'tbinaryoperator': Result := visit_TBinaryOperator(node);
    'tnumberoperator': Result := visit_TNumberToken(node);
    'tunaryoperator': Result := visit_TUnaryOperator(node);
    'tcompound': visit_Compound(node);
    'tassignoperator': visit_Assign(node);
    'tvariable': visit_Var(node);
    'tnooperator': visit_NoOp(node);
    'tprogram': visit_program(node);
    'tblock': visit_Block(node);
    'tvardecl': visit_VarDecl(node);
    'ttype': visit_type(node);
  end;
end;

function TInterpreter.Interpret: extended;
begin
  Result := self.visit(self.Parser.parse);
end;

function TInterpreter.visit_TUnaryOperator(node: TAST): extended;
begin
  case node.op.TokenType of
    tkPlus: Result := +visit(node.Expression);
    tkMinus: Result := -visit(node.Expression);
  end;
end;

procedure TInterpreter.visit_Compound(node: TAST);
var
  loop: longint;
begin
  with (node as TCompound).Children do
    for loop := 0 to Count - 1 do
      visit(((Items[loop] as TAST)));
end;

procedure TInterpreter.visit_NoOp(node: TAST);
begin

end;

procedure TInterpreter.visit_program(node: TAST);
begin
  self.visit((node as TProgram).Block);
end;

procedure TInterpreter.visit_Block(node: TAST);
var
  loop: longint;
begin
  for loop := 0 to (node as TBlock).Declarations.Count - 1 do
    self.visit((node as TBlock).Declarations[loop] as TAST);
  self.visit((node as TBlock).Compound_Statement);
end;

procedure TInterpreter.visit_VarDecl(node: TAST);
begin

end;

procedure TInterpreter.visit_Type(node: TAST);
begin

end;

procedure TInterpreter.visit_Assign(node: TAST);
begin
  self.GlobalScope.Value[Node.Left.Value] := node.Right;

end;

function TInterpreter.visit_Var(node: TAST): TAST;
begin
  Result := self.GlobalScope.Value[node.Value];
  if not Assigned(Result) then
    raise Exception.Create('Var error');
end;

end.
