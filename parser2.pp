unit parser2;

{$mode objfpc}{$modeswitch advancedrecords}

interface

uses token, lexer, SysUtils, ast, objlist;

type

  { TParser }

  TParser = record
  private
    Lexer: TLexer;
    currentToken: TToken;
  public
    class function New(const inputText: string): TParser; static;
    procedure EatToken(TokenType: TTokenKind);
    procedure Error;
    function Factor: TAST;
    function Term: TAST;
    function nextExpression: TAST;
    function Parse: TAST;
    function program_statement: TAST;
    function compound_statement: TCompound;
    function statement_list: TObjlist;
    function statement: TAST;
    function Assignment_statement: TAST;
    function empty: TAST;
    function variable: TAST;
    function block: TAST;
    function declarations: TObjlist;
    function var_declarations: TObjlist;
    function type_spec: TType;
  end;

implementation

{TParser}

function TParser.program_statement: TAST;
var
  program_name: string;
begin
  self.EatToken(tkProgram);
  program_name := self.variable.Value;
  self.EatToken(tkSemi);
  Result := TProgram.Create(program_name, self.block);
  self.EatToken(tkDot);
end;

function TParser.compound_statement: TCompound;
begin
  Result := TCompound.Create();
  self.EatToken(tkBegin);
  Result.Children := self.statement_list();
  self.EatToken(tkEnd);
end;

function TParser.statement_list: TObjlist;
begin
  Result := TObjlist.Create;
  Result.Add(self.statement);

  while self.currentToken.TokenType = tkSemi do
  begin
    self.EatToken(tkSemi);
    Result.add(self.statement());
  end;
end;

function TParser.statement: TAST;
begin
  case self.currentToken.TokenType of
    TkBegin: Result := self.compound_statement;
    tkId: Result := self.Assignment_statement;
    else
      Result := self.empty;
  end;
end;

function TParser.Assignment_statement: TAST;
var
  left, right: TAST;
  cToken: TToken;
begin
  left := self.variable;
  cToken := self.currentToken;
  self.EatToken(tkAssign);
  right := self.nextExpression;
  Result := TAssignOperator.Create(left, right, cToken);
end;

function TParser.variable: TAST;
begin
  Result := TVariable.Create(self.currentToken);
  self.EatToken(TkID);
end;

function TParser.empty: TAST;
begin
  Result := TNoOperator.Create();
end;

class function TParser.New(const inputText: string): TParser;
begin
  Result.Lexer := TLexer.New(inputText);
  Result.currentToken := Result.Lexer.getNextToken();
end;

procedure TParser.Error;
begin
  raise Exception.Create('Invalid syntax on Parser');
end;

procedure TParser.EatToken(TokenType: TTokenKind);
begin
  if Self.currentToken.TokenType = TokenType then
    self.currentToken := self.lexer.getNextToken
  else
    self.Error();
end;

function TParser.nextExpression: TAST;
var
  cToken: TToken;
begin
  Result := self.term();
  while (self.currentToken.TokenType in TLvl1TokenTypes) do
  begin
    cToken := self.currentToken;
    case cToken.TokenType of
      tkPlus: EatToken(cToken.TokenType);
      tkMinus: EatToken(cToken.TokenType);
    end;
    Result := TBinaryOperator.Create(Result, self.term, cToken);
  end;
end;

function TParser.Term: TAST;
var
  cToken: TToken;
begin
  Result := self.factor();
  while (self.currentToken.TokenType in TLvl2TokenTypes) do
  begin
    cToken := self.currentToken;
    case cToken.TokenType of
      tkMul: EatToken(cToken.TokenType);
      tkSlash: EatToken(cToken.TokenType);
      tkExp: EatToken(cToken.TokenType);
      tkDiv: EatToken(cToken.TokenType);
    end;
    Result := TBinaryOperator.Create(Result, self.factor(), cToken);
  end;
end;

function TParser.Factor: TAST;
var
  cToken: TToken;
begin
  cToken := self.currentToken;

  case cToken.TokenType of
    tkPlus:
    begin
      Self.EatToken(tkPlus);
      Result := TUnaryOperator.Create(cToken, self.Factor());
    end;
    tkMinus:
    begin
      Self.EatToken(tkMinus);
      Result := TUnaryOperator.Create(cToken, self.Factor());
    end;
    tkConstantInteger:
    begin
      self.EatToken(tkConstantInteger);
      Result := TNumberOperator.Create(cToken);
    end;
    tkConstantReal:
    begin
      self.EatToken(tkConstantReal);
      Result := TNumberOperator.Create(cToken);
    end;
    tkLParen:
    begin
      self.EatToken(tkLParen);
      Result := self.nextExpression;
      self.EatToken(tkRParen);
    end;
    else
      Result := self.variable;
  end;
end;

function TParser.Parse: TAST;
begin
  Result := self.program_statement;
  if not (Self.currentToken.TokenType = tkEOF) then
    self.Error;
end;

function TParser.block: TAST;
var
  decl: TObjlist;
begin
  decl := self.declarations;
  Result := TBlock.Create(decl, self.compound_statement);
end;

function TParser.declarations: TObjlist;
begin
  if self.currentToken.TokenType = tkVar then
  begin
    self.EatToken(tkVar);
    while self.currentToken.TokenType = tkId do
    begin
      Result := var_declarations;
      self.EatToken(tkSemi);
    end;
  end;
end;

function TParser.var_declarations: TObjlist;
var
  var_nodes: TObjlist;
  type_node: TType;
  i: longint;
begin
  Result := TObjlist.Create();
  var_nodes := TObjlist.Create();
  var_nodes.add(TVariable.Create(self.currentToken));
  self.EatToken(tkId);
  while self.currentToken.TokenType = tkComma do
  begin
    self.EatToken(tkComma);
    var_nodes.add(TVariable.Create(self.currentToken));
    self.EatToken(tkId);
  end;
  self.EatToken(tkColon);
  type_node := self.type_spec;
  for i := 0 to var_nodes.Count - 1 do
    Result.add(TVarDecl.Create(var_nodes[i] as TAST, type_node));
end;

function TParser.type_spec: TType;
var
  cToken: TToken;
begin
  cToken := self.currentToken;
  case self.currentToken.TokenType of
    tkTypeInteger: self.EatToken(tkTypeInteger);
    tkTypeReal: self.EatToken(tkTypeReal);
  end;
  Result := TType.Create(cToken);
end;

end.
