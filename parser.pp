unit parser;
{$mode objfpc}
interface

uses token, expr;

type 
  TTokenArray = array of TToken;
  TTokenTypes = array of TTokenKind;
  TParser = object
    Tokens: TTokenArray;
    currentToken: longint;
    class function New(newTokens: TTokenArray): TParser; static;
    procedure Error(token: TToken; Msg: string);
    function Match(Kinds: TTokenTypes): boolean;
    function Check(Kind: TTokenKind): boolean;
    function Advance: TToken;
    function Previous: TToken;
    function Parse: TExpr;
    function Peek: TToken;
    function isAtEnd: boolean;
    function Comparison: TExpr;
    function Addition: TExpr;
    function Multiplication: TExpr;
    function Exponentiation: TExpr;
    function Expression: TExpr;
    function Equality: TExpr;
    function Unary: TExpr;
    function Primary: TExpr;
    function Consume(Kind: TTokenKind; Msg: String): TToken;
  end;

implementation

procedure TParser.Error(token: TToken; Msg: string);
begin
  //Error here
  writeln('erro: ', Msg); // TODO: This function were to be at TInterpreter
end;

function TParser.Consume(Kind: TTokenKind; Msg: String): TToken;
begin
  if Check(Kind) then exit(Advance);
  Error(Peek, Msg);
end;

function TParser.Primary: TExpr;
begin
  case Advance.TokenType of
    tkTrue:   exit(TLiteral.Create(True));
    tkFalse:  exit(TLiteral.Create(False));
    tkNil:    exit(TLiteral.Create(Nil));
    tkNumber, 
    tkString: exit(TLiteral.Create(Previous.Value));
    tkLParen: begin
        result := TGrouping.Create(Expression);
        Consume(tkRParen, 'Expect ")" after expression.');
      end;
  else
    Error(self.Tokens[0],'Que diabos é isto?'); // Teste
    Halt();
    //exit(Variable);
  end;
end;

function TParser.Unary: TExpr;
begin
  if Match([tkBang, tkMinus]) then
    exit(TUnary.Create(Previous, Unary));
  exit(Primary);
end;

function TParser.Exponentiation: TExpr;
begin
  result := Unary;
  while Match([tkExp]) do
    result := TBinary.Create(result, Previous, Unary);
end;

function TParser.Multiplication: TExpr;
begin
  result := Exponentiation;
  while Match([tkSlash, tkStar]) do
    result := TBinary.Create(result, Previous, Exponentiation);
end;

function TParser.Addition: TExpr;
begin
  result := Multiplication;
  while Match([tkMinus, tkPlus]) do
    result := TBinary.Create(result, Previous, Multiplication);
end;

function TParser.Comparison: TExpr;
begin
   result := Addition;
    while Match([tkGreater, tkGreaterEqual, tkLess, tkLessEqual]) do
      result := TBinary.Create(result, Previous, Addition);                 
end;

function TParser.Equality: TExpr;
begin
  result := Comparison;
  while Match([tkBangEqual, tkEqual]) do
    result := TBinary.Create(result, Previous, Comparison);
end;

function TParser.Expression: TExpr;
begin
  exit(Equality);
end;

function TParser.Parse: TExpr;
begin
  exit(Expression);
end;

function TParser.Previous: TToken;
begin
  exit(Tokens[currentToken-1]);  
end;

function TParser.Peek: TToken;
begin
  exit(Tokens[currentToken]);  
end;

function TParser.isAtEnd: boolean;
begin
  Exit(Peek.TokenType = tkEOF);
end;

function TParser.Advance: TToken;
begin
  if not isAtEnd then
    inc(currentToken);
  exit(Previous);
end;

function TParser.Check(Kind: TTokenKind): boolean;
begin
  if isAtEnd then
    exit(false);
  exit(Peek.TokenType = Kind);
end;

function TParser.Match(Kinds: TTokenTypes): boolean;
var
  current: TTokenKind;
begin
  result := false;
  for current in Kinds do
    if Check(current) then begin
      Advance;
      exit(true);
    end;
end;

class function TParser.New(newTokens: TTokenArray): TParser; static;
begin
  result.currentToken := 0;
  result.Tokens := newTokens;
end;

end.
