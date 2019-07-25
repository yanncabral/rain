unit token;

{$mode objfpc}
{$H-}
{$modeSwitch advancedRecords}

interface

type TTokenKind = (                                   
  // Single-character tokens.                      
  tkLParen, tkRParen, tkLBrace, tkRBrace, tkLBracket, tkRBracket, tkColon,
  tkComma, tkDot, tkMinus, tkPlus, tkSemicolon, tkSlash, tkStar, tkExp,

  // One or two character tokens.                  
  tkBang, tkBangEqual,                                
  tkAssign, tkEqual,                              
  tkGreater, tkGreaterEqual,                          
  tkLess, tkLessEqual,                                

  // Literals.                                     
  tkId, tkString, tkNumber,                      

  // Keywords.                                     
  tkAnd, tkClass, tkElse, tkFalse, tkFunction, tkFor, tkIf, tkNil, tkOr,  
  tkPrint, tkReturn, tkSuper, tkThis, tkTrue, tkVar, tkConst, tkWhile, tkRepeat, tkUntil,   

  tkEOF, tkLineBreak                                              
);                                                  

const
  Kinds: array[TTokenKind] of ShortString = (                                   
    // Single-character tokens.                      
    'LEFT_PAREN', 'RIGHT_PAREN', 'LEFT_BRACE', 'RIGHT_BRACE', 'LEFT_BRACKET', 'RIGHT_BRACKET',
    'COLON', 'COMMA', 'DOT', 'MINUS', 'PLUS', 'SEMICOLON', 'SLASH', 'STAR', 'EXPONENT',

    // One or two character tokens.                  
    'BANG', 'BANG_EQUAL',                                
    'ASSIGN', 'EQUAL',                              
    'GREATER', 'GREATER_EQUAL',                          
    'LESS', 'LESS_EQUAL',                                

    // Literals.                                     
    'IDENTIFIER', 'STRING', 'NUMBER',                      

    // Keywords.                                     
    'AND', 'CLASS', 'ELSE', 'FALSE', 'FUNCTION', 'FOR', 'IF', 'NIL', 'OR',  
    'PRINT', 'RETURN', 'SUPER', 'THIS', 'TRUE', 'VAR', 'CONST', 'WHILE', 'REPEAT', 'UNTIL',   

    'EOF', 'LINE_BREAK'                                              
  );                                                  
    
type
  TToken = record
    Value: shortstring; // Limite de 255 caracteres.
    TokenType: TTokenKind;
    class function Create(const newTokenType: TTokenKind;
        const newValue: string = ''): TToken; static;
    function toString: string;
  end;

implementation

{ TToken }

class function TToken.Create(const newTokenType: TTokenKind;
  const newValue: string = ''): TToken;
begin
  with Result do
  begin
    Value := newValue;
    TokenType := newTokenType;
  end;
end;

function TToken.toString: string;
begin
  result := 'Token(' + Kinds[self.TokenType];
  if self.Value <> '' then
    result += ', "' + self.Value+'"';
  result += ')';
end;

end.
