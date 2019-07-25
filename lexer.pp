unit lexer;

{$mode objfpc}{$H+}

interface

uses token, SysUtils, StrUtils;

type
  { TLexer }

  TLexer = object
  private
    inputText: string;
    currentLine: longint;
    startLine: longint;
    currentPos: longint;
    Lexeme: char;
    inputLength: longint;
    hadError: boolean;
  public
    function Peek(peek_pos: longint = 1): char;
    function nextValidToken: char;
    function getNumberToken: TToken;
    function skipComments: TToken;
    function getOperatorToken: string;
    class function New(const Text: string): TLexer; static;
    procedure Error(const line: longint; msg: string);
    procedure Report(const line, Where: longint; msg: string; start: longint = 0);
    function getNextToken: TToken;
    function Advance(const count: longint = 1): char;
    function getIdToken: TToken;
    function getReservedWord(const token: string): TTokenKind;
    function isAlpha(const c: char): boolean;
    function isNumeric(const c: char): boolean;
    function isAlphaNumeric(const c: char): boolean;
    procedure newLine();
    function Match(const c: char): boolean;
    function getStringToken: TToken;
    function IfThen_(const check: boolean; const v1, v2: TTokenKind): TTokenKind;    
  end;

implementation

{ TLexer }
function TLexer.skipComments: TToken;
begin
  Advance(posEx(LineEnding, inputText, currentPos+2)-currentPos);
  Exit(TToken.Create(tkLineBreak));
end;

procedure TLexer.newLine;
begin
  inc(currentLine);
  startLine := currentPos+1;
end;

function TLexer.getReservedWord(const token: string): TTokenKind;
begin
  if token[1] = '_' then
    Exit(tkId);
  case LowerCase(token) of // LowerCase turns the lang in case insensitive
    'and':    Exit(tkAnd);
    'class':  Exit(tkClass);
    'else':   Exit(tkElse);
    'false':  Exit(tkFalse);
    'for':    Exit(tkFor);
    'func':   Exit(tkFunction);
    'if':     Exit(tkIf);
    'nil':    Exit(tkNil);
    'or':     Exit(tkOr);
    'print':  Exit(tkPrint);
    'return': Exit(tkReturn);
    'super':  Exit(tkSuper);
    'this':   Exit(tkThis);
    'true':   Exit(tkTrue);
    'var':    Exit(tkVar);
    'while':  Exit(tkWhile);
    'repeat': Exit(tkRepeat);
    'until':  Exit(tkUntil);
    'const':  Exit(tkConst);
  end;
  Exit(tkId);
end;

function TLexer.getIdToken: TToken;
var
  token: shortstring = '';
  len: byte = 1;
  tokenId: TTokenKind;
  initPos, initLine: longint;
begin
  token := Lexeme;
  initPos := currentPos;
  initLine := currentLine;
  while isAlphaNumeric(Peek) do
    if len < 32 then begin // Limit identifiers to 32 characters.
      token += Advance;
      inc(len)
    end else
      Report(initLine, initPos, 'The limit of 32 characters for identifier was reached.');
  exit(TToken.Create(getReservedWord(token)));
end;

function TLexer.getNumberToken: TToken;
var
  token: string;
begin
  token := Lexeme;
  while Peek in ['0'..'9','.'] do
    token += Advance;  
  if Lexeme = '.' then
    Report(currentLine, currentPos, 'Numbers can''''t terminate with "."');
  Exit(TToken.Create(tkNumber, token));
end;

function TLexer.getOperatorToken: string;
begin
  if Peek = '=' then
    exit(':'+Advance);
  exit(':');
end;

class function TLexer.New(const Text: string): TLexer; static;
begin
  result.inputText := Text;
  result.inputLength := Length(result.inputText);
  result.currentPos := 0; //Pascal string begins at [1] offset
  result.currentLine := 1;
  result.startLine := 1;
end;

function TLexer.nextValidToken: char;
begin
  repeat 
    result := Advance();
  until result <> ' ';
end;

function TLexer.Peek(peek_pos: longint = 1): char;
begin
  peek_pos += currentPos;
  if (peek_pos > inputLength) then
    Exit(#0);
  Exit(inputText[peek_pos]);
end;

function TLexer.Advance(const count: longint = 1): char;
var
  next: char;
begin
  next := Peek(count);
  Inc(currentPos, count);
  if next = LineEnding then begin
    newLine;
    //exit(Advance); Uncomment this line to ommit tkLineBreak token
    end;
  Lexeme := next;
  Exit(Lexeme);
end;

function TLexer.IfThen_(const check: boolean; const v1, v2: TTokenKind): TTokenKind;
begin
  if not check then
    exit(v2);
  advance;
  exit(v1);
end;

function TLexer.getNextToken: TToken;
begin
  case nextValidToken of
    'A'..'Z',
    'a'..'z', '_': Exit(getIdToken);
    '0'..'9': Exit(getNumberToken);
    '/':  if Match('/') then
            Exit(skipComments)
          else
            Exit(TToken.Create(tkSlash));
    '*': Exit(TToken.Create(tkStar,'*'));
    '+': Exit(TToken.Create(tkPlus,'+'));
    '-': Exit(TToken.Create(tkMinus,'-'));
    '^': Exit(TToken.Create(tkExp,'^'));
    '(': Exit(TToken.Create(tkLParen));
    ')': Exit(TToken.Create(tkRParen));
    '[': Exit(TToken.Create(tkLBracket));
    ']': Exit(TToken.Create(tkRBracket));    
    ';': Exit(TToken.Create(tkSemicolon));
    '.': Exit(TToken.Create(tkDot));
    '''', '"': Exit(getStringToken); // Ambos os tipos de aspas são suportados p/ strings.
    '{': Exit(TToken.Create(tkLBrace)); 
    '!': Exit(TToken.Create(ifThen_(Match('='), tkBangEqual, tkBang)));      
    '=': Exit(TToken.Create(ifThen_(Match('='), tkEqual, tkAssign)));    
    '<': Exit(TToken.Create(ifThen_(Match('='), tkLessEqual, tkLess)));      
    '>': Exit(TToken.Create(ifThen_(Match('='), tkGreaterEqual, tkGreater)));
    '}': Exit(TToken.Create(tkRBrace));   
    ':': Exit(TToken.Create(tkColon));    
    //' ': Exit();
    LineEnding: Exit(TToken.Create(tkLineBreak)); // \n também é um token aqui. A função Advance o desabilita.
    #0:  Exit(TToken.Create(tkEOF)); // Fim do arquivo
    ',': Exit(TToken.Create(tkComma));
  end;
  //Se não foi reconhecido
  Report(currentLine, currentPos,'Unexpected character "'+(Lexeme)+'".');
end;

procedure TLexer.Error(const line: longint; msg: string);
begin
  Report(line, 0, msg);
end;

procedure TLexer.Report(const line, Where: longint; msg: string; start: longint = 0);
var
  loop: longint;
begin
  writeLn('Error: ',msg);
  msg := inttostr(line) + ' | ';
  write(^I, msg);
  if start = 0 then
    start := startLine;
  WriteLn(Copy(inputText, start, PosEx(LineEnding, inputText, start) - start));
  write(^I);
  for loop := 1 to Where-start+Length(msg) do
    write(' ');
  writeLn('^-- roughly here');
  system.Halt(); // Exit after one lexical error.
end;

function TLexer.isAlpha(const c: char): boolean;
begin
  exit(c in ['A'..'Z', 'a'..'z','_','?']);
end;

function TLexer.isNumeric(const c: char): boolean;
begin
  exit(c in ['0'..'9']);
end;

function TLexer.isAlphaNumeric(const c: char): boolean;
begin
  exit(isAlpha(c) or isNumeric(c));
end;

function TLexer.Match(const c: char): boolean;
begin
  Exit(Peek = c);
end;

function TLexer.getStringToken: TToken;
var
  endPos: longint;
begin
  endPos := posEx(lexeme, inputText, currentPos+1);
  if endPos = 0 then
    Report(currentLine, currentPos, 'Unterminated string.', startLine);
  dec(endPos, currentPos);
  result := TToken.Create(tkString, Copy(inputText, currentPos+1, endPos-1));
  Advance(endPos);
end;

end.
