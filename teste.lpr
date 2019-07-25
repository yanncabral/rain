{$mode objfpc}
uses Lexer, classes, token,
expr, parser, AstPrinter2, garbagecollector;

var
   myLexer: TLexer;
   myParser: TParser;
   lText: TStringList;
   myexpr: TExpr;
   tokens: TTokenArray;
   currentLen: longint;
   printer: TAstPrinter;
begin
   SetLength(tokens, 1);
   try
   lText := TStringList.Create();
   lText.LoadFromFile('teste.rain');
   myLexer := TLexer.New(lText.text);
   //writeLn('Tokens: ');
   repeat
      currentLen := Length(tokens);
      tokens[currentLen-1] := myLexer.getNextToken();
      SetLength(tokens, currentLen + 1 );      
      //writeLn(tokens[currentLen-1].toString);
   until tokens[currentLen-1].TokenType = tkEOF;
   myParser := TParser.New(tokens);
   myExpr := myParser.Parse;
   printer :=TAstPrinter.Create();
   writeLn(printer.Print(myExpr));
   finally
       lText.Free();
   end;
end.
