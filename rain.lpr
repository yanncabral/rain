program rain; {$mode objfpc}{$H-}{$modeSwitch advancedRecords}

uses
  token, lexer, interpreter, parser, ast, SmartObj;

var
  falseinput: string = 'PROGRAM Part10AST; VAR a, b: INTEGER; y : REAL; BEGIN {Part10AST} a := 2; b := 10 * a + 10 * a DIV 4; y := 20 / 7 + 3.14; END. {Part10AST}';
begin
  WriteLn(TInterpreter.New(falseinput).Interpret:1:2);
  case ParamCount do begin
    0: WriteLn(TInterpreter.New(falseinput).Interpret():1:2);
    1:TInterpreter.New(falseinput).Interpret();
    else
      WriteLn('Usage: rain [script]');
      System.Halt(64);
    end;
end.