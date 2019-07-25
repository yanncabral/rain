unit AstPrinter;
{$mode objfpc}  

interface

uses expr;

type
    TAstPrinter = class(TInterfacedObject, IVisitor)
        function parenthesize(name: string; exprs: TExprArray): string;
        function visit(expr: TBinary): string; overload;
        function visit(expr: TGrouping): string; overload;
        function visit(expr: TLiteral): string; overload;
        function visit(expr: TUnary): string; overload;
        function Print(Expression: TExpr): string;
    end;

implementation

function TAstPrinter.parenthesize(name: string; exprs: TExprArray): string;
var
    x: TExpr;
begin
    result := '(' + name;
    for x in exprs do
      result += ' '+x.accept(self);
    result += ')';
end;
  

function TAstPrinter.visit(expr: TBinary): string;
begin
    exit(self.parenthesize(expr.op.value, [expr.left, expr.right]));
end;

(* function TAstPrinter.visit(expr: TExpr): string;
begin
    exit('nada');
end; *)

function TAstPrinter.visit(expr: TGrouping): string;
begin
    exit(parenthesize('group', [expr.expression]));   
end;

function TAstPrinter.visit(expr: TLiteral): string;
begin
(*     if expr.value = nil then
        exit('nil'); *)
    exit(expr.value);
end;

function TAstPrinter.visit(expr: TUnary): string;
begin
    exit(parenthesize(expr.op.value, [expr.right]));
end;


function TAstPrinter.Print(Expression: TExpr): string;
begin
  writeLn('Parser:');
  exit(Expression.Accept(self));
end;

end.
