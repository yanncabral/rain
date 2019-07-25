{$mode objfpc}
uses token;

type
    TVariant = Variant;
    IVisitor = interface;
    
    TExpr = class 
        function Accept(Visitor: IVisitor): string; virtual; abstract;
    end;
    TExprArray = array of TExpr;    

    TBinary = class(TExpr)
        Left: TExpr;
        Op: TToken;
        Right: TExpr;
        function Accept(Visitor: IVisitor): string; override;
        constructor Create(nLeft: TExpr; nOp: TToken; nRight: TExpr); virtual;
    end;

    TGrouping = class(TExpr)
        Expression: TExpr;
        function Accept(Visitor: IVisitor): string; override;
        constructor Create(nExpression: TExpr);
    end;

    TLiteral = class(TExpr)
        Value: TVariant;
        function Accept(Visitor: IVisitor): string; override;
        constructor Create(nValue: TVariant);
    end;

    TUnary = class(TExpr)
        Op: TToken;
        Right: TExpr;
        function Accept(Visitor: IVisitor): string; override;
        constructor Create(nOp: TToken; nRight: TExpr);
    end;

    IVisitor = interface
        function visit(expr: TBinary): string; overload; virtual; abstract;
        function visit(expr: TGrouping): string; overload; virtual; abstract;
        function visit(expr: TLiteral): string; overload; virtual; abstract;
        function visit(expr: TUnary): string; overload; virtual; abstract;
    end;

function TBinary.Accept(visitor: IVisitor): string;
begin
  exit(visitor.visit(self));
end;


function TLiteral.Accept(visitor: IVisitor): string;
begin
  exit(visitor.visit(self));
end;


function TUnary.Accept(visitor: IVisitor): string;
begin
  exit(visitor.visit(self));
end;


function TGrouping.Accept(visitor: IVisitor): string;
begin
  exit(visitor.visit(self));
end;

constructor TBinary.Create(nLeft: TExpr; nOp: TToken; nRight: TExpr);
begin
    self.Left := nLeft;
    self.Op := nOp;
    self.Right := nRight;
end;

constructor TGrouping.Create(nExpression: TExpr);
begin
    self.Expression := nExpression;
end;

constructor TLiteral.Create(nValue: TVariant);
begin
    self.Value := nValue;
end;

constructor TUnary.Create(nOp: TToken; nRight: TExpr);
begin
    self.Op := nOp;
    self.Right := nRight;
end;

var
    Expression: TBinary;
begin
    Expression := TBinary.Create(
        TUnary.Create(TToken.Create(tkMinus, '-'),
        TLiteral.Create(123)),
        TToken.Create(tkStar, '*'),
        TGrouping.Create(TBinary.Create(
            TLiteral.Create(45.67), 
            TToken.Create(tkPlus, '+'), 
            TLiteral.Create(false)
            )
        )
    );
    writeLn(AstPrinter.Create().print(Expression));
end.