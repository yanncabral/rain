unit expr;
{$mode objfpc}

interface

uses token, garbagecollector;

type
    TVariant = Variant;
    IVisitor = interface;
    TExpr = class(TTraceableObject)
        function Accept(Visitor: IVisitor): string; virtual; abstract;
    end;
    TExprArray = array of TExpr;

    TBinary = class(TExpr)
        Left: TExpr;
        Op: TToken;
        Right: TExpr;
        function Accept(Visitor: IVisitor): string; override;
        constructor Create(nLeft: TExpr; nOp: TToken; nRight: TExpr);
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

implementation

constructor TBinary.Create(nLeft: TExpr; nOp: TToken; nRight: TExpr);
begin
    inherited Create;
    self.Left := nLeft;
    self.Op := nOp;
    self.Right := nRight;
end;

function TBinary.Accept(Visitor: IVisitor): string;
begin
    exit(Visitor.visit(self));
end;

constructor TGrouping.Create(nExpression: TExpr);
begin
    inherited Create;
    self.Expression := nExpression;
end;

function TGrouping.Accept(Visitor: IVisitor): string;
begin
    exit(Visitor.visit(self));
end;

constructor TLiteral.Create(nValue: TVariant);
begin
    inherited Create;
    self.Value := nValue;
end;

function TLiteral.Accept(Visitor: IVisitor): string;
begin
    exit(Visitor.visit(self));
end;

constructor TUnary.Create(nOp: TToken; nRight: TExpr);
begin
    inherited Create;
    self.Op := nOp;
    self.Right := nRight;
end;

function TUnary.Accept(Visitor: IVisitor): string;
begin
    exit(Visitor.visit(self));
end;

end.
