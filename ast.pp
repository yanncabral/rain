unit ast;

{$mode objfpc}
{$modeSwitch advancedRecords}

interface

uses token, objlist, garbagecollector;

type

  { TNAST }

  TAST = class(TTraceableObject)
  private
    tag: longint;
  public
    Token, Op: TToken;
    Value: string;
    Left, Right, Expression: TAST;
  end;

  TSymbol = class
  private
    FName: string;
    FKind: string;
    function getID(): string;
  public
    constructor Create(Name: string; const Kind: string = '');
    property ID: string read getID;
  end;

  TBuiltinTypeSymbol = class(TSymbol)
  end;

  { TBlock }

  TBlock = class(TAST)
  public
    Declarations: TObjlist;
    Compound_Statement: TAST;
    constructor Create(newDeclarations: TObjlist; newCompound_Statement: TAST);
  end;

  { TVarDecl }


  TVarDecl = class(TAST)
  public
    VarNode, TypeNode: TAST;
    constructor Create(newVarNode, newTypeNode: TAST);
  end;

  { TProgram }

  TProgram = class(TAST)
  public
    ProgramName: string;
    Block: TAST;
    constructor Create(newName: string; newBlock: TAST);
  end;

  { TCompound }


  TCompound = class(TAST)
  public
    Children: TObjlist;
    constructor Create;

  end;

  { TNoOperator }

  TNoOperator = class(TAST)
    constructor Create;
  end;

  TType = class(TAST)
  public
    constructor Create(newToken: TToken);
  end;

  { TVariable }

  TVariable = class(TAST)
  public
    constructor Create(TokenVar: TToken);
  end;

  { TAssignOperator }

  TAssignOperator = class(TAST)
  public
    constructor Create(LeftToken, RightToken: TAST; OperatorToken: TToken);
  end;

  { TUnaryOperator }

  TUnaryOperator = class(TAST)
  public
    constructor Create(OperatorToken: TToken; Expr: TAST);
  end;

  { TBinaryOperator }

  TBinaryOperator = class(TAST)
  public
    constructor Create(LeftToken, RightToken: TAST; OperatorToken: TToken);
  end;

  { TNumberOperator }

  TNumberOperator = class(TAST)
  public
    constructor Create(NumberToken: TToken);
  end;
  
implementation


constructor TSymbol.Create(Name: string; const Kind: string = '');
begin
   self.FName := Name;
   self.FKind := Kind;
end;

function TSymbol.getID(): string;
begin
   exit(self.FName);
end;

{ TNoOperator }

constructor TNoOperator.Create;
begin
  inherited Create;
end;

{TBinOp}

constructor TBinaryOperator.Create(LeftToken, RightToken: TAST; OperatorToken: TToken);
begin
  //  self.new;
  inherited Create;
  self.Left := LeftToken;
  self.Token := OperatorToken;
  Self.Op := OperatorToken;
  self.Right := RightToken;
end;

{TNumberToken}

constructor TNumberOperator.Create(NumberToken: TToken);
begin
  //  self.new;
  inherited Create;
  self.Token := NumberToken;
  self.Value := NumberToken.Value;
end;

{TUnaryOperator}

constructor TUnaryOperator.Create(OperatorToken: TToken; Expr: TAST);
begin
  //  self.new;
  inherited Create;
  Self.Op := OperatorToken;
  Self.Token := OperatorToken;
  Self.Expression := Expr;
end;

constructor TAssignOperator.Create(LeftToken, RightToken: TAST; OperatorToken: TToken);
begin
  //  self.new;
  inherited Create;
  self.Left := LeftToken;
  self.Token := OperatorToken;
  Self.Op := OperatorToken;
  self.Right := RightToken;
end;

constructor TVariable.Create(TokenVar: TToken);
begin
  //  self.new;
  inherited Create;
  self.Token := TokenVar;
  self.Value := TokenVar.Value;
end;

constructor TCompound.Create;
begin
  //  self.new;
  inherited Create;
  self.Children := TObjlist.Create;
end;

constructor TProgram.Create(newName: string; newBlock: TAST);
begin
  //  self.new;
  inherited Create;
  self.ProgramName := newName;
  self.Block := newBlock;
end;

constructor TBlock.Create(newDeclarations: TObjlist; newCompound_Statement: TAST);
begin
  //  self.new;
  inherited Create;
  self.Declarations := newDeclarations;
  self.Compound_Statement := newCompound_Statement;
end;

constructor TVarDecl.Create(newVarNode, newTypeNode: TAST);
begin
  //  self.new;
  inherited Create;
  self.VarNode := newVarNode;
  self.TypeNode := newTypeNode;
end;

constructor TType.Create(newToken: TToken);
begin
  //  self.new;
  inherited Create;
  self.Token := newToken;
end;

end.