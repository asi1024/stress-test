%{
open Syntax
%}

%token BPRE EPRE
%token SPACE EOLN EOF
%token COMMA UNDER LBRACE RBRACE DOTS
%token LPAREN RPAREN PLUS MINUS DIV (* LT GT *)

%token <int> INTV
%token <string> ID

%start toplevel
%type <Syntax.format> toplevel
%%

toplevel : BPRE x=ListAll EOF { List (x, '\n') }

ListAll :
    EPRE { [] }
  | x=LineAll EOLN y=ListAll { x :: y }
  | x=LineAll EOLN DOTS EOLN y=LineAll EOLN tail=ListAll { loop x y "i" '\n' :: tail }

LineAll : x=LineList { List (x, ' ') }

LineList :
    x=Variable { [ x ] }
  | head=Variable SPACE tail=LineList { head :: tail }
  | v=Variable SpaceOpt DOTS SpaceOpt w=Variable { [ loop v w "j" ' ' ] }

Variable :
    v=ID { Var (v, []) }
  | v=ID UNDER e=Expr { Var (v, [e]) }
  | v=ID UNDER LBRACE i=Index RBRACE { Var (v, i) }

Index :
    e=Expr { [e] }
  | e=Expr COMMA SpaceOpt tail=Index { e :: tail }

Expr :
    e=ExprPlus { e }
  | s=ID UNDER e=Expr { Name (s, [e]) }

ExprPlus :
    e=ExprDiv { e }
  | e1=ExprPlus PLUS e2=ExprDiv { BinOp (Plus, e1, e2) }
  | e1=ExprPlus MINUS e2=ExprDiv { BinOp (Minus, e1, e2) }

ExprDiv :
    e=ExprMult { e }
  | e1=ExprDiv DIV e2=ExprMult { BinOp (Div, e1, e2) }

ExprMult :
    e=ExprUnit { e }
  | e1=ExprMult e2=ExprUnit { BinOp (Mult, e1, e2) }

ExprUnit :
    i=INTV { Const i }
  | s=ID   { Name (s, []) }
  | s=ID UNDER LBRACE i=Index RBRACE { Name (s, i) }
  | LPAREN e=Expr RPAREN { e }

SpaceOpt :
          { () }
  | SPACE { () }
