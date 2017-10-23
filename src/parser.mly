%{
open Syntax
%}

%token BVAR EVAR BPRE EPRE
%token SPACE EOLN EOF
%token COMMA UNDER LBRACE RBRACE HDOTS VDOTS
%token LPAREN RPAREN PLUS MINUS DIV (* LT GT *)

%token <int> INTV
%token <string> ID

%start toplevel
%type <Syntax.format> toplevel
%%

toplevel : BPRE x=ListAll EPRE EOF { List (x, '\n') }

ListAll :
    x=LoopAll { [ x ] }
  | x=LoopAll EOLN y=ListAll { x :: y }

LoopAll :
    x=LineAll { x }
  | x=LineAll EOLN VDOTS EOLN y=LineAll { loop x y "i" '\n' }

LineAll : x=LineList { List (x, ' ') }

LineList :
    x=LineLoop { [ x ] }
  | head=LineLoop SPACE tail=LineList { head :: tail }

LineLoop :
    v=Variable { v }
  | v=Variable SPACE HDOTS SPACE w=Variable { loop v w "j" ' ' }

Variable : BVAR v=VarInner EVAR { v }

VarInner :
    v=ID { Var (v, []) }
  | v=ID UNDER e=Expr { Var (v, [e]) }
  | v=ID UNDER LBRACE i=Index RBRACE { Var (v, i) }

Index :
    e=Expr { [e] }
  | e=Expr COMMA tail=Index { e :: tail }

Expr : e=ExprPlus { e }

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
  | s=ID UNDER e=Expr { Name (s, [e]) }
  | s=ID UNDER LBRACE i=Index RBRACE { Name (s, i) }
  | LPAREN e=Expr RPAREN { e }
