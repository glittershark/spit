%token EOF LPAR RPAR QUOT BACKTICK COMMA COMMA_AT DOT
%token <string> ATOM
%token <string> STRING
%token <int> INT

%start <Ast.sexp list> sexp_eof

%%

literal:
  | s=STRING { Ast.LString s }
  | i=INT { Ast.LInt i }
  ;

cons:
  LPAR;
  e1=sexp;
  DOT
  e2=sexp;
  RPAR
  { Ast.Cons (e1, e2) }

sexp_eof:
  | es=list(sexp); EOF { es }
  ;

sexp:
  | a=ATOM { Ast.Atom a }
  | c = cons { c }
  | LPAR; es=list(sexp); RPAR { Ast.List es }
  | lit=literal { Ast.Literal lit }
  | QUOT; s=sexp { Ast.Quote s }
  | BACKTICK; s=sexp { Ast.Quasiquote s }
  | COMMA; s=sexp { Ast.Unquote s }
  | COMMA_AT; s=sexp { Ast.UnquoteSplicing s }
  ;

%%
