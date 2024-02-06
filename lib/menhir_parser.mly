%token EOF LPAR RPAR QUOT BACKTICK COMMA COMMA_AT DOT
%token <string> ATOM
%token <string> STRING
%token <int> INT

%start <Ast.Sexp.t list> sexp_eof

%%

literal:
  | s=STRING { Ast.Literal.String s }
  | i=INT { Ast.Literal.Int i }
  ;

cons:
  LPAR;
  e1=sexp;
  DOT
  e2=sexp;
  RPAR
  { Ast.Sexp.Cons (e1, e2) }

sexp_eof:
  | es=list(sexp); EOF { es }
  ;

sexp:
  | a=ATOM { Ast.Sexp.Atom a }
  | c = cons { c }
  | LPAR; es=list(sexp); RPAR { Ast.Sexp.List es }
  | lit=literal { Ast.Sexp.Literal lit }
  | QUOT; s=sexp { Ast.Sexp.Quote s }
  | BACKTICK; s=sexp { Ast.Sexp.Quasiquote s }
  | COMMA; s=sexp { Ast.Sexp.Unquote s }
  | COMMA_AT; s=sexp { Ast.Sexp.UnquoteSplicing s }
  ;

%%
