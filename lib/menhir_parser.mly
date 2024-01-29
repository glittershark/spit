%token EOF LPAR RPAR QUOT BACKTICK COMMA COMMA_AT
%token <string> ATOM
%token <string> STRING
%token <int> INT

%start <Ast.sexp list> sexp_eof

%%

literal:
  | s=STRING { Ast.LString s }
  | i=INT { Ast.LInt i }
  ;

sexp_eof:
  | es=list(sexp); EOF { es }
  ;

sexp:
  | a=ATOM { Ast.Atom a }
  | LPAR; es=list(sexp); RPAR { Ast.List es }
  | lit=literal { Ast.Literal lit }
  | QUOT; s=sexp { Ast.Quote s }
  | BACKTICK; s=sexp { Ast.Quasiquote s }
  | COMMA; s=sexp { Ast.Unquote s }
  | COMMA_AT; s=sexp { Ast.UnquoteSplicing s }
  ;

%%
