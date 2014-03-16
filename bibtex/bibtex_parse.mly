%{
  open Bibtex
%}

%token END COMMA EQ LBRACE RBRACE
%token <string> FIELD VALUE
%token <string> AT IDENTIFIER

%start main
%type <Bibtex.bibtex> main

%%

main:
  AT IDENTIFIER COMMA fields END {
    {
      pubtype = $1;
      key = $2;
      fields = $4;
    }
  }
  ;
fields:
  | { [] }
  | field { [ $1 ] }
  | field COMMA fields { $1 :: $3 }
field:
  IDENTIFIER EQ VALUE { ($1, $3) }
  | IDENTIFIER EQ IDENTIFIER { ($1, $3) }
  ;
