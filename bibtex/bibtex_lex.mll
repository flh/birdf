{
  open Bibtex_parse
  exception Eof
}

let identifier = ['a'-'z' 'A'-'Z' '_' ':' '+' '/' '-' '.' '0'-'9']+
let spacing = ['\t' ' ' '\n']

rule bibtex =
  parse
    ['\t' ' ' '\n'] { bibtex lexbuf }
  | identifier as i { IDENTIFIER(i) }
  | [')' '}'] { outside lexbuf; END }
  | '@' (identifier as i) spacing* ['(' '{'] { AT(i) }
  | ',' { COMMA }
  | '=' { EQ }
  | '"' { VALUE(qvalue lexbuf) }
  | '{' { VALUE(bvalue 0 lexbuf) }
  | eof { raise Eof }
and outside =
  parse
  [^ '@' ]* { () }
  | eof { raise Eof }
and qvalue =
  parse
    '"' { "" }
  | '{' { let s = bvalue 0 lexbuf in s ^ (qvalue lexbuf) }
  | [^'{' '"']* as s { s ^ (qvalue lexbuf) }
and bvalue n =
  parse
    '}' { if n = 0 then "" else bvalue (n-1) lexbuf }
  | '{' { bvalue (n+1) lexbuf }
  | ([^ '{' '}']* as s) { s ^ (bvalue n lexbuf) }
