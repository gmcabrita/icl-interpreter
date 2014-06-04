{
module Parser where

import Lexer
import Syntax
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    num         { TokenNum $$ }
    id          { TokenId $$ }
    str         { TokenStr $$ }
    'while'     { TokenWhile }
    '{'         { TokenLBracket }
    '}'         { TokenRBracket }
    '('         { TokenLParen }
    ')'         { TokenRParen }
    ';'         { TokenSemicolon }
    'if'        { TokenIf }
    'else'      { TokenElse }
    '='         { TokenEquals }
    'print'     { TokenPrint }
    'println'   { TokenPrintln }
    'free'      { TokenFree }
    'function'  { TokenFunction}
    '+'         { TokenPlus }
    '-'         { TokenMinus }
    '*'         { TokenStar }
    '/'         { TokenForwardSlash }
    '<='        { TokenLET }
    '<'         { TokenLT }
    '>'         { TokenGT }
    '>='        { TokenGET }
    '=='        { TokenEQ }
    'true'      { TokenTrue }
    'false'     { TokenFalse }
    'and'       { TokenAnd }
    'or'        { TokenOr }
    'not'       { TokenNot }
    '?'         { TokenQuestionMark }
    ':'         { TokenColon }
    'alloc'     { TokenAlloc }
    'val'       { TokenVal }
    'return'    { TokenReturn }
    ','         { TokenComma }
    'fun'       { TokenFun }
    'int'       { TokenInt }
    'bool'      { TokenBool }
    'string'    { TokenString }

%nonassoc '>' '<' '>=' '<=' '=='
%left ';'
%left '(' ')'
%right '?' ':'
%left 'if' 'else'
%left '/'
%left '+' '-'
%left '<' '<='
%left '>' '>='
%left '=='
%left 'and'
%left 'or'
%left 'not'
%left '*' id
%%

Start :
      E                                             { $1 }
;

S :
      '{' D S '}'                                   { SDecl $2 $3 }
    | 'while' '(' E ')' S                           { While $3 $5 }
    | S ';' S                                       { Seq $1 $3 }
    | E '(' ES ')'                                  { Call $1 $3 }
    | 'if' '(' E ')' S                              { If $3 $5 }
    | 'if' '(' E ')' S 'else' S                     { IfElse $3 $5 $7 }
    | E '=' E                                       { Assign $1 $3 }
    | 'print' '(' E ')'                             { Print $3 }
    | 'println' '(' ')'                             { Println }
    | 'free' '(' E ')'                              { Free $3 }
;

E :
      B                                             { $1 }
    | id                                            { Id $1 }
    | 'function' '(' PS ')' B                       { Lambda $3 $5 }
    | E '(' ES ')'                                  { Apply $1 $3 }
    | str                                           { Str $1 }
    | num                                           { Num $1 }
    | E '+' E                                       { Add $1 $3 }
    | E '-' E                                       { Subtract $1 $3 }
    | E '*' E                                       { Multiply $1 $3 }
    | E '/' E                                       { Divide $1 $3 }
    | E '<=' E                                      { Let $1 $3 }
    | E '<' E                                       { Lt $1 $3 }
    | E '>' E                                       { Gt $1 $3 }
    | E '>=' E                                      { Get $1 $3 }
    | E '==' E                                      { Eq $1 $3 }
    | 'true'                                        { VTrue }
    | 'false'                                       { VFalse }
    | E 'and' E                                     { And $1 $3 }
    | E 'or' E                                      { Or $1 $3 }
    | 'not' E                                       { Not $2 }
    | E '?' E ':' E                                 { Ternary $1 $3 $5 }
    | '(' E ')'                                     { $2 }
    | 'alloc' '(' E ')'                             { Alloc $3 }
    | '*' E                                         { Deref $2 }
;

D :
      'val' id '=' E ';' D                          { ($2,$4):$6 }
    | 'function' id '(' PS ')' B D                  { ($2,(Lambda $4 $6)):$7 }
    |                                               { [] }
;

B :
      '{' D S ';' 'return' E ';' '}'                { LDecl $2 $3 $6 }
;

PS :
      X                                             { $1 }
    |                                               { [] }
;

X :
      id ':' T                                      { [($1, $3)] }
    | id ':' T ',' X                                { ($1, $3):$5 }
;

ES :
      Y                                             { $1 }
    |                                               { [] }
;

Y :
      E                                             { [$1] }
    | E ',' Y                                       { $1:$3 }
;

T :
      'int'                                         { IntType }
    | 'bool'                                        { BoolType }
    | 'string'                                      { StrType }
    | '*' T                                         { RefType $2 }
    | 'fun' '(' W ')'                               { FunType (init $3) (last $3) }
;

--TS :
--                                                    { [] }
--    | W                                             { $1 }
--;

W :
      T                                             { $1:[] }
    | T ',' W                                       { $1:$3 }
;

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}
