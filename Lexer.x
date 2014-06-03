{

module Lexer where
}

%wrapper "basic"

$digit = 0-9                -- digits
$alpha = [a-zA-Z]           -- alphabetic characters
$alphanum = [a-zA-Z0-9]     -- string
$nl    = [\n\r]             -- newline

tokens :-

  $nl+                      ;--{ skip }
  $white+                   ;
  $digit+                   { \s -> TokenNum (read s) }
  while                     { \_ -> TokenWhile }
  \{                        { \_ -> TokenLBracket }
  \}                        { \_ -> TokenRBracket }
  \(                        { \_ -> TokenLParen }
  \)                        { \_ -> TokenRParen }
  \;                        { \_ -> TokenSemicolon }
  if                        { \_ -> TokenIf }
  else                      { \_ -> TokenElse }
  \=                        { \_ -> TokenEquals }
  print                     { \_ -> TokenPrint }
  println                   { \_ -> TokenPrintln }
  free                      { \_ -> TokenFree }
  function                  { \_ -> TokenFunction}
  \+                        { \_ -> TokenPlus }
  \-                        { \_ -> TokenMinus }
  \*                        { \_ -> TokenStar }
  \/                        { \_ -> TokenForwardSlash }
  \<=                       { \_ -> TokenLET }
  \<                        { \_ -> TokenLT }
  \>                        { \_ -> TokenGT }
  \>=                       { \_ -> TokenGET }
  \==                       { \_ -> TokenEQ }
  true                      { \_ -> TokenTrue }
  false                     { \_ -> TokenFalse }
  and                       { \_ -> TokenAnd }
  or                        { \_ -> TokenOr }
  not                       { \_ -> TokenNot }
  \?                        { \_ -> TokenQuestionMark }
  \:                        { \_ -> TokenColon }
  alloc                     { \_ -> TokenAlloc }
  val                       { \_ -> TokenVal }
  return                    { \_ -> TokenReturn }
  \,                        { \_ -> TokenComma }
  fun                       { \_ -> TokenFun }
  int                       { \_ -> TokenInt }
  bool                      { \_ -> TokenBool }
  string                    { \_ -> TokenString }
  $alpha $alphanum*         { \s -> TokenId s }
  \" $alphanum* \"          { \s -> TokenStr $ init $ tail s }




{

data Token =
    TokenNum Int
  | TokenId String
  | TokenStr String
  | TokenWhile
  | TokenLBracket
  | TokenRBracket
  | TokenLParen
  | TokenRParen
  | TokenSemicolon
  | TokenIf
  | TokenElse
  | TokenEquals
  | TokenPrint
  | TokenPrintln
  | TokenFree
  | TokenFunction
  | TokenPlus
  | TokenMinus
  | TokenStar
  | TokenForwardSlash
  | TokenLET
  | TokenLT
  | TokenGT
  | TokenGET
  | TokenEQ
  | TokenTrue
  | TokenFalse
  | TokenAnd
  | TokenOr
  | TokenNot
  | TokenQuestionMark
  | TokenColon
  | TokenAlloc
  | TokenVal
  | TokenReturn
  | TokenComma
  | TokenFun
  | TokenInt
  | TokenBool
  | TokenString
  deriving (Eq, Show )

}
