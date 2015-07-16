{
module Tokens where
}

%wrapper "basic"

$digit = 0-9
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  p                             ;
  cnf      { \s -> TokenStartFormula }
  0             { \s -> TokenEndClause }
  $digit+       { \s -> TokenInt (read s) }
  \-            { \s -> TokenMinus }

{

-- The token type:
data Token = TokenInt Int
        | TokenMinus
        | TokenEndClause
        | TokenStartFormula
        deriving (Eq,Show)

scanTokens = alexScanTokens

}
