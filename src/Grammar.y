{
module Grammar where
import Tokens
}

%name parseCNF
%tokentype { Token }
%error { parseError }

%token
    '-' { TokenMinus }
    int { TokenInt $$ }
    '0' { TokenEndClause}
    '>' { TokenStartFormula} 

%left '-'
%%

WholeFormula: '>' int int SubFormula {WholeFormula $2 $3 $4 }

SubFormula: WholeClause WholeClause  {SubFormula $1 $2}
    | WholeClause SubFormula     {SubFormulaJoined $1 $2}

WholeClause: SubClause '0' {WholeClause $1 }
    | Literal '0' {WholeUnitClause $1}

SubClause: Literal Literal  {SubClause $1 $2}
    | Literal SubClause     {SubClauseJoined $1 $2}

Literal : '-' Literal        { Negative $2 }
    | int                    { Literal $1 }

{

parseError :: [Token] -> a
parseError _ = error "Parse error"


data Exp = WholeFormula Int Int Exp
        | SubFormula Exp Exp
        | SubFormulaJoined Exp Exp
        | WholeClause Exp
        | WholeUnitClause Exp
        | SubClause Exp Exp
        | SubClauseJoined Exp Exp
        | Negative Exp
        | Literal Int
        deriving Show
}
