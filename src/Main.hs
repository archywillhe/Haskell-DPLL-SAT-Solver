module Main where
import Grammar
import Tokens
import Data.List

data Satisfiability = SAT | UNSAT deriving (Eq, Show)

type Literal = Int
type Variable = Int
type Clause = [Literal]

type VariableAssignement = (Int,Bool)

--extract variable from literal
exv :: Literal -> Variable
exv l 
    | l > 0 = l
    | l < 0 = -l

-- cnf With a certain Literal that yields True
assignTrueFor ::  [Clause] -> Variable -> [Clause]
(assignTrueFor) cnf literal = [ (- literal) `delete` clause  | clause <- cnf, (not $ literal `elem` clause)]
--unit propagation
dpll :: ([Variable], [Clause]) -> [VariableAssignement] ->(Satisfiability,[VariableAssignement])

dpll (vss,cnf) as
    | [] `elem` cnf = (UNSAT,[])
    | [] == vss = (SAT,as)
    | let (result,list) = process (vs, (cnf `assignTrueFor` x)) ((x,True):as), result == SAT = (SAT,list)
    | otherwise = process (vs, (cnf `assignTrueFor` (-x))) ((x,False):as)
        where (x:vs) = vss

-- process: do Boolean Constraint Propagation && pure literal elmination
process :: ([Variable], [Clause]) -> [VariableAssignement] ->(Satisfiability,[VariableAssignement])
process (vs,cnf) as = dpll (vs',cnf') as
    where 
        (vs',cnf') = bcp_and_ple (vs,cnf) ([],[]) True False
        bcp_and_ple x previous ple_done up_done
            | not ple_done = bcp_and_ple (ple x) x True False --secondly we ple
            | x == previous = x     -- lastly we check if it is the same; 
                                    --if not we go on with the recursive loop 
            | not up_done =  bcp_and_ple (up x) x False True --firstly we do up
            | otherwise = x

-- pure literal elmination
ple (vs,cnf) = (vs',cnf')
    where
        cnf' = foldl (assignTrueFor) cnf pls
        vs' = vs \\ (fmap exv pls)
        pls = fpureLiterals ls [] 
        ls = concat cnf
        fpureLiterals (x:xs) o = fpureLiterals xs o'
            where 
                o'
                    | (x `elem` ls) && ( -x `elem` ls) = o
                    | (-x `elem` ls) = -x:o
                    | otherwise = x:o
        fpureLiterals [] o = o

-- unit propagation

up (vs,cnf)
    | length ucs == 0 = (vs',cnf')
    | otherwise = up (vs',cnf')
    where
        cnf' = foldl (assignTrueFor) cnf ucs
        vs' = vs \\ (fmap exv ucs)
        ucs = [ x | (x:xs) <- cnf, xs == []]


readyForDPLL :: Exp  -> ([Variable], [Clause])
readyForDPLL (WholeFormula a _ b) = ([1..a], makeCNF b)
makeCNF :: Exp -> [Clause]
makeCNF (SubFormula a b) = [makeClause a, makeClause b]
makeCNF (SubFormulaJoined a xs) = makeClause a : makeCNF xs

makeClause :: Exp -> Clause
makeClause (WholeClause a) = makeClause a
makeClause (WholeUnitClause a) = [l a]
makeClause (SubClause a b) = [l a, l b]
makeClause (SubClauseJoined a xs) = l a : makeClause xs

l :: Exp -> Int
l (Negative (Literal x)) =  -x
l (Literal x) = x

main :: IO ()
main = do
    s <- getContents
    print $ dpll ( readyForDPLL $ parseCNF (scanTokens s) )[]
    