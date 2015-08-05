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

exvWithValue:: Literal -> VariableAssignement
exvWithValue l 
    | l > 0 = (l,True)
    | l < 0 = (-l,False)

assignTrueFor ::  [Clause] -> Variable -> [Clause]
(assignTrueFor) cnf literal = [ (- literal) `delete` clause  | clause <- cnf, (not $ literal `elem` clause)]

hasConflict = (elem) []

dpll :: ([Variable], [Clause]) -> [VariableAssignement] ->(Satisfiability,[VariableAssignement])
dpll (vss@(x:vs),cnf) as
    | hasConflict cnf = (UNSAT,[])
    | let (result,list) = enterDecisionLevelWAL x, 
        result == SAT = (result,list)
    | otherwise = enterDecisionLevelWAL (-x)
        -- enterDecisionLevelWAL: enter Decision Level With Assigned Literal
        where enterDecisionLevelWAL theVariable = do_up_and_ple (vs, (cnf `assignTrueFor` theVariable)) (exvWithValue theVariable:as)
dpll ([],_) as = (SAT,as)

-- do_up_and_ple: do unit propagation && pure literal elmination
do_up_and_ple :: ([Variable], [Clause]) -> [VariableAssignement] ->(Satisfiability,[VariableAssignement])
do_up_and_ple (vs,cnf) as = dpll (vs',cnf') as'
    where 
        ((vs',cnf'),as') = up_and_ple ((vs,cnf),as)
        up_and_ple x = check_if_ple_gets_same_result (ple x') x'
            where x' = (up (ple x)) 
        check_if_ple_gets_same_result x previous
            | x == previous = x
            | otherwise = up_and_ple x

-- pure literal elmination
ple ((vs,cnf),as)
    | length pls == 0 = ((vs,cnf),as)
    | otherwise = up ((vs',cnf'),as')
    where
        cnf' = foldl (assignTrueFor) cnf pls
        vs' = vs \\ (fmap exv pls)
        as' = as ++ (fmap exvWithValue pls)
        pls = nubBy (==) $ find_pure_literals literals [] 
        literals = concat cnf
        find_pure_literals (x:xs) o = find_pure_literals xs o'
            where 
                o'
                    | (x `elem` literals) && ( -x `elem` literals) = o
                    | (-x `elem` literals) = -x:o
                    | otherwise = x:o
        find_pure_literals [] o = o

-- unit propagation
up ((vs,cnf),as)
    | length ucs == 0 = ((vs,cnf),as)
    | otherwise = up ((vs',cnf'),as')
    where
        cnf' = foldl (assignTrueFor) cnf ucs
        as' = as ++ (fmap exvWithValue ucs)
        vs' = vs \\ (fmap exv ucs)
        ucs = [ x | (x:xs) <- cnf, xs == []]

dpllStart :: ([Variable], [Clause]) ->(Satisfiability,[VariableAssignement])
dpllStart (vs,cnf) = do_up_and_ple (vs, cnf) []


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
    print $ dpllStart ( readyForDPLL $ parseCNF (scanTokens s) )
    