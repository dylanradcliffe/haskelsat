{-# OPTIONS -Wall #-}
module Main where

import Data.List
import Data.List.Split
import Data.Maybe (mapMaybe)
import Debug.Trace

main :: IO ()
main = do
  contents <- getContents
  print $ naiveSolve $ parseDimacs contents

-- Key types
-- assignments are integers representing
-- the variables, positive for True
-- negative for false
type Assignments = [Int]

data Clause = Clause Assignments deriving (Show)

data Expr = Expr [Clause] deriving (Show)

data SatRes = Sat Assignments | Unsat deriving (Show)

data Cnf = Cnf Expr Int deriving (Show) -- Int is number of vars
-- DIMACS parser

{--
c Comment
p cnf 4 3
1 3 -4 0
4 0 2
-3

> 1 3 -4
> 4
> 2 -3
--}

parseDimacs :: String -> Cnf
parseDimacs s = parseDimacsClauses $ map read $ tokens
  where
    tokens = filter (/= "") $ splitOn " " $ unwords $ dropHeaderLines $ lines s

dropHeaderLines :: [String] -> [String]
dropHeaderLines (l : ls)
  | head l == 'c' || head l == 'p' = dropHeaderLines ls
dropHeaderLines a = a

parseDimacsClauses :: [Int] -> Cnf
parseDimacsClauses ns =
  let -- must terminate with 0 so last split needs to be dropped
      clauses = splitOn [0] ns
      clauses_trunced = if null $ last clauses then init clauses else clauses
      expr = Expr $ map Clause clauses_trunced
      maxVar = maximum $ map abs $ ns
   in Cnf expr maxVar

-- Naive Solver
-- Brute force, no heuristics or optimisations
naiveSolve :: Cnf -> SatRes
naiveSolve (Cnf expr _) = naiveSolveFrom [] expr

naiveSolveFrom :: Assignments -> Expr -> SatRes
naiveSolveFrom as exp
  | isUnsat reduced = Unsat
  | null reduced = Sat (sortByVar as)
  | otherwise =
      let nu = nextUnassigned as
          -- try1 = trace ("trying " ++ (show (-nu : as))) naiveSolveFrom (-nu : as) exp
          try1 = naiveSolveFrom (-nu : as) exp
          -- try2 = trace ("trying " ++ (show (nu : as))) naiveSolveFrom (nu : as) exp
          try2 = naiveSolveFrom (nu : as) exp
       in -- make another assignment
          -- trace ("reduced: " ++ (show reduced) ++ " assigned:" ++ (show as)) $
          combine try1 try2
  where
    Expr reduced = reduce as exp

isUnsat :: [Clause] -> Bool
isUnsat [] = False
isUnsat (Clause [] : _) = True
isUnsat (_ : cs) = isUnsat cs

reduce :: Assignments -> Expr -> Expr
reduce as (Expr cs) = Expr $ mapMaybe (reduceClause as) cs

reduceClause :: Assignments -> Clause -> Maybe Clause
reduceClause [] c = Just c
reduceClause (a : as) (Clause c)
  | elem a c = Nothing -- remove clause
  | otherwise = reduceClause as $ Clause $ filter (/= -a) c -- remove negated term if present

combine :: SatRes -> SatRes -> SatRes
combine Unsat Unsat = Unsat
combine (Sat as) _ = Sat as
combine _ (Sat as) = Sat as

nextUnassigned :: Assignments -> Int
nextUnassigned [] = 1
nextUnassigned as = maximum (map abs as) + 1

sortByVar :: [Int] -> [Int]
sortByVar = sortBy (\a b -> compare (abs a) (abs b))