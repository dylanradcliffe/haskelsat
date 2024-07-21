{-# OPTIONS -Wall #-}
module Main where

-- import Data.List
import Data.List.Split

-- import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  contents <- getContents
  print $ parseDimacs contents

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
parseDimacs s = parseDimacsClauses $ map read $ splitOn " " $ unwords $ dropHeaderLines $ lines s

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

{--
-- Naive Solver
-- Brute force, no heuristics
naiveSolve :: Expr -> SatRes
naiveSolve = naiveSolveFrom []

naiveSolveFrom :: Assignments -> Expr -> SatRes
naiveSolveFrom as exp
  | isUnsat reduced = Unsat

  | null reduced = Sat as
  | otherwise = let
      try1 =
    in  -- make another assignment
  where
    Expr reduced = reduce as exp

isUnsat :: p -> Bool
isUnsat _ = False

reduce _ e = e

combine :: SatRes -> SatRes -> SatRes
combine Unsat Unsat = Unsat
combine (Sat as) _ = Sat as
combine _ (Sat as) = Sat as

nextUnassigned
--}