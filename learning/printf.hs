{-# LANGUAGE GADTs #-}
-- Statically type-checked printf. Using GADTs and monadic style to solve it.
-- Reference:
-- https://www.microsoft.com/en-us/research/publication/generalized-algebraic-data-types-and-object-oriented-programming/
-- http://www.cs.ox.ac.uk/ralf.hinze/talks/fullcircle.pdf

data Format t where
    Int :: Format t -> Format (Int -> t)
    Lit :: String -> Format t -> Format t
    End :: Format String

mprintf' :: Format t -> String -> t
mprintf' (End) = id
mprintf' (Int rst) = \s i -> mprintf' rst $ s ++ (show i)
mprintf' (Lit s' rst) = (mprintf' rst) . (\s -> s ++ s')

mprintf :: Format t -> t
mprintf = flip mprintf' ""

--Example: mprintf (Int . (Lit " hi ") . Int $ End) 1 3
