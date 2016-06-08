{-# LANGUAGE RecordWildCards #-}
import Data.List

data Currency = Currency { unit :: Rational, count :: Integer }
type Reserve = [Currency]

changeOne :: Rational -> Currency -> (Rational, Integer)
changeOne amount Currency{..} =
    let changeCount = min count $ toInteger . floor $ amount/unit
        remainder = amount - toRational changeCount * unit
    in (remainder, changeCount)

--(Rational, [Currency], Reserve)
changeAllGreedy :: Rational -> Reserve -> (Rational, [Currency])
changeAllGreedy amount reserve =
    foldl' changeOneDetail (amount, []) reserve
    where changeOneDetail (remainder, changes) c@Currency{ unit = cunit } =
              let (newRemainder, changeCount) = changeOne remainder c
              in (newRemainder, Currency{ unit = cunit, count = changeCount} : changes)
