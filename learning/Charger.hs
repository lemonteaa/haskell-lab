{-# LANGUAGE RecordWildCards #-}
-- Strange. This is so clunky it seems to be worse than imperative when functional style
-- is supposed to be powerful. Need more practice ;)
import Data.List

data Currency = Currency { unit :: Rational, count :: Integer } deriving (Show)
type Reserve = [Currency]

changeOne :: Rational -> Currency -> (Rational, Integer)
changeOne amount Currency{..} =
    let changeCount = min count $ toInteger . floor $ amount/unit
        remainder = amount - toRational changeCount * unit
    in (remainder, changeCount)

changeAllGreedy :: Rational -> Reserve -> (Rational, [Currency], Reserve)
changeAllGreedy amount reserve =
    let (finalRemainder, changes) = foldl' changeOneDetail (amount, []) reserve
        reserveAfterChange = zipWith deduct reserve (reverse changes)
    in (finalRemainder, changes, reserveAfterChange)
    where changeOneDetail (remainder, changes) c@Currency{ unit = cunit } =
              let (newRemainder, changeCount) = changeOne remainder c
              in (newRemainder, Currency{ unit = cunit, count = changeCount} : changes)
          deduct Currency{unit=cunit, count=original} Currency{count=used} =
              Currency{unit=cunit, count=(original - used)}
