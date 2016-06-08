{-# LANGUAGE RecordWildCards #-}
data Currency = Currency { unit :: Rational, count :: Integer }
type Reserve = [Currency]

changeOne :: Rational -> Currency -> (Rational, Integer)
changeOne amount Currency{..} =
	let changeCount = min count $ toInteger . floor $ amount/unit in
		(amount - toRational changeCount * unit, changeCount)
