module Change.Bell (
         Bell(),
         fromInt,
         toInt,
         fromChar,
         toChar
       ) where

import Data.List

data Bell = Bell Int deriving (Eq, Ord, Show, Read)

instance Bounded Bell where
    minBound = Bell 1
    maxBound = Bell $ length bellChars

instance Enum Bell where
    succ x           = fromInt $ toInt x + 1
    pred x           = fromInt $ toInt x - 1
    toEnum           = fromInt
    fromEnum         = toInt
    enumFrom x       = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y bound
                           where
                               bound | fromEnum y >= fromEnum x = maxBound
                                     | otherwise                = minBound

fromInt :: Int -> Bell
fromInt i | i < min = error $ "Bell number must be >= " ++ show min
          | i > max = error $ "Bell number must be <= " ++ show max
          | otherwise = Bell i
          where min = toInt (minBound :: Bell)
                max = toInt (maxBound :: Bell)

toInt :: Bell -> Int
toInt (Bell i) = i

bellChars :: String
bellChars = "1234567890ETABCDFGHJKLMNPQRSUVWY"

fromChar :: Char -> Bell
fromChar c = Bell $ x + 1 where Just x = elemIndex c bellChars

toChar :: Bell -> Char
toChar x = bellChars !! (toInt x - 1)
