module Change.Bell (
         Bell(),
         fromInt,
         toInt,
         fromChar,
         toChar
       ) where

import Data.List

newtype Bell = Bell Int deriving (Eq, Ord)

instance Show Bell where
    showsPrec _ x = (:) $ toChar x

instance Read Bell where
    readsPrec _ []     = []
    readsPrec _ (c:cs) = case fromChar c of
                             Just b  -> [(b, cs)]
                             Nothing -> []

instance Bounded Bell where
    minBound = Bell 1
    maxBound = Bell $ length bellChars

instance Enum Bell where
    succ x           = fromInt $ toInt x + 1
    pred x           = fromInt $ toInt x - 1
    toEnum           = fromInt
    fromEnum         = toInt
    enumFrom x       = enumFromTo x maxBound
    enumFromThen x y = enumFromThenTo x y bound where
                           bound | fromEnum y >= fromEnum x = maxBound
                                 | otherwise                = minBound

fromInt                 :: Int -> Bell
fromInt i | i < minBell = error $ "Bell number must be >= " ++ show minBell
          | i > maxBell = error $ "Bell number must be <= " ++ show maxBell
          | otherwise   = Bell i
          where minBell = toInt (minBound :: Bell)
                maxBell = toInt (maxBound :: Bell)

toInt          :: Bell -> Int
toInt (Bell i) = i

bellChars :: String
bellChars = "1234567890ETABCDFGHJKLMNPQRSUVWY"

fromChar   :: Char -> Maybe Bell
fromChar c = fmap (fromInt . (+1)) $ elemIndex c bellChars

toChar   :: Bell -> Char
toChar x = bellChars !! (toInt x - 1)
