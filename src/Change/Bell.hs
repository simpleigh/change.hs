module Change.Bell (
    -- * Bell type
    Bell(),
    
    -- * Conversions to and from Int
    fromInt,
    toInt,
    
    -- * Conversions to and from Char
    fromChar,
    toChar
    
    ) where

import Data.List

-- | The Bell data type.
-- The lowest-level component of the library, representing an individual bell.
-- Internally a Bell is represented as an Int.
-- Functions are provided for easy conversion between other types.
newtype Bell = Bell Int deriving (Eq, Ord)

-- | Make a bell from an Int bell number.
fromInt                 :: Int -> Bell
fromInt i | i < minBell = error $ "Bell number must be >= " ++ show minBell
          | i > maxBell = error $ "Bell number must be <= " ++ show maxBell
          | otherwise   = Bell i
          where minBell = toInt (minBound :: Bell)
                maxBell = toInt (maxBound :: Bell)

-- | Get Int bell number.
toInt          :: Bell -> Int
toInt (Bell i) = i

-- | Attempt to produce a bell from a Char.
-- The characters @1234567890ETABCDFGHJKLMNPQRSUVWY@ are used to represent
-- bells. Passing an invalid character will return @Nothing@.
fromChar   :: Char -> Maybe Bell
fromChar c = fmap (fromInt . (+1)) $ elemIndex c bellChars

-- | Get bell letter.
toChar   :: Bell -> Char
toChar x = bellChars !! (toInt x - 1)

bellChars :: String
bellChars = "1234567890ETABCDFGHJKLMNPQRSUVWY"

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
