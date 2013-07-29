{-# OPTIONS_GHC -Wall #-}
-- |
-- Module     : Data.Change.Bell
-- Copyright  : Copyright (C) 2013  Leigh Simpson <code@simpleigh.com>
-- License    : GNU LGPL 2.1
-- 
-- This module provides the 'Bell' type and functions for manipulating bells.
module Data.Change.Bell (
    -- * Bell
    Bell(),
    
    -- ** Conversions
    fromInt,
    toInt,
    fromChar,
    toChar,
    
    ) where

import Data.List

-----------------------------------------------------------------------------

-- | The Bell data type.
-- The lowest-level component of the library, representing an individual bell.
-- Internally a bell is represented as an 'Int',
-- but this is encapsulated and additional functionality provided:
-- 
-- * A bell is bounded, being between 1 and 32.
--   The 'Bounded' typeclass provides access to 'maxBound' and 'minBound'
-- 
-- * 'Show' and 'Read' use the standardised bell lettering
--   @1234567890ETABCDFGHJKLMNPQRSUVWY@.
--
-- * The 'Enum' typeclass is implemented completely.
--
-- * Additional functions allow easy conversion to other types.
newtype Bell =
    Bell { -- | Get the number of a bell.
           -- 
           -- >>> toInt $ read "E"
           -- 11
           toInt :: Int
         } deriving (Eq, Ord)

-----------------------------------------------------------------------------

-- | Make a bell from a number.
-- 
-- >>> fromInt 11
-- E
-- 
-- An exception is thrown if the supplied number is outside the valid bounds.
fromInt                 :: Int -> Bell
fromInt i | i < minBell = error $ "Bell number must be >= " ++ show minBell
          | i > maxBell = error $ "Bell number must be <= " ++ show maxBell
          | otherwise   = Bell i
          where minBell = toInt (minBound :: Bell)
                maxBell = toInt (maxBound :: Bell)


-- | Attempt to make a bell from a letter.
-- 
-- >>> fromChar 'E'
-- Just E
fromChar   :: Char -> Maybe Bell
fromChar c = fmap (fromInt . (+1)) $ elemIndex c bellChars


-- | Get the letter associated with a bell.
-- 
-- >>> toChar $ fromInt 11
-- 'E'
toChar   :: Bell -> Char
toChar x = bellChars !! (toInt x - 1)


-- | Mapping of characters to bells.
bellChars :: String
bellChars = "1234567890ETABCDFGHJKLMNPQRSUVWY"

-----------------------------------------------------------------------------

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
