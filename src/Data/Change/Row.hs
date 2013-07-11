-- |
-- Module     : Data.Change.Row
-- Copyright  : Copyright (C) 2013  Leigh Simpson <code@simpleigh.com>
-- License    : GNU LGPL 2.1
--
module Data.Change.Row (
    -- * Row type
    Row(),
    
    -- * Creating rows
    rounds,
    queens,
    kings,
    tittums,
    reverseRounds,
    cyclic,
    pblh,
    
    -- * Accessing rows
    toList,
    getBell,
    findBell,
    
    -- * Row transpositions
    transpose,
    inverse,
    
    -- * Properties of rows
    bells,
    isRounds,
    isPblh,
    sign,
    index,
    cycles,
    order,
    
    -- * Additional types
    Length(),
    getLength,
    makeLength,
    Parity(Even, Odd),
    
    ) where

import Data.Change.Bell
import qualified Data.List (transpose)

-- | The row type
-- A row is defined as a list of bells with the following restrictions:
--     1. Bells may not be repeated
--     2. Bells may not be omitted
-- Rows therefore act as mathematical permutations, and may be manipulated
-- as such.
newtype Row =
    Row { -- | Returns the row as a @Bell@ list.
          toList :: [Bell]
    } deriving (Eq, Show, Read)

-- | Construct rounds on @n@ bells.
rounds            :: Length -> Row
rounds (Length n) = Row [fromInt 1 .. fromInt n]

-- | Construct queens on @n@ bells.
queens            :: Length -> Row
queens (Length n) = Row $ [fromInt 1, fromInt 3 .. fromInt n] ++ [fromInt 2, fromInt 4 .. fromInt n]

-- | Construct kings on @n@ bells.
kings            :: Length -> Row
kings (Length 1) = Row [fromInt 1]
kings (Length n) = Row $ reverse [fromInt 1, fromInt 3 .. fromInt n] ++ [fromInt 2, fromInt 4 .. fromInt n]

-- | Construct tittums on @n@ bells.
tittums            :: Length -> Row
tittums (Length n) = Row $ concat $ Data.List.transpose [[fromInt 1 .. fromInt half], [fromInt $ half + 1 .. fromInt n]]
                         where half = (n + 1) `div` 2

-- | Construct reverse rounds on @n@ bells.
reverseRounds            :: Length -> Row
reverseRounds (Length n) = Row $ reverse [fromInt 1 .. fromInt n]

-- | Construct a cyclic lead head.
-- Computed as @(13456..2)^c@ on @n@ bells with @h@ hunt bells.
cyclic       :: Length -- ^ Number of bells, @n@
             -> Length -- ^ Number of hunt bells, @h@
             -> Int -- ^ Lead head number, @c@
             -> Row
cyclic _ _ _ = error "Not implemented"

-- | Construct a plain bob lead head.
-- Returns the first plain bob lead head on @n@ bells with @h@ hunt bells.
pblh     :: Length -- ^ Number of bells, @n@
         -> Length -- ^ Number of hunt bells, @h@
         -> Row
pblh _ _ = error "Not implemented"

-- | Retrieve a bell by index.
getBell            :: Row -> Int -> Bell
getBell (Row bs) i = bs !! (i - 1)

-- | Find the index of a bell. Indices are 1-based, i.e. they are between 1 and
-- @n@, the number of bells.
findBell                 :: Row -> Bell -> Int
findBell (Row bs) b | toInt b > length bs = error "Bell out of range"
                    | otherwise           = (1+) $ length $ takeWhile (/= b) bs

-- | Transpose a row by another
transpose                 :: Row -> Row -> Row
transpose (Row _) (Row _) = error "Not implemented"

-- | Find the inverse of a row
inverse         :: Row -> Row
inverse (Row _) = error "Not implemented"

-- | How many bells are in the row?
bells          :: Row -> Length
bells (Row bs) = Length $ length bs

-- | Is the row rounds?
isRounds   :: Row -> Bool
isRounds r = r == (rounds . bells $ r)

-- | Is the row a plain bob lead head?
isPblh :: Row -- ^ Row
       -> Length -- ^ Number of hunt bells, @h@
       -> Maybe Length
isPblh _ _ = error "Not implemented"

-- | Is the row odd or even?
sign   :: Row -> Parity
sign _ = error "Not implemented"

-- | Calculates a unique index for the row
index   :: Row -> Integer
index _ = error "Not implemented"

-- | Express the row as a product of disjoint cycles.
cycles   :: Row -> [[Bell]]
cycles _ = error "Not implemented"

-- | Calculate the order of the row.
order   :: Row -> Integer
order _ = error "Not implemented"

instance Ord Row where
    compare x y | cx /= cy  = compare cx cy
                | otherwise = compare ix iy
        where cx = bells x
              cy = bells y
              ix = index x
              iy = index y

-- | Type representing the length of a row
newtype Length =
    Length { -- | Access row length.
             getLength :: Int
           } deriving (Eq, Ord, Show, Read)

-- | Construct a row length
makeLength :: Int -> Length
makeLength i | i < minLength = error $ "Length must be >= " ++ show minLength
             | i > maxLength = error $ "Length must be <= " ++ show maxLength
             | otherwise     = Length i
             where minLength = getLength (minBound :: Length)
                   maxLength = getLength (maxBound :: Length)

instance Bounded Length where
    minBound = Length 1
    maxBound = Length $ toInt $ (maxBound :: Bell)

-- | Type representing the parity of a row
data Parity = Even | Odd deriving (Eq, Enum, Show, Read)
