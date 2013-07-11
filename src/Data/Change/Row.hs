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
    
    -- * Properties of rows
    bells,
    isRounds,
    isPblh,
    sign,
    cycles,
    order,
    
    ) where

import Data.Change.Bell
import Data.Change.Row.Parity

-- | The row type
-- A row is defined as a list of bells with the following restrictions:
--     1. Bells may not be repeated
--     2. Bells may not be omitted
-- Rows therefore act as mathematical permutations, and may be manipulated
-- as such.
newtype Row =
    Row { -- | Returns the row as a @Bell@ list.
          toList :: [Bell]
    } deriving (Eq, Ord, Show, Read)

-- | Construct rounds on @n@ bells.
rounds   :: Int -> Row
rounds i = Row [fromInt 1 .. fromInt i]

-- | Construct queens on @n@ bells.
queens   :: Int -> Row
queens i = Row $ [fromInt 1, fromInt 3 .. fromInt i] ++ [fromInt 2, fromInt 4 .. fromInt i]

-- | Construct kings on @n@ bells.
kings   :: Int -> Row
kings 1 = Row [fromInt 1]
kings 2 = Row [fromInt 1, fromInt 2]
kings i = Row $ [fromInt j, fromInt (j - 2) .. fromInt 1] ++ [fromInt 2, fromInt 4 .. fromInt i]
              where j = if even i then i - 1 else i

-- | Construct tittums on @n@ bells.
tittums   :: Int -> Row
tittums _ = error "Not implemented"

-- | Construct reverse rounds on @n@ bells.
reverseRounds   :: Int -> Row
reverseRounds 1 = Row [fromInt 1]
reverseRounds i = Row [fromInt i, fromInt (i - 1) .. fromInt 1]

-- | Construct a cyclic lead head.
-- Computed as @(13456..2)^c@ on @n@ bells with @h@ hunt bells.
cyclic       :: Int -- ^ Number of bells, @n@
             -> Int -- ^ Number of hunt bells, @h@
             -> Int -- ^ Lead head number, @c@
             -> Row
cyclic _ _ _ = error "Not implemented"

-- | Construct a plain bob lead head.
-- Returns the first plain bob lead head on @n@ bells with @h@ hunt bells.
pblh     :: Int -- ^ Number of bells, @n@
         -> Int -- ^ Number of hunt bells, @h@
         -> Row
pblh _ _ = error "Not implemented"

-- | Retrieve a bell by index.
getBell            :: Row -> Int -> Bell
getBell (Row bs) i = (!!) bs (i - 1)

-- | How many bells are in the row?
bells          :: Row -> Int
bells (Row bs) = length bs

-- | Is the row rounds?
isRounds   :: Row -> Bool
isRounds r = r == (rounds . bells $ r)

-- | Is the row a plain bob lead head?
isPblh :: Row -- ^ Row
       -> Int -- ^ Number of hunt bells, @h@
       -> Maybe Int
isPblh _ _ = error "Not implemented"

-- | Is the row odd or even?
sign   :: Row -> Parity
sign _ = error "Not implemented"

-- | Express the row as a product of disjoint cycles.
cycles   :: Row -> [[Bell]]
cycles _ = error "Not implemented"

-- | Calculate the order of the row.
order   :: Row -> Int
order _ = error "Not implemented"
