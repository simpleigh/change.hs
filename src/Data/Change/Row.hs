-- |
-- Module     : Data.Change.Row
-- Copyright  : Copyright (C) 2013  Leigh Simpson <code@simpleigh.com>
-- License    : GNU LGPL 2.1
--
-- Functions and types for operations involving rows.
module Data.Change.Row (
    -- * Row type
    Row(),
    
    -- ** Creating rows
    fromList,
    rounds,
    queens,
    kings,
    tittums,
    reverseRounds,
    plainBobLeadHead,
    cyclicLeadHead,
    fromNumber,
    
    -- ** Accessing rows
    toList,
    getBell,
    findBell,
    
    -- ** Row transpositions
    transpose,
    divide,
    inverse,
    
    -- ** Properties of rows
    bells,
    isRounds,
    isPlainBobLeadhead,
    isCyclicLeadHead,
    sign,
    number,
    cycles,
    order,
    
    -- * Length type
    Length(),
    
    -- ** Length functions
    getLength,
    makeLength,
    
    -- * Parity type
    Parity(Even, Odd),
    
    -- * Number type
    Number,
    
    ) where

import Data.Change.Bell
import Data.List(sort)
import qualified Data.List(transpose)

-----------------------------------------------------------------------------

-- | The row type
-- A row is defined as a list of bells with the restrictions that bells may not
-- be repeated nor omitted.
-- Rows act as mathematical permutations, and may be manipulated as such.
newtype Row =
    Row { -- | Returns the row as a @Bell@ list.
          toList :: [Bell]
    } deriving (Eq, Read)


-- | Construct a row from a list of bells
fromList    :: [Bell] -> Maybe Row
fromList [] = Nothing
fromList bs | (length bs) > maxLength = Nothing
            | (sort bs) == roundsList = Just $ Row bs
            | otherwise               = Nothing
            where maxLength  = toInt (maxBound :: Bell)
                  roundsList = [fromInt 1 .. fromInt $ length bs]


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


-- | Construct a plain bob lead head.
-- Returns the first plain bob lead head on @n@ bells with @h@ hunt bells.
plainBobLeadHead     :: Length -- ^ Number of bells, @n@
                     -> Length -- ^ Number of hunt bells, @h@
                     -> Row
plainBobLeadHead _ _ = error "Not implemented"


-- | Construct a cyclic lead head.
-- Computed as @(13456..2)^c@ on @n@ bells with @h@ hunt bells.
cyclicLeadHead       :: Length -- ^ Number of bells, @n@
                     -> Length -- ^ Number of hunt bells, @h@
                     -> Int -- ^ Lead head number, @c@
                     -> Row
cyclicLeadHead _ _ _ = error "Not implemented"


-- | Generates a row from an index number.
fromNumber   :: Number -> Row
fromNumber _ = error "Not implemented"


-- | Retrieve a bell by index.
getBell            :: Row -> Int -> Bell
getBell (Row bs) i = bs !! (i - 1)


-- | Find the index of a bell. Indices are 1-based, i.e. they are between 1 and
-- @n@, the number of bells.
findBell                 :: Row -> Bell -> Int
findBell (Row bs) b | toInt b > length bs = error "Bell out of range"
                    | otherwise           = (1+) $ length $ takeWhile (/= b) bs


-- | Transpose a row by another.
transpose     :: Row -> Row -> Row
transpose x y | bells x /= bells y = error "Mismatched row length"
              | otherwise          = Row [getBell x (toInt b) | b <- toList y]


-- | Divide a row by another.
divide     :: Row -> Row -> Row
divide x y | bells x /= bells y = error "Mismatched row length"
           | otherwise          = transpose x (inverse y)


-- | Find the inverse of a row.
inverse   :: Row -> Row
inverse r = Row [fromInt $ findBell r b | b <- toList $ rounds $ bells r]


-- | How many bells are in the row?
bells          :: Row -> Length
bells (Row bs) = Length $ length bs


-- | Is the row rounds?
isRounds   :: Row -> Bool
isRounds r = r == (rounds . bells $ r)


-- | Is the row a plain bob lead head?
isPlainBobLeadhead   :: Row -> Maybe (Length, Int)
isPlainBobLeadhead _ = error "Not implemented"


-- | Is the row a cyclic lead head?
isCyclicLeadHead   :: Row -> Maybe (Length, Int)
isCyclicLeadHead _ = error "Not implemented"


-- | Is the row odd or even?
sign   :: Row -> Parity
sign _ = error "Not implemented"


-- | Calculates a unique index number for the row.
number   :: Row -> Number
number _ = error "Not implemented"


-- | Express the row as a product of disjoint cycles.
cycles   :: Row -> [[Bell]]
cycles _ = error "Not implemented"


-- | Calculate the order of the row.
order   :: Row -> Integer
order _ = error "Not implemented"


instance Show Row where
    showsPrec _ r = foldr (.) id (map ((:) . toChar) (toList r))


instance Ord Row where
    compare x y | cx /= cy  = compare cx cy
                | otherwise = compare ix iy
        where cx = bells x
              cy = bells y
              ix = number x
              iy = number y

-----------------------------------------------------------------------------

-- | Type representing the length of a row.
newtype Length =
    Length { -- | Access row length.
             getLength :: Int
           } deriving (Eq, Ord, Show, Read)


-- | Construct a row length.
makeLength   :: Int -> Length
makeLength i | i < minLength = error $ "Length must be >= " ++ show minLength
             | i > maxLength = error $ "Length must be <= " ++ show maxLength
             | otherwise     = Length i
             where minLength = getLength (minBound :: Length)
                   maxLength = getLength (maxBound :: Length)


instance Bounded Length where
    minBound = Length 1
    maxBound = Length $ toInt $ (maxBound :: Bell)

-----------------------------------------------------------------------------

-- | The parity of a row.
-- Rows are defined as even or odd.
-- Even rows require an even number of pairwise swaps to return to rounds.
data Parity = Even | Odd deriving (Eq, Enum, Show, Read)

-----------------------------------------------------------------------------

-- | Type representing a row number.
-- A row may be identified by a unique row number in the range @1 .. n!@ where
-- @n@ is the number of bells.
type Number = Integer
