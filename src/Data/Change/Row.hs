module Data.Change.Row (
    Row(),
    rounds
    ) where

import Data.Change.Bell

newtype Row = Row [Bell] deriving (Eq, Ord, Show, Read)

rounds   :: Int -> Row
rounds i = Row [ fromInt 1 .. fromInt i]
