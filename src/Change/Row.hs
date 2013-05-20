module Change.Row (
         Row(),
         rounds
       ) where

import Change.Bell

data Row = Row [Bell] deriving (Eq, Ord, Show, Read)

rounds :: Int -> Row
rounds i = Row [ fromInt 1 .. fromInt i]
