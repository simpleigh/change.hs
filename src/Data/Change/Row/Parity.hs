-- |
-- Module     : Data.Change.Row.Parity
-- Copyright  : Copyright (C) 2013  Leigh Simpson <code@simpleigh.com>
-- License    : GNU LGPL 2.1
--
module Data.Change.Row.Parity (
    Parity(Even, Odd)
    ) where

-- | Type representing the parity of a row
data Parity = Even | Odd deriving (Eq, Enum, Show, Read)
