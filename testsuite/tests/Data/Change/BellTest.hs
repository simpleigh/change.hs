{-# OPTIONS_GHC -F -pgmF htfpp #-}

import Test.Framework

import Data.Change.Bell

test_test = assertEqual (toChar $ fromChar '1') '1'

main :: IO ()
main = htfMain htf_thisModulesTests
