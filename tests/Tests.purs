module Main where

import Control.Monad.Eff
import Debug.Trace
import Test.QuickCheck

foreign import uniq
  "function uniq(arr) {\
  \  return require('underscore').uniq(arr);\
  \}" :: forall a. [a] -> [a]

main = do

  trace "foreign import underscore uniq: reduce an array to a set"
  quickCheck $ \n -> uniq [n, n, -1] == [n, -1] <?> "uniq failed " ++ show n
