-- | Example of Haskell to hardware via reification and CCC

module Main where

import ReificationRules.Run (go)

main :: IO ()
main = go "not"  not
