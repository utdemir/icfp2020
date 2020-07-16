{-# LANGUAGE TemplateHaskell #-}

module Main where

import Hedgehog
import Hedgehog.Main

prop_test :: Property
prop_test = property $ do
  "Main" === "Main"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
