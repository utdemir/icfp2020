{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Maybe
import Eval
import Hedgehog hiding (assert, eval)
import Hedgehog.Main
import Parse

assert :: [String] -> String -> Property
assert program expected =
  let ObjectAtom actual = parseAndEval (unlines program) "galaxy"
      ObjectAtom expected' = parseAndEval ("galaxy = " ++ expected) "galaxy"
   in withTests 1 . property $ do
        actual === expected'

assertSimple :: String -> String -> Property
assertSimple program expected = assert ["galaxy = " ++ program] expected

prop_add = assertSimple "ap ap add 1 2" "3"

prop_mul = assertSimple "ap ap mul 3 4" "12"

prop_div = assertSimple "ap ap div 5 -3" "-1"

prop_neg = assertSimple "ap neg 1" "-1"

prop_car = assertSimple "ap car ap ap cons 1 2" "1"

prop_cdr = assertSimple "ap cdr ap ap cons 1 2" "2"

prop_s = assertSimple "ap ap ap s mul ap add 1 6" "42"

prop_c = assertSimple "ap ap ap c add 1 2" "3"

prop_b = assertSimple "ap ap ap b inc dec 6" "6"

prop_isnil_t = assertSimple "ap ap ap isnil nil 1 0" "1"

prop_isnil_f = assertSimple "ap ap ap isnil ap ap cons 10 11 1 0" "0"

prop_eq_t = assertSimple "ap ap ap ap eq 4 4 1 0" "1"

prop_eq_f = assertSimple "ap ap ap ap eq 4 5 1 0" "0"

prop_lt_t = assertSimple "ap ap ap ap lt 4 5 1 0" "1"

prop_lt_f = assertSimple "ap ap ap ap lt 4 4 1 0" "0"

prop_i = assertSimple "ap i 0" "0"

prop_i_2 = assertSimple "ap ap i inc 3" "4"

prop_refs =
  assert
    [ ":1 = 2",
      ":2 = ap add :1",
      ":3 = ap ap add ap :2 3 1",
      "galaxy = :3"
    ]
    "6"

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
