{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.List
import Data.Maybe
import Eval
import Hedgehog hiding (assert, eval)
import qualified Hedgehog.Gen as Gen
import Hedgehog.Main
import qualified Hedgehog.Range as Range
import Parse

assert :: [String] -> String -> Property
assert program expected =
  let ObjectAtom actual = parseAndEval (unlines program) "galaxy"
      ObjectAtom expected' = parseAndEval ("galaxy = " ++ expected) "galaxy"
   in withTests 1 . property $ do
        actual === expected'

assertSimple :: String -> String -> Property
assertSimple program expected = assert ["galaxy = " ++ program] expected

assertHs :: (Eq a, Show a) => [String] -> (Env -> Object -> a) -> a -> Property
assertHs program conv expected =
  withTests 1 . property $
    let env = eval . simpleProgram . parse $ unlines program
        res = resolve env (TermRef "galaxy")
     in conv env res === expected

prop_add = assertSimple "ap ap add 1 2" "3"

prop_mul = assertSimple "ap ap mul 3 4" "12"

prop_div = assertSimple "ap ap div 5 -3" "-1"

prop_neg = assertSimple "ap neg 1" "-1"

prop_car = assertSimple "ap car ap ap cons 1 2" "1"

prop_cdr = assertSimple "ap cdr ap ap cons 1 2" "2"

prop_s = assertSimple "ap ap ap s mul ap add 1 6" "42"

prop_c = assertSimple "ap ap ap c add 1 2" "3"

prop_b = assertSimple "ap ap ap b ap mul 2 dec 6" "10"

prop_t = assertSimple "ap ap ap ap t t ap inc 5 1 0" "1"

prop_f = assertSimple "ap ap f 1 0" "0"

prop_isnil_t = assertSimple "ap ap ap isnil nil 1 0" "1"

prop_isnil_f = assertSimple "ap ap ap isnil ap ap cons 10 11 1 0" "0"

prop_eq_t = assertSimple "ap ap ap ap eq 4 4 1 0" "1"

prop_eq_f = assertSimple "ap ap ap ap eq 4 5 1 0" "0"

prop_lt_t = assertSimple "ap ap ap ap lt 4 5 1 0" "1"

prop_lt_f = assertSimple "ap ap ap ap lt 4 4 1 0" "0"

prop_i = assertSimple "ap i 0" "0"

prop_i_2 = assertSimple "ap ap i inc 3" "4"

prop_toHsBool_t = assertHs ["galaxy = t"] toHsBool True

prop_toHsBool_f = assertHs ["galaxy = f"] toHsBool False

prop_toHsBool_t_2 = assertHs ["galaxy = ap ap lt 2 ap inc 2"] toHsBool True

prop_vecToList_empty =
  assertHs
    ["galaxy = nil"]
    (\env obj -> toHsVec env obj (const ()))
    []

prop_vecToList =
  assertHs
    ["galaxy = ap ap cons 1 ap ap cons ap inc 1 ap ap cons ap inc ap inc 1 nil"]
    (\env obj -> toHsVec env obj (\(ObjectAtom (AtomNum n)) -> n))
    [1, 2, 3]

prop_draw =
  assertHs
    ["galaxy = ap draw ap ap cons ap ap vec 5 3 ap ap cons ap ap vec 6 3 ap ap cons ap ap vec 4 4 ap ap cons ap ap vec 6 4 ap ap cons ap ap vec 4 5 nil"]
    (\_ (ObjectAtom (AtomPicture coords)) -> coords)
    [(5, 3), (6, 3), (4, 4), (6, 4), (4, 5)]

prop_draw_simple =
  assertHs
    ["galaxy = ap draw ap ap cons ap ap cons 1 2 nil"]
    (\_ (ObjectAtom (AtomPicture coords)) -> coords)
    [(1, 2)]

prop_draw_empty =
  assertHs
    ["galaxy = ap draw nil"]
    (\_ (ObjectAtom (AtomPicture coords)) -> coords)
    []

prop_multipledraw =
  assertHs
    [ "galaxy = ap multipledraw ap ap cons p1 ap ap cons p2 ap ap cons p3 nil",
      "p1 = ap ap cons ap ap cons 1 2 nil",
      "p2 = nil",
      "p3 = ap ap cons ap ap cons 3 4 ap ap cons ap ap cons 4 5 ap ap cons ap ap cons 5 6 nil"
    ]
    (\env xs -> toHsVec env xs $ \(ObjectAtom p) -> p)
    [AtomPicture [(1, 2)], AtomPicture [], AtomPicture [(3, 4), (4, 5), (5, 6)]]

prop_modulate_0 =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = 0"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "010"

prop_modulate_neg_2 =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = ap neg 2"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "10100010"

prop_modulate_26 =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = 26"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "0111000011010"

prop_modulate_255 =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = 255"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "0111011111111"

prop_modulate_nil =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = nil"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "00"

prop_modulate_cons =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = ap ap cons 1 2"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "110110000101100010"

prop_modulate_vec =
  withTests 1 . property $
    let env = parseAndEvalEnv "galaxy = ap ap cons 1 ap ap cons 2 nil"
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "1101100001110110001000"

prop_modulate_nested =
  withTests 1 . property $
    let env =
          parseAndEvalEnv $
            unlines
              [ "galaxy = ap ap cons 1 ap ap cons lst ap ap cons 4 nil",
                "lst = ap ap cons 2 ap ap cons 3 nil"
              ]
        ret = resolve env (TermRef "galaxy")
     in modulate env ret === "1101100001111101100010110110001100110110010000"

prop_demodulate_modulate_num =
  property $ do
    x <- forAll $ Gen.integral (Range.linear (-1000) 1000)
    let obj = AtomNum x
        ObjectAtom actual = demodulate (modulate [] (ObjectAtom obj))
    actual === obj

prop_demodulate_vec =
  withTests 1 . property $
    let demod = demodulate "1101100001110110001000"
        parsed = toHsVec [] demod (\(ObjectAtom (AtomNum d)) -> d)
     in parsed === [1, 2]

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
