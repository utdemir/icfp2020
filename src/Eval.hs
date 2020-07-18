{-# LANGUAGE LambdaCase #-}

module Eval where

import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Stack
import Parse

data Definition a = Definition {dLhs :: String, dRhs :: a}
  deriving (Show)

type ParsedDefinition = Definition [Token]

type EvaluatedDefinition = Definition Term

type Env = [EvaluatedDefinition]

simpleProgram :: Program -> [ParsedDefinition]
simpleProgram = map $ \case
  Equation [TokenUnknown tok] xs -> Definition tok xs
  _ -> error "not a simple program"

data Atom
  = AtomNum Int
  | AtomPicture [(Int, Int)]
  deriving (Show, Eq)

data Object
  = ObjectAtom Atom
  | ObjectPartial (Term -> Term)

instance Show Object where
  show (ObjectAtom atom) = "ObjectAtom " ++ show atom
  show (ObjectPartial _) = "ObjectPartial"

data Term
  = TermObject Object
  | TermRef String
  deriving (Show)

parseAndEval :: String -> String -> Object
parseAndEval program term =
  let parsed = parse program
      simple = simpleProgram parsed
      result = resolve (eval simple) (TermRef term)
   in result

eval :: [ParsedDefinition] -> Env
eval parsed =
  let env = map (run env) parsed
   in env
  where
    run :: Env -> ParsedDefinition -> EvaluatedDefinition
    run env (Definition ref ts) =
      case foldl' (go env) [] (reverse ts) of
        [ret] -> Definition ref (TermObject $ resolve env ret)
        err ->
          error $
            "can not evaluate "
              ++ show ref
              ++ ": expecting singleton list, but got: "
              ++ show err
    go :: Env -> [Term] -> Token -> [Term]
    go env stack (TokenNum d) =
      TermObject (ObjectAtom (AtomNum d)) : stack
    go env stack (TokenUnknown d) =
      TermRef d : stack
    go env stack TokenAdd = numOp2 env (+) : stack
    go env stack TokenMul = numOp2 env (*) : stack
    go env stack TokenDiv = numOp2 env quot : stack
    go env stack TokenNeg = numOp env negate : stack
    go env stack TokenInc = numOp env succ : stack
    go env stack TokenDec = numOp env pred : stack
    go env stack TokenEq = numPred2 env (==) : stack
    go env stack TokenLt = numPred2 env (<) : stack
    go env stack TokenAp = case stack of
      fun : arg : xs -> apply env fun arg : xs
      _ -> error $ "TokenAp: weird stack: " ++ show stack
    go _ stack TokenNil = TermObject objectNil : stack
    go env stack TokenCons = TermObject (objectCons env) : stack
    go env stack TokenVec = go env stack TokenCons
    go env stack TokenCar = TermObject (objectCar env) : stack
    go env stack TokenCdr = TermObject (objectCdr env) : stack
    go _ stack TokenT = TermObject objectT : stack
    go _ stack TokenF = TermObject objectF : stack
    go _ stack TokenI = TermObject objectI : stack
    go env stack TokenB =
      let ret =
            TermObject . ObjectPartial $ \x0 ->
              TermObject . ObjectPartial $ \x1 ->
                TermObject . ObjectPartial $ \x2 ->
                  apply env x0 (apply env x1 x2)
       in ret : stack
    go env stack TokenC =
      let ret =
            TermObject . ObjectPartial $ \x0 ->
              TermObject . ObjectPartial $ \x1 ->
                TermObject . ObjectPartial $ \x2 ->
                  apply env (apply env x0 x2) x1
       in ret : stack
    go env stack TokenS =
      let ret =
            TermObject . ObjectPartial $ \x0 ->
              TermObject . ObjectPartial $ \x1 ->
                TermObject . ObjectPartial $ \x2 ->
                  apply env (apply env x0 x2) (apply env x1 x2)
       in ret : stack
    go env stack TokenIsNil =
      let ret =
            TermObject . ObjectPartial $ \x0 ->
              apply env x0 $ TermObject . ObjectPartial $ \_ ->
                TermObject . ObjectPartial $ \_ ->
                  TermObject objectF
       in ret : stack
    go _ _ other = error $ "unknown token: " ++ show other

objectNil :: Object
objectNil = ObjectPartial $ \_ -> TermObject objectT

objectT :: Object
objectT = ObjectPartial $ \o1 -> TermObject . ObjectPartial $ \_ -> o1

objectF :: Object
objectF = ObjectPartial $ \_ -> TermObject . ObjectPartial $ \o2 -> o2

objectI :: Object
objectI = ObjectPartial $ \i -> i

objectCons :: Env -> Object
objectCons env =
  ObjectPartial $ \o1 ->
    TermObject . ObjectPartial $ \o2 ->
      TermObject . ObjectPartial $ \o3 ->
        apply env (apply env o3 o1) o2

objectCar :: Env -> Object
objectCar env = ObjectPartial $ \o1 -> apply env o1 (TermObject objectT)

objectCdr :: Env -> Object
objectCdr env = ObjectPartial $ \o1 -> apply env o1 (TermObject objectF)

liftToTerm :: (Term -> Term) -> Term
liftToTerm f = TermObject . ObjectPartial $ \p -> f p

liftToTerm2 :: (Term -> Term -> Term) -> Term
liftToTerm2 f =
  TermObject . ObjectPartial $ \p1 ->
    TermObject . ObjectPartial $ \p2 ->
      f p1 p2

numOp :: Env -> (Int -> Int) -> Term
numOp env f = liftToTerm $ \t1 ->
  case resolve env t1 of
    ObjectAtom (AtomNum n1) -> TermObject $ ObjectAtom (AtomNum $ f n1)
    _ -> error $ "weird args: " ++ show t1

numOp2 :: Env -> (Int -> Int -> Int) -> Term
numOp2 env f = liftToTerm2 $ \t1 t2 ->
  case (resolve env t1, resolve env t2) of
    (ObjectAtom (AtomNum n1), ObjectAtom (AtomNum n2)) ->
      TermObject $ ObjectAtom (AtomNum $ f n1 n2)
    _ -> error $ "weird args: " ++ show (t1, t2)

numPred2 :: Env -> (Int -> Int -> Bool) -> Term
numPred2 env f = liftToTerm2 $ \t1 t2 ->
  case (resolve env t1, resolve env t2) of
    (ObjectAtom (AtomNum n1), ObjectAtom (AtomNum n2)) ->
      TermObject $ if f n1 n2 then objectT else objectF
    _ -> error $ "weird args: " ++ show (t1, t2)

apply :: HasCallStack => Env -> Term -> Term -> Term
apply env f g = case resolve env f of
  ObjectPartial f' -> f' g
  err -> error $ "expecting a function, but got: " ++ show err

resolve :: Env -> Term -> Object
resolve env obj@(TermObject k) = k
resolve env (TermRef d) =
  maybe
    (error $ "can not resolve: \"" ++ show d ++ "\"")
    (resolve env . dRhs)
    (find (\x -> dLhs x == d) env)
