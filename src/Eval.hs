{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Eval where

import Data.Bits
import Data.Bool
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.List
import Data.Maybe
import Debug.Trace
import GHC.Stack
import Network.HTTP.Simple
import Parse
import Util
import Prelude hiding (interact)

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
  = AtomNum Integer
  | AtomPicture [(Integer, Integer)]
  deriving (Show, Eq)

data Object
  = ObjectAtom Atom
  | ObjectPartial String (Term -> Term)

instance Show Object where
  show (ObjectAtom atom) = "ObjectAtom " ++ show atom
  show (ObjectPartial str _) = "ObjectPartial(" ++ str ++ ")"

data Term
  = TermObject Object
  | TermRef String
  deriving (Show)

parseAndEval :: String -> String -> Object
parseAndEval program term =
  let env = parseAndEvalEnv program
   in resolve env (TermRef term)

parseAndEvalEnv :: String -> Env
parseAndEvalEnv = eval . simpleProgram . parse

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
            TermObject . ObjectPartial "b0" $ \x0 ->
              TermObject . ObjectPartial "b1" $ \x1 ->
                TermObject . ObjectPartial "b2" $ \x2 ->
                  apply env x0 (apply env x1 x2)
       in ret : stack
    go env stack TokenC =
      let ret =
            TermObject . ObjectPartial "c0" $ \x0 ->
              TermObject . ObjectPartial "c1" $ \x1 ->
                TermObject . ObjectPartial "c2" $ \x2 ->
                  apply env (apply env x0 x2) x1
       in ret : stack
    go env stack TokenS =
      let ret =
            TermObject . ObjectPartial "s0" $ \x0 ->
              TermObject . ObjectPartial "s1" $ \x1 ->
                TermObject . ObjectPartial "s2" $ \x2 ->
                  apply env (apply env x0 x2) (apply env x1 x2)
       in ret : stack
    go env stack TokenIsNil = TermObject (objectIsNil env) : stack
    go env stack TokenDraw = TermObject (objectDraw env) : stack
    go env stack TokenMultipleDraw = TermObject (objectMultipleDraw env) : stack
    go _ _ other = error $ "unknown token: " ++ show other

objectNil :: Object
objectNil = ObjectPartial "nil" $ \_ -> TermObject objectT

objectT :: Object
objectT = ObjectPartial "t" $ \o1 -> TermObject . ObjectPartial "t'" $ \_ -> o1

objectF :: Object
objectF = ObjectPartial "f" $ \_ -> TermObject . ObjectPartial "f'" $ \o2 -> o2

objectI :: Object
objectI = ObjectPartial "i" $ \i -> i

objectCons :: Env -> Object
objectCons env =
  ObjectPartial "cons1" $ \o1 ->
    TermObject . ObjectPartial "cons2" $ \o2 ->
      TermObject . ObjectPartial "cons3" $ \o3 ->
        apply env (apply env o3 o1) o2

objectDraw :: Env -> Object
objectDraw env =
  ObjectPartial "draw" $ \obj ->
    let ls = toHsVec env (resolve env obj) parseItem
        parseItem = toHsNumPair env
     in TermObject . ObjectAtom . AtomPicture $ ls

objectMultipleDraw :: HasCallStack => Env -> Object
objectMultipleDraw env =
  ObjectPartial "multipledraw" $ \obj ->
    let isNil = applyObj (objectIsNil env) obj
     in if toHsBool env (resolve env isNil)
          then TermObject objectNil
          else
            let fst = applyObj (objectDraw env) (applyObj (objectCar env) obj)
                rest = applyObj (objectMultipleDraw env) (applyObj (objectCdr env) obj)
             in apply2 env (TermObject $ objectCons env) fst rest

objectCar :: HasCallStack => Env -> Object
objectCar env = ObjectPartial "car" $ \o1 -> apply env o1 (TermObject objectT)

objectCdr :: HasCallStack => Env -> Object
objectCdr env = ObjectPartial "cdr" $ \o1 -> apply env o1 (TermObject objectF)

objectIsNil :: HasCallStack => Env -> Object
objectIsNil env =
  ObjectPartial "isnil0" $ \x0 ->
    apply env x0 $ TermObject . ObjectPartial "isnil1" $ \_ ->
      TermObject . ObjectPartial "isnil2" $ \_ ->
        TermObject objectF

liftToTerm :: (Term -> Term) -> Term
liftToTerm f = TermObject . ObjectPartial "liftToTerm" $ \p -> f p

liftToTerm2 :: (Term -> Term -> Term) -> Term
liftToTerm2 f =
  TermObject . ObjectPartial "liftToTerm2_1" $ \p1 ->
    TermObject . ObjectPartial "liftToTerm_2_2" $ \p2 ->
      f p1 p2

numOp :: Env -> (Integer -> Integer) -> Term
numOp env f = liftToTerm $ \t1 ->
  case resolve env t1 of
    ObjectAtom (AtomNum n1) -> TermObject $ ObjectAtom (AtomNum $ f n1)
    _ -> error $ "weird args: " ++ show t1

numOp2 :: Env -> (Integer -> Integer -> Integer) -> Term
numOp2 env f = liftToTerm2 $ \t1 t2 ->
  case (resolve env t1, resolve env t2) of
    (ObjectAtom (AtomNum n1), ObjectAtom (AtomNum n2)) ->
      TermObject $ ObjectAtom (AtomNum $ f n1 n2)
    _ -> error $ "weird args: " ++ show (t1, t2)

numPred2 :: Env -> (Integer -> Integer -> Bool) -> Term
numPred2 env f = liftToTerm2 $ \t1 t2 ->
  case (resolve env t1, resolve env t2) of
    (ObjectAtom (AtomNum n1), ObjectAtom (AtomNum n2)) ->
      TermObject $ if f n1 n2 then objectT else objectF
    _ -> error $ "weird args: " ++ show (t1, t2)

toHsVec :: Env -> Object -> (Object -> a) -> [a]
toHsVec env vec conv =
  let isEmpty = apply env (TermObject $ objectIsNil env) (TermObject vec)
   in if toHsBool env (resolve env isEmpty)
        then []
        else
          let head = applyObj (objectCar env) (TermObject vec)
              tail = applyObj (objectCdr env) (TermObject vec)
           in conv (resolve env head) : toHsVec env (resolve env tail) conv

toHsBool :: Env -> Object -> Bool
toHsBool env obj =
  let b =
        apply2
          env
          (TermObject obj)
          (TermObject $ ObjectAtom $ AtomNum 1)
          (TermObject $ ObjectAtom $ AtomNum 0)
   in case b of
        TermObject (ObjectAtom (AtomNum d)) -> d == 1
        err -> error $ "expecting a boolean, but got: " ++ show err

toHsNumPair :: Env -> Object -> (Integer, Integer)
toHsNumPair env obj =
  let fst = applyObj (objectCar env) (TermObject obj)
      snd = applyObj (objectCdr env) (TermObject obj)
   in case (resolve env fst, resolve env snd) of
        (ObjectAtom (AtomNum a), ObjectAtom (AtomNum b)) -> (a, b)
        err -> error $ "expecting a pair, but got: " ++ show err

apply :: HasCallStack => Env -> Term -> Term -> Term
apply env f g = applyObj (resolve env f) g

applyObj :: HasCallStack => Object -> Term -> Term
applyObj f g = case f of
  ObjectPartial _ f' -> f' g
  err -> error $ "expecting a function, but got: " ++ show err

apply2 :: HasCallStack => Env -> Term -> Term -> Term -> Term
apply2 env f p1 p2 = apply env (apply env f p1) p2

resolve :: Env -> Term -> Object
resolve env obj@(TermObject k) = k
resolve env (TermRef d) =
  maybe
    (error $ "can not resolve: \"" ++ show d ++ "\"")
    (resolve env . dRhs)
    (find (\x -> dLhs x == d) env)

modulate :: Env -> Object -> String
modulate _ (ObjectAtom (AtomNum n)) =
  let sign = if n < 0 then "10" else "01"
      bits = numToBits (abs n)
      width = ceiling $ (fromIntegral $ length bits) / 4
      body = lpad (width * 4) '0' bits
   in sign ++ replicate width '1' ++ "0" ++ body
modulate env obj =
  let isNil = applyObj (objectIsNil env) (TermObject obj)
   in if toHsBool env (resolve env isNil)
        then "00"
        else -- assuming it's a cons now

          let lhs = resolve env $ applyObj (objectCar env) (TermObject obj)
              rhs = resolve env $ applyObj (objectCdr env) (TermObject obj)
           in "11" ++ modulate env lhs ++ modulate env rhs

demodulate :: String -> Object
demodulate all = case go all of
  (ret, "") -> ret
  (_, leftover) -> error $ "leftover on demodulate: " ++ show leftover
  where
    go :: String -> (Object, String)
    go str =
      case str of
        ('0' : '1' : n) -> let (n', r) = demodNum n in (ObjectAtom . AtomNum $ n', r)
        ('1' : '0' : n) -> let (n', r) = demodNum n in (ObjectAtom . AtomNum $ negate n', r)
        ('0' : '0' : rest) -> (objectNil, rest)
        ('1' : '1' : rest) ->
          let e1 = objectCons []
              (e2, r') = go rest
              (e3, r'') = go r'
           in ( resolve [] $ apply2 [] (TermObject e1) (TermObject e2) (TermObject e3),
                r''
              )
    demodNum :: String -> (Integer, String)
    demodNum str =
      let (widthPat, '0' : rest) = span (== '1') str
          width = length widthPat
          (bits, rest') = splitAt (width * 4) rest
       in (bitsToNum bits, rest')

drawPicture :: [((Int, Int), Char)] -> String
drawPicture [] = ""
drawPicture coords =
  let xMin = minimum $ map (fst . fst) coords
      xMax = maximum $ map (fst . fst) coords
      yMin = minimum $ map (snd . fst) coords
      yMax = maximum $ map (snd . fst) coords
      im = unlines $ flip map [yMin .. yMax] $ \y ->
        let line = flip map [xMin .. xMax] $ \x ->
              case (x, y) `lookup` coords of
                Just chr -> chr
                Nothing -> '.'
         in lpad 2 ' ' (show y) ++ " " ++ line
   in unlines ["   " ++ concatMap (show . abs) [xMin .. xMax], im]

drawPictures :: [[(Int, Int)]] -> String
drawPictures coords =
  drawPicture
    . concatMap (\(xs, chr) -> map (,chr) xs)
    . zip coords
    $ drawingChars
  where
    drawingChars = "abcdefghijklmnoprqrstuvwxyz"

--- INTERACT
f38 :: Env -> Object -> (Object, Object, Object) -> IO (Object, Object)
f38 env protocol (flag, newState, data') =
  case flag of
    ObjectAtom (AtomNum flag') ->
      if flag' == 0
        then return (newState, resolve env $ applyObj (objectMultipleDraw env) (TermObject data'))
        else interact env protocol newState =<< send env data'
    _ -> error "invalid flag"

interact :: Env -> Object -> Object -> Object -> IO (Object, Object)
interact env protocol state vector =
  let res = apply2 env (TermObject protocol) (TermObject state) (TermObject vector)
      [flag, newState, data'] = toHsVec env (resolve env res) id
   in f38 env protocol (flag, newState, data')

apikey = "xxx"

send :: Env -> Object -> IO Object
send env obj = do
  let modulated = modulate env obj
  error $ "transmitting: " ++ show modulated

  request' <- parseRequest ("POST https://icfpc2020-api.testkontur.ru/aliens/send")
  let request = setRequestBodyLBS (BLU.fromString modulated) request'
  response <- httpLBS request
  let statuscode = show (getResponseStatusCode response)
  case statuscode of
    "200" -> return . demodulate $ BLU.toString (getResponseBody response)
    _ ->
      error $
        "Unexpected server response:\nHTTP code: " ++ statuscode
          ++ "\nResponse body: "
          ++ BLU.toString (getResponseBody response)
