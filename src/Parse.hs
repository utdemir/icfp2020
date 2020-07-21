module Parse where

import Data.Char (isDigit)
import Data.List.Split
import Text.Read

type Program = [Equation]

data Equation = Equation {eLhs :: [Token], eRhs :: [Token]}
  deriving (Show)

data Token
  = TokenNum Integer
  | TokenNil
  | TokenCons
  | TokenVec
  | TokenGalaxy
  | TokenAp
  | TokenNeg
  | TokenDec
  | TokenInc
  | TokenDraw
  | TokenMultipleDraw
  | TokenUnknown String
  | TokenC
  | TokenB
  | TokenS
  | TokenCar
  | TokenEq
  | TokenIsNil
  | TokenMul
  | TokenAdd
  | TokenLt
  | TokenDiv
  | TokenI
  | TokenT
  | TokenF
  | TokenCdr
  deriving (Show, Eq)

parse :: String -> Program
parse = map parseEquation . lines

parseEquation :: String -> Equation
parseEquation str =
  let [lhs, rhs] = splitOn " = " str
   in Equation (parseTokens lhs) (parseTokens rhs)

parseTokens :: String -> [Token]
parseTokens = map parseToken . splitOn " "

parseToken :: String -> Token
parseToken "ap" = TokenAp
parseToken "cons" = TokenCons
parseToken "vec" = TokenVec
parseToken n | Just d <- readMaybe n = TokenNum d
parseToken "nil" = TokenNil
parseToken "neg" = TokenNeg
parseToken "c" = TokenC
parseToken "b" = TokenB
parseToken "s" = TokenS
parseToken "isnil" = TokenIsNil
parseToken "car" = TokenCar
parseToken "eq" = TokenEq
parseToken "mul" = TokenMul
parseToken "add" = TokenAdd
parseToken "lt" = TokenLt
parseToken "div" = TokenDiv
parseToken "i" = TokenI
parseToken "t" = TokenT
parseToken "f" = TokenF
parseToken "cdr" = TokenCdr
parseToken "dec" = TokenDec
parseToken "inc" = TokenInc
parseToken "draw" = TokenDraw
parseToken "multipledraw" = TokenMultipleDraw
parseToken o = TokenUnknown o
