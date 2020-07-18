{-# LANGUAGE BangPatterns #-}

import Control.Exception
import Data.ByteString.Lazy.UTF8 as BLU
import Data.List
import Eval
import Network.HTTP.Simple
import Parse
import System.Environment
import System.IO

main = do
  input <- readFile "galaxy.txt"
  -- input <- readFile "simple.txt"
  let parsed = parse input
      simple = simpleProgram parsed
  -- print simple
  let env = eval simple
      r = find (\d -> dLhs d == "galaxy") env
  hPutStrLn stderr $ "Result: " ++ show r

-- main :: IO ()
-- main = catch (
--     do
--         args <- getArgs
--         putStrLn ("ServerUrl: " ++ args!!0 ++ "; PlayerKey: " ++ args!!1)
--         request' <- parseRequest ("POST " ++ (args!!0))
--         let request = setRequestBodyLBS (BLU.fromString (args!!1)) request'
--         response <- httpLBS request
--         let statuscode = show (getResponseStatusCode response)
--         case statuscode of
--             "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
--             _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
--     ) handler
--     where
--         handler :: SomeException -> IO ()
--         handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
--
