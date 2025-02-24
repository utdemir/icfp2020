{-# LANGUAGE BangPatterns #-}

import Control.Exception
import Control.Monad
import Data.ByteString.Lazy.UTF8 as BLU
import Data.List
import Eval
import Network.HTTP.Simple
import Parse
import System.Environment
import System.IO hiding (interact)
import Visualize
import Prelude hiding (interact)

main = do
  input <- readFile "galaxy.txt"
  let env = parseAndEvalEnv input
      galaxy = resolve env (TermRef "galaxy")

  visualize env galaxy

{-main = do
  input <- readFile "galaxy.txt"
  let env = parseAndEvalEnv input
      galaxy = resolve env (TermRef "galaxy")

  let mkCoord x y =
        resolve env $
          apply2
            env
            (TermObject (objectCons env))
            (TermObject . ObjectAtom $ AtomNum x)
            (TermObject . ObjectAtom $ AtomNum y)

  foldM
    ( \st click -> do
        (newState, data') <- interact env galaxy st click
        let images = toHsVec env data' (\(ObjectAtom (AtomPicture coords)) -> coords)
        putStrLn $ drawPictures images
        return newState
    )
    objectNil
    ( concat
        [ replicate 10 (mkCoord 0 0),
          replicate 10 (mkCoord (-2) (-6)),
          replicate 10 (mkCoord (-7) (-6))
        ]
    )-}

-- main :: IO ()
-- main =
--   catch
--     ( do
--         args <- getArgs
--         putStrLn ("ServerUrl: " ++ args !! 0 ++ "; PlayerKey: " ++ args !! 1)
--         request' <- parseRequest ("POST " ++ (args !! 0))
--         let request = setRequestBodyLBS (BLU.fromString (args !! 1)) request'
--         response <- httpLBS request
--         let statuscode = show (getResponseStatusCode response)
--         case statuscode of
--           "200" -> putStrLn ("Server response: " ++ BLU.toString (getResponseBody response))
--           _ -> putStrLn ("Unexpected server response:\nHTTP code: " ++ statuscode ++ "\nResponse body: " ++ BLU.toString (getResponseBody response))
--     )
--     handler
--   where
--     handler :: SomeException -> IO ()
--     handler ex = putStrLn $ "Unexpected server response:\n" ++ show ex
