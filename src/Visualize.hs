{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TupleSections #-}

module Visualize where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.ByteString.Lazy.UTF8 as BLU
import Data.List
import qualified Data.Map as M
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty ((<|), NonEmpty (..))
import Data.Word
import Debug.Trace
import Eval
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Interact
import Network.HTTP.Simple
import Parse
import System.Environment
import System.IO hiding (interact)
import Prelude hiding (interact)

visualize :: Env -> Object -> IO ()
visualize env protocol = do
  initial <- interact env protocol objectNil (mkCoord 0 0)
  interactIO
    (InWindow "galaxy" (200, 200) (10, 10))
    red
    (initial :| [], 1.0)
    draw
    handleEvent
    (\_ -> return ())
  where
    handleEvent event (states, scaled) = case event of
      EventKey (MouseButton LeftButton) Up _ (x, y) -> do
        let (x', y') = (round $ x / scaled, round $ y / scaled)
        print $ "entering: " ++ show (x', y')
        let (oldState, _) = NE.head states
        new' <- interact env protocol oldState (mkCoord x' y')
        return (new' <| states, scaled)
      EventKey (MouseButton RightButton) Up _ (x, y) -> do
        print $ "undo"
        case NE.nonEmpty $ NE.tail states of
          Nothing -> return (states, scaled)
          Just ne -> return (ne, scaled)
      EventKey (Char '+') Up _ _ -> do
        return (states, scaled * 1.5)
      EventKey (Char '-') Up _ _ -> do
        return (states, scaled * 0.5)
      _ -> return (states, scaled)
    draw (states, scaled) = do
      let (_, data') = NE.head states
          images = reverse $ toHsVec env data' (\(ObjectAtom (AtomPicture coords)) -> coords)
          colors = [0, 50, 100, 150, 200]
          images' = concatMap (\(xs, c) -> map (,c) xs) $ zip images colors
      return . scale scaled scaled $ drawPicture images'
    drawPicture :: [((Integer, Integer), Word8)] -> Picture
    drawPicture coords =
      let coordsM = M.fromList coords
          xMin = minimum $ map (fst . fst) coords
          xMax = maximum $ map (fst . fst) coords
          yMin = minimum $ map (snd . fst) coords
          yMax = maximum $ map (snd . fst) coords
          width = xMax - xMin + 1
          height = yMax - yMin + 1
          bs = BS.pack $ flip concatMap [-height.. height] $ \y ->
            flip concatMap [-width .. width] $ \x ->
              case (x, y) `M.lookup` coordsM of
                Just c -> [c, c, c, 255]
                Nothing -> [255, 255, 255, 0]
       in bitmapOfByteString
            (fromIntegral $ 2*width+1)
            (fromIntegral $ 2*height+1)
            (BitmapFormat BottomToTop PxRGBA)
            bs
            False
    mkCoord x y =
      resolve env $
        apply2
          env
          (TermObject (objectCons env))
          (TermObject . ObjectAtom $ AtomNum x)
          (TermObject . ObjectAtom $ AtomNum y)
