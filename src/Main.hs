{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators
           , FlexibleContexts
           , BangPatterns
           #-}

module Main where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent (forkIO)

import qualified Data.Array.Repa as R
import Data.Array.Repa (Z(..), (:.)(..))

import Data.Array.Repa.Eval as RE()
import Data.Array.Repa.Algorithms.Randomish
import Data.Array.Repa.Unsafe (unsafeTraverse)

import Vis
import SpatialMath
import qualified Quat()

import System.Environment (getArgs)

import qualified Algorithm.GameOfLife as GOL
import qualified Algorithm.GameOfLife3D as GOL3

{- Asynchronous not-gloss rendering

   The worker thread should be able to carry on rendering new
   timesteps up to queueSize steps ahead of the renderer.  The MVars are
   used as a FIFO queue to transfer rendered GOL frames from the
   simulator to not-gloss's simulation loop.
-}

queueSize :: Int
queueSize = 100

timeStep :: Double
timeStep = 0.4

type State = VisObject Double
type Queue a = [MVar a]

type Grid e sh a = R.Array e sh a

produce :: Queue State -> Grid R.U sh a ->
           (Grid R.U sh a -> IO State) ->
           (Float -> Grid R.U sh a -> IO (Grid R.U sh a)) -> IO b
produce (m:ms) !state disp step = do state' <- step 0 state
                                     rendered <- disp state'
                                     f <- tryPutMVar m $! rendered
                                     case f of
                                       False -> putStrLn "producer blocked" >>=
                                                \_ -> putMVar m $! rendered
                                       True -> return ()
                                     produce ms state' disp step
produce [] _ _ _ = error "Bug in produce(): empty circular list!"

type World = (VisObject Double, [MVar (VisObject Double)])

consume :: Float -> World -> IO World
consume _ (_, (m:ms)) = do new' <- tryTakeMVar m
                           new <- case new' of
                                    Nothing -> putStrLn "consumer blocked" >>=
                                               \_ -> takeMVar m
                                    Just x -> return x
                           return $ (new, ms)
consume _ (_, []) = error "Bug in consume(): empty circular list!"

draw :: World -> IO (VisObject Double)
draw = return . fst

main :: IO ()
main = do
  n <- getArgs
  let side = case n of
               [] -> 50
               (x:_) -> read x
  ms' <- replicateM queueSize newEmptyMVar
  let ms = cycle ms'
  -- grid2 <- R.computeP $ R.map fromIntegral $ randomishIntArray(Z :. side :. side) 0 1 222
  grid3 <- R.computeP $ R.map fromIntegral $ randomishIntArray(Z :. side :. side :. side) 0 1 222
  _ <- forkIO $ produce ms grid3 render3 GOL3.step
  first <- takeMVar $ head ms
  simulateIO Nothing "simulation" timeStep (first, tail ms) draw consume

{- Rendering with not-gloss -}

edgeSize :: Double
edgeSize = 0.2
gridSize :: Double
gridSize = 0.22

identityQuat :: Quat Double
identityQuat = Quat 1 0 0 0

cube :: Flavour -> Color -> VisObject Double
cube = Box (edgeSize, edgeSize, edgeSize)

render :: (Integral a, R.Source b a, Monad m) =>
          R.Array b R.DIM2 a -> m (VisObject Double)
render arr = return $ VisObjects $ R.toList $ R.deepSeqArray bout bout
  where mkBox !r !c !v = Trans (Xyz (-r * edgeSize) (-c * edgeSize) 0) $
                         RotQuat identityQuat $ cube Solid (makeColor v v v 0.5)
        traverseBoxes !f !sh@(R.Z :. r :. c) = mkBox (fromIntegral r)
                                                     (fromIntegral c)
                                                     (fromIntegral $ f sh)
        bout = unsafeTraverse arr id traverseBoxes

render3 :: (Integral a, R.Source b a, Monad m) =>
          R.Array b R.DIM3 a -> m (VisObject Double)
render3 arr = return $ VisObjects $ R.toList $ R.deepSeqArray bout bout
  where mkBox !r !c !h !v = Trans (Xyz (-r * gridSize) (-c * gridSize) (h * gridSize)) $
                            RotQuat identityQuat $ cube Solid (makeColor 0.8 0.8 0.8 v)
        traverseBoxes !f !sh@(R.Z :. r :. c :. h) = mkBox (fromIntegral r)
                                                          (fromIntegral c)
                                                          (fromIntegral h)
                                                          (fromIntegral $ f sh)
        bout = unsafeTraverse arr id traverseBoxes
