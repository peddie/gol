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

import Algorithm.GameOfLife (step)

{- Text output, synchronous (downsampled) rendering -}

-- type World = R.Array R.U R.DIM2 Int

-- chunk :: Int -> [a] -> [[a]]
-- chunk _ [] = []
-- chunk n xs = first : chunk n rest
--   where (first, rest) = splitAt n xs

-- show2D :: (R.Source b a, Show a, RE.Elt a) => Int -> R.Array b R.DIM2 a -> IO ()
-- show2D n = mapM_ (putStrLn . unwords . map show) . chunk n . R.toList

-- printCount :: Int
-- printCount = 1000

-- program :: Int -> Int -> World -> IO ()
-- program count sz world = do
--     input <- case count of
--                0 -> show2D sz world >> getLine
--                _ -> return "c"
--     let newcount = case count of
--                      0 -> printCount
--                      x -> x - 1
--     case input of
--         "q" -> return ()
--         _ -> tick 0 world >>= program newcount sz

-- main' :: IO ()
-- main' = do n <- getArgs
--            let side = read $ head n
--            program 0 side $ randomishIntArray (Z :. side :. side) 0 1 222

{- Synchronous not-gloss output -}

-- main'' :: IO ()
-- main'' = do
--   n <- getArgs
--   let side = read $ head n
--   let state0 = randomishIntArray(Z :. side :. side) 0 1 222
--   simulateIO Nothing "simulation" 0.05 state0 render tick

{- Asynchronous not-gloss rendering

   The worker thread should be able to carry on rendering new
   timesteps up to queueSize steps ahead of the renderer.  The MVars are
   used as a FIFO queue to transfer rendered GOL frames from the
   simulator to not-gloss's simulation loop.
-}

queueSize :: Int
queueSize = 100

timeStep :: Double
timeStep = 0.1

produce :: [MVar (VisObject Double)] -> R.Array R.U R.DIM2 Int -> IO b
produce (m:ms) !state = do state' <- step 0 state
                           rendered <- render state'
                           f <- tryPutMVar m $! rendered
                           case f of
                             False -> putStrLn "producer blocked" >>=
                                      \_ -> putMVar m $! rendered
                             True -> return ()
                           produce ms state'
produce [] _ = error "Bug in produce(): empty circular list!"

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
  let grid0 = randomishIntArray(Z :. side :. side) 0 1 222
  _ <- forkIO $ produce ms grid0
  first <- takeMVar $ head ms
  simulateIO Nothing "simulation" timeStep (first, tail ms) draw consume

{- Rendering with not-gloss -}

edgeSize :: Double
edgeSize = 0.2

identityQuat :: Quat Double
identityQuat = Quat 1 0 0 0

cube :: Flavour -> Color -> VisObject Double
cube = Box (edgeSize, edgeSize, edgeSize)

render :: (Integral a, R.Source b a, Monad m) =>
          R.Array b R.DIM2 a -> m (VisObject Double)
render arr = return $ VisObjects $ R.toList $ unsafeTraverse arr id traverseBoxes
  where mkBox !r !c !v = Trans (Xyz (-r * edgeSize) (-c * edgeSize) 0) $
                         RotQuat identityQuat $ cube Solid (makeColor v v v 0.5)
        traverseBoxes !f !sh@(R.Z :. r :. c) = mkBox (fromIntegral r)
                                                     (fromIntegral c)
                                                     (fromIntegral $ f sh)
