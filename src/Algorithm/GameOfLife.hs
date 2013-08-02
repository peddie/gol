{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns
  , QuasiQuotes
  , FlexibleContexts
  #-}

module Algorithm.GameOfLife (
                             step
                            ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..))

import Data.Array.Repa.Eval as RE()
import Data.Array.Repa.Stencil.Dim2 as RS
import Data.Array.Repa.Stencil
import qualified Quat()

{- The Repa GOL algorithm -}

countNeighbors :: Stencil R.DIM2 Int
countNeighbors = [stencil2| 1 1 1
                            1 0 1
                            1 1 1 |]

transition :: Int -> Int -> Int
transition 1 2 = 1
transition 1 3 = 1
transition 1 _ = 0
transition 0 3 = 1
transition 0 _ = 0
transition _ 0 = 0
transition _ 1 = 0
transition _ _ = 1

step :: (R.Source e Int, Monad m) =>
        Float -> R.Array e R.DIM2 Int -> m (R.Array R.U R.DIM2 Int)
step _ !world = R.computeP $ R.zipWith transition world neighbors
   where neighbors = mapStencil2 (BoundClamp) countNeighbors world
