{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BangPatterns
  , QuasiQuotes
  , FlexibleContexts
  #-}

module Algorithm.GameOfLife3D (
                               step
                              ) where

import qualified Data.Array.Repa as R
import Data.Array.Repa ((:.)(..), Z(..))
import Data.Word (Word8())

{- The Repa GOL algorithm -}


{-# INLINE countNeighbors #-}
countNeighbors :: R.DIM3 -> (R.DIM3 -> Word8) -> R.DIM3 -> Word8
countNeighbors sz lk idx = sum $ map safeLookup $ variants idx
    where
      (Z :. x' :. y' :. z') = sz
      {-# INLINE this #-}
      this = lk idx
      {-# INLINE safeLookup #-}
      safeLookup i@(Z:.x:.y:.z) | (x<0) || (y<0) || (z<0) = 0
                                | (x>x'-1) || (y>y'-1) || (z>z'-1) = 0
                                | otherwise = lk i
      {-# INLINE variants #-}
      -- TODO(MP): Maybe this shouldn't consider "corner neighbors,"
      -- to make the rules a bit simpler?
      variants (Z:.x:.y:.z) = do xx <- [x-1,x,x+1]
                                 yy <- [y-1,y,y+1]
                                 zz <- [z-1,z,z+1]
                                 return (Z:.xx:.yy:.zz)

{-# INLINE transition #-}
transition :: Word8 -> Word8 -> Word8
-- TODO(MP): Tweak these a bit.
transition 1 2 = 1
transition 1 3 = 1
transition 1 _ = 0

transition 0 3 = 1
transition 0 2 = 1
transition 0 _ = 0

transition _ 0 = 0
transition _ 1 = 0
transition _ _ = 1

{-# INLINE step #-}
step :: (R.Source e Word8, Monad m) =>
        Float -> R.Array e R.DIM3 Word8 -> m (R.Array R.U R.DIM3 Word8)
step _ !world = R.computeP $ R.zipWith transition world neighbors
   where neighbors = R.traverse world id $ countNeighbors (R.extent world)
