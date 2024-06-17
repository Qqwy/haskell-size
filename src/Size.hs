{-# LANGUAGE GHC2021 #-}
module Size
  (Size
  -- * Conversions
  , toInt
  , fromInt
  , toWord
  , fromWord
  , safeFromInteger
  , toNatural
  , safeFromNatural
  -- * Manually call checked math functions
  , checkedAdd 
  , checkedSub
  , checkedMul
  -- * Overflow/Underflow exception types
  , Size.Internal.Overflow
  , Size.Internal.Underflow
  )
  where

import Size.Internal 
