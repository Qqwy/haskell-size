{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | This module is to be considered internal,
-- and therefore might change even in minor PVP versions.
module Size.Prim where

import Control.Exception qualified as Exception
import GHC.Exts (addIntC#, subWordC#, timesInt2#, Int(I#), Word(W#))

-- | Checked int addition.
-- Not general-purpose; expects input ints to be nonnegative.
-- (Will always raise an `Overflow`, even on `Underflow` 
-- if two negative numbers are added)
checkedAdd :: Int -> Int -> Int
{-# INLINE checkedAdd #-}
checkedAdd !(I# x#) !(I# y#) =
    case addIntC# x# y# of
      (# r#, 0# #) -> I# r#
      _ -> overflowError

-- | Checked int subtraction.
-- Not general-purpose; expects input ints to be nonnegative.
-- (Will always raise an `Underflow`, even on `Overflow` 
-- if two negative numbers are subtracted)
checkedSub :: Word -> Word -> Word
{-# INLINE checkedSub #-}
checkedSub !(W# x#) !(W# y#) =
    case subWordC# x# y# of
      (# r#, 0# #) -> W# r#
      _ -> underflowError

-- | Checked int multiplication.
-- Not general-purpose; expects input ints to have same signs.
-- (Will always raise an `Overflow`, even on `Underflow`
-- if a negative and a positive number are multiplied)
checkedMul :: Int -> Int -> Int
{-# INLINE checkedMul #-}
checkedMul !(I# x#) !(I# y#) = 
  case timesInt2# x# y# of
    (# 0#, _, result #) -> I# result
    _ -> overflowError

-- | Raise an `Exception.Underflow` `ArithException.
underflowError :: a
{-# NOINLINE underflowError #-}
underflowError = Exception.throw Exception.Underflow

-- | Raise an `Exception.Overflow` `ArithException.
overflowError :: a
{-# NOINLINE overflowError #-}
overflowError = Exception.throw Exception.Overflow
