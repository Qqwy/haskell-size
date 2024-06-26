{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
-- | This module is to be considered internal,
-- and therefore might change even in minor PVP versions.
module Size.Internal.Prim where

import Control.Exception qualified as Exception
import GHC.Exts (addIntC#, subWordC#, timesInt2#, Int(I#), Word(W#))
import GHC.Stack (HasCallStack)
import Data.Maybe (fromMaybe)

-- | Checked int addition.
-- Not general-purpose; expects input ints to be nonnegative.
-- (Will always raise an `Overflow`, even on `Underflow` 
-- if two negative numbers are added)
addChecked :: HasCallStack => Int -> Int -> Int
{-# INLINE addChecked #-}
addChecked x y = fromMaybe overflowError $ addSafe x y

addSafe :: Int -> Int -> Maybe Int
{-# INLINE addSafe #-}
addSafe !(I# x#) !(I# y#) =
    case addIntC# x# y# of
      (# r#, 0# #) -> Just (I# r#)
      _ -> Nothing

-- | Checked int subtraction.
-- Not general-purpose; expects input ints to be nonnegative.
-- (Will always raise an `Underflow`, even on `Overflow` 
-- if two negative numbers are subtracted)
subChecked :: HasCallStack => Word -> Word -> Word
{-# INLINE subChecked #-}
subChecked x y = fromMaybe underflowError $ subSafe x y

subSafe :: Word -> Word -> Maybe Word
{-# INLINE subSafe #-}
subSafe !(W# x#) !(W# y#) =
    case subWordC# x# y# of
      (# r#, 0# #) -> Just (W# r#)
      _ -> Nothing

-- | Checked int multiplication.
-- Not general-purpose; expects input ints to have same signs.
-- (Will always raise an `Overflow`, even on `Underflow`
-- if a negative and a positive number are multiplied)
mulChecked :: HasCallStack => Int -> Int -> Int
{-# INLINE mulChecked #-}
mulChecked x y = fromMaybe overflowError $ mulSafe x y

mulSafe :: Int -> Int -> Maybe Int
{-# INLINE mulSafe #-}
mulSafe !(I# x#) !(I# y#) = 
  case timesInt2# x# y# of
    (# 0#, _, result #) -> Just (I# result)
    _ -> Nothing

-- | Raise an `Underflow`.
underflowError :: HasCallStack => a
{-# NOINLINE underflowError #-}
underflowError = Exception.throw Underflow

-- | Raise an `Overflow`.
overflowError :: HasCallStack => a
{-# NOINLINE overflowError #-}
overflowError = Exception.throw Overflow

-- | Exception type indicating numerical overflow.
--
-- This is an 'exception subtype' of `ArithException`, so:
-- 
-- - If you want to handle `Overflow` specifically, catch the `Overflow` type.
-- - If you want to handle any kind of arithmetic exceptions, catch the `Exception.ArithException` type.
data Overflow = Overflow
  deriving (Eq, Ord, Show)

-- | Exception type indicating numerical underflow.
--
-- This is an 'exception subtype' of `ArithException`, so:
-- 
-- - If you want to handle `Underflow` specifically, catch the `Underflow` type.
-- - If you want to handle any kind of arithmetic exceptions, catch the `Exception.ArithException` type.
data Underflow = Underflow
  deriving (Eq, Ord, Show)

instance Exception.Exception Overflow where
  toException Overflow = Exception.toException Exception.Overflow
  fromException se = do 
    Exception.Overflow <- Exception.fromException se
    pure Overflow

instance Exception.Exception Underflow where
  toException Underflow = Exception.toException Exception.Underflow
  fromException se = do 
    Exception.Underflow <- Exception.fromException se
    pure Underflow
