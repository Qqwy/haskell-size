{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
module Size where
-- TODO inline all the tiny things

import GHC.Exts                (timesInt2#, Int(I#))
import GHC.Natural (Natural) -- TODO: public module?
import Control.Exception as Exception

-- | Represents the size of, or index/offset into, a datastructure.
--
-- Sizes are always unsigned (they cannot be negative).
--
-- The maximum bound of a `Size` is the same as the maximum bound of `Int`.
-- The minimum bound of a `Size` obviously is `0`.
--
-- In practice, on modern GHCs, the maximum bound of a `Size` is 2^63-1 on 64-bit environments and 2^31-1 on 32-bit environments.
-- That is: `Size` uses the unsigned range of an `Int`.
-- This is done to ensure that the conversion of `Size` -> `Int` is always valid, unintentional overflow will never occur.
--
-- In development and test environments (i.e. with optimizations disabled), overflow/underflow checking is done.
-- When built with optimizations enabled, these checks are removed; in this environment, `Size` behaves just like a normal `Int`.
newtype Size = Size Int
  deriving newtype Read
  deriving newtype Show
  deriving newtype Eq
  deriving newtype Ord
  deriving newtype Real

instance Num Size where
    (+) = checkedAdd
    (-) = checkedSub
    (*) = checkedMul
    -- (/) (Size x) (Size y) = (/) x y
    negate = underflowError
    abs = id
    signum _ = 1
    fromInteger = Size . fromInteger

toInt :: Size -> Int
toInt (Size x) = x

fromInt :: Int -> Maybe Size
fromInt x
  | x < 0 = Nothing 
  | otherwise = Just (Size x)

toWord :: Size -> Word
toWord (Size x) = fromIntegral x

fromWord :: Word -> Maybe Size
fromWord = fromInt . fromIntegral

safeFromInteger :: Integer -> Maybe Size
safeFromInteger x
  | x < 0 = Nothing
  | x > (toInteger (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromInteger x))

safeFromNatural :: Natural -> Maybe Size 
safeFromNatural x
  | x > (fromIntegral (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromIntegral x))

checkedAdd :: Size -> Size -> Size
{-# INLINE checkedAdd #-}
checkedAdd (Size x) (Size y)
  | r < 0 = overflowError
  | otherwise = Size r
  where
    r = x + y

checkedSub :: Size -> Size -> Size
{-# INLINE checkedSub #-}
checkedSub (Size x) (Size y)
  | r < 0 = underflowError
  | otherwise = Size r
  where
    r = x - y


checkedMul :: Size -> Size -> Size
{-# INLINE checkedMul #-}
checkedMul (Size !_x@(I# x#)) (Size !_y@(I# y#)) = 
  case timesInt2# x# y# of
    (# 0#, _, result #) -> Size (I# result)
    _ -> overflowError

underflowError :: a
underflowError = Exception.throw Exception.Underflow
overflowError :: a
overflowError = Exception.throw Exception.Overflow
