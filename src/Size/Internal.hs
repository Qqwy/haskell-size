{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
module Size.Internal
  (Size(Size)
  , toInt
  , fromInt
  , toWord
  , fromWord
  , safeFromInteger
  , toNatural
  , safeFromNatural
  , checkedAdd
  , checkedSub
  , checkedMul
  )
where

import Data.Function ((&))
import GHC.Natural (Natural) -- TODO: public module?
import GHC.Natural qualified as Natural
import GHC.Enum qualified as Enum
import Data.Maybe (fromMaybe)
import Text.Read qualified as Read
import Control.Monad (guard)

import Size.Prim qualified

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
newtype Size = 
    -- | Directly accessing the `Size` newtype constructor
    -- is considered unsafe, as it can be used to construct invalid (negative) `Size`s.
    Size Int
  deriving newtype Show
  deriving newtype Eq
  deriving newtype Ord
  -- `Size` is just like `Int` w.r.t. to `Integral`. Division can never trigger overflow/underflow.
  deriving newtype Integral
  deriving newtype Real

instance Read Size where
  readPrec     = do
    x <- Read.readPrec @Int
    guard (x >= 0)
    pure (Size x)

  readListPrec = Read.readListPrecDefault

-- #define DISABLE_OVERFLOW_CHECKS


-- | `Size` does _checked arithmetic_.
-- This means that any overflow/overflow that occurs during addition/subtraction/multiplication
-- is raised as an `Overflow :: ArithException` (resp. `Underflow :: ArithException`).
--
-- If the `DISABLE_OVERFLOW_CHECKS` flag is set, these checks are skipped.
instance Num Size where
    {-# INLINE (+) #-}
#ifdef DISABLE_OVERFLOW_CHECKS
    (Size x) + (Size y) = Size (x + y)
#else
    (+) = checkedAdd
#endif

    {-# INLINE (-) #-}
#ifdef DISABLE_OVERFLOW_CHECKS
    (Size x) - (Size y) = Size (x - y)
#else
    (-) = checkedSub
#endif

    {-# INLINE (*) #-}
#ifdef DISABLE_OVERFLOW_CHECKS
    (Size x) * (Size y) = Size (x * y)
#else
    (*) = checkedMul
#endif

    {-# INLINE negate #-}
    negate = Size.Prim.underflowError

    {-# INLINE abs #-}
    abs = id

    {-# INLINE signum #-}
    signum _ = 1

    {-# INLINE fromInteger #-}
    fromInteger = Size . fromInteger

-- | `Size` is nonnegative, so its `minBound` is 0. Its `maxBound` is the same as the maxBound of `Int`.
instance Bounded Size where
    {-# INLINE minBound #-}
    minBound = 0

    {-# INLINE maxBound #-}
    maxBound = Size (maxBound @Int)

-- | Conversions to and from Int using the `Enum` class will always do bounds checking.
instance Enum Size where
    {-# INLINE succ #-}
    succ x
      | x /= maxBound = x + 1
      | otherwise = Enum.succError "Size"

    {-# INLINE pred #-}
    pred x
      | x /= minBound = x - 1
      | otherwise = Enum.predError "Size"

    {-# INLINE fromEnum #-}
    fromEnum = toInt

    {-# INLINE toEnum #-}
    toEnum x = 
        x 
        & fromInt 
        & fromMaybe raiseErr
        where
          raiseErr = (Enum.toEnumError "Size" x (0, maxBound @Size))

-- | Converts a `Size` to an `Int`.
--
-- This will never fail, as the domain of `Size` is smaller than that of `Int`.
toInt :: Size -> Int
toInt (Size x) = x
{-# INLINE toInt #-}

-- | Attempts to convert an `Int` to a `Size`.
--
-- This will fail for negative `Int`s, in which case `Nothing` will be returned.
fromInt :: Int -> Maybe Size
{-# INLINE fromInt #-}
fromInt x
  | x < 0 = Nothing 
  | otherwise = Just (Size x)

-- | Converts a `Size` to a `Word`.
-- This will never fail, as the domain of `Size` is smaller than that of `Word`.
toWord :: Size -> Word
{-# INLINE toWord #-}
toWord (Size x) = fromIntegral x

-- | Attempts to convert an `Int` to a `Size`.
--
-- This will fail for `Word`s larger than `maxBound @Size` (usually 2^63 on 64-bit machines or 2^31 on 32-bit machines), 
-- in which case `Nothing` will be returned.
fromWord :: Word -> Maybe Size
{-# INLINE fromWord #-}
fromWord = fromInt . fromIntegral

-- | Attempts to convert an `Integer` to a `Size`.
--
-- When negative or larger than `maxBound @Size`, `Nothing` will be returned.
safeFromInteger :: Integer -> Maybe Size
{-# INLINE safeFromInteger #-}
safeFromInteger x
  | x < 0 = Nothing
  | x > (toInteger (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromInteger x))

toNatural :: Size -> Natural
{-# INLINE toNatural #-}
toNatural x = x & toWord & Natural.wordToNatural

-- | Attempts to convert an `Integer` to a `Size`.
--
-- When larger than `maxBound @Size`, `Nothing` will be returned.
safeFromNatural :: Natural -> Maybe Size 
{-# INLINE safeFromNatural #-}
safeFromNatural x
  | x > (fromIntegral (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromIntegral x))

checkedAdd :: Size -> Size -> Size
{-# INLINE checkedAdd #-}
checkedAdd (Size x) (Size y) = Size (Size.Prim.checkedAdd x y)

checkedSub :: Size -> Size -> Size
{-# INLINE checkedSub #-}
checkedSub (Size x) (Size y) = Size (Size.Prim.checkedSub x y)

checkedMul :: Size -> Size -> Size
{-# INLINE checkedMul #-}
checkedMul (Size x) (Size y) = Size (Size.Prim.checkedMul x y)