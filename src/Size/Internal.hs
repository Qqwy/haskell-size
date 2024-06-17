{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}
-- | This module is to be considered internal,
-- and therefore might change even in minor PVP versions.
module Size.Internal
  (Size(Size)
  , toInt
  , fromIntSafe
  , fromIntChecked
  , toWord
  , fromWordSafe
  , fromWordChecked
  , toInteger
  , fromIntegerSafe
  , fromIntegerChecked
  , toNatural
  , fromNaturalSafe
  , fromNaturalChecked
  , addSafe
  , addChecked
  , subSafe
  , subChecked
  , mulSafe
  , mulChecked
  , Size.Internal.Prim.Overflow
  , Size.Internal.Prim.Underflow
  , Natural
  )
where

import Data.Function ((&))
import Numeric.Natural (Natural)
import Numeric.Natural qualified as Natural
import Data.Maybe (fromMaybe)
import Text.Read qualified as Read
import Control.Monad (guard)
import qualified Text.Printf
import Data.Ix (Ix)
import Data.Bits (Bits(..), FiniteBits(..))
import qualified Data.Bits
import GHC.Stack (HasCallStack)

import Size.Internal.Prim qualified

#if defined(FORCE_CHECKED_MATH) && defined(IGNORE_CHECKED_MATH)
#error The cabal library flags `force-checked-math` and `ignore-checked-math` cannot be enabled at the same time
#endif

-- | Represents the size of, or index/offset into, a datastructure.
--
-- Sizes are always unsigned (they cannot be negative).
--
-- The maximum bound of a `Size` is the same as the maximum bound of `Int` (which as per the Haskell report is at least 2^29-1).
-- The minimum bound of a `Size` obviously is `0`.
--
-- In practice, on modern GHC versions, the maximum bound of a `Size` is 2^63-1 on 64-bit environments and 2^31-1 on 32-bit environments.
-- That is: `Size` uses the unsigned range of an `Int`.
-- This is done to ensure that the conversion of `Size` -> `Int` is always valid, unintentional overflow will never occur.
--
-- == Conditional checks
--
-- In development and test environments (i.e. with optimizations disabled), overflow/underflow checking is done.
-- When built with optimizations enabled, these checks are removed; in this environment, `Size` behaves just like a normal `Int`.
-- To be precise:
--
-- - If the `+ignore-checked-math` library flag is set, overflow checks are always skipped.
-- - If the `+force-checked-math` library flag is set, overflow checks are always active.
-- - If no library flag is set, and the library is compiled without rewrite rules (such as using @-O0@), overflow checks are skipped.
-- - If no library flag is set, and the library is compiled with rewrite rules active (such as using @-fenable-rewrite-rules@ or @-O1@ or @-O2@ which include it), overflow checks are active.
newtype Size = 
    -- | Directly accessing the `Size` newtype constructor
    -- is considered unsafe, as it can be used to construct invalid (negative) `Size`s.
    Size Int
  deriving newtype Show -- ^ Just like any other integral number
  deriving newtype Eq
  deriving newtype Ord
  -- ^`Size` is just like `Int` w.r.t. to `Integral`. Division can never trigger overflow/underflow.
  deriving newtype Integral
  deriving newtype Real
  deriving newtype Ix
  deriving newtype Text.Printf.PrintfArg

-- | Just like any other unsigned integral number.
instance Read Size where
  readPrec     = do
    x <- Read.readPrec @Int
    guard (x >= 0)
    pure (Size x)

  readListPrec = Read.readListPrecDefault


-- | `Size` does /checked arithmetic/.
-- This means that any overflow resp. overflow that occurs during addition, subtraction or multiplication
-- is raised as an `Overflow` (resp. `Underflow`).
instance Num Size where
    (+) = add
    (-) = sub
    (*) = mul

    {-# INLINE negate #-}
    negate = Size.Internal.Prim.underflowError

    {-# INLINE abs #-}
    abs = id

    {-# INLINE signum #-}
    signum _ = 1

    {-# INLINE fromInteger #-}
    fromInteger x =
        x
        & fromIntegerSafe
        & fromMaybe raiseErr
        where
          {-# NOINLINE raiseErr #-}
          raiseErr 
            | x < 0 = Size.Internal.Prim.underflowError 
            | otherwise = Size.Internal.Prim.overflowError

add :: Size -> Size -> Size
{-# INLINE [1] add #-}
#ifdef IGNORE_CHECKED_MATH
add (Size x) (Size y) = Size (x + y)
#else
add = addChecked
#endif

sub :: Size -> Size -> Size
{-# INLINE [1] sub #-}
#ifdef IGNORE_CHECKED_MATH
sub (Size x) (Size y) = Size (x - y)
#else
sub = subChecked
#endif

mul :: Size -> Size -> Size
{-# INLINE [1] mul #-}
#ifdef IGNORE_CHECKED_MATH
mul (Size x) (Size y) = Size (x * y)
#else
mul = mulChecked
#endif


#if !defined(FORCE_CHECKED_MATH)
-- These rules will only trigger iff:
-- 1) FORCE_CHECKED_MATH is _not_ set
-- 2) The code is compiled with rewrite rules enabled, usually by using `-O1` or `-O2`.
{-# RULES
  "size/add"    forall x y. add x y = (\(Size a) (Size b) -> Size (a + b)) x y
#-}
{-# RULES
  "size/sub"    forall x y. sub x y = (\(Size a) (Size b) -> Size (a - b)) x y
#-}
{-# RULES
  "size/mul"    forall x y. mul x y = (\(Size a) (Size b) -> Size (a * b)) x y
#-}
#endif

-- | `Size` does /checked arithmetic/.
-- This means that any overflow resp. overflow that occurs during bit shifting or setting
-- is raised as an `Overflow` (resp. `Underflow`).
instance Bits Size where
  x .&. y = toEnum $ (fromEnum x) .&. fromEnum y
  x .|. y = toEnum $ (fromEnum x) .|. fromEnum y
  x `xor` y = toEnum $ fromEnum x `xor` fromEnum y
  complement = toEnum . complement  . fromEnum
  shift x a = toEnum $ shift (fromEnum x) a
  rotate x a = toEnum $ rotate (fromEnum x) a
  bit = Data.Bits.bitDefault
  isSigned = const False
  popCount = popCount . fromEnum
  testBit = testBit . fromEnum
  bitSize = finiteBitSize
  bitSizeMaybe = bitSizeMaybe . fromEnum

instance FiniteBits Size where
  finiteBitSize = finiteBitSize . fromEnum

-- | `Size` is nonnegative, so its `minBound` is 0. 
-- Its `maxBound` is the same as the maxBound of `Int`.
-- (usually 2^63 on 64-bit machines or 2^31 on 32-bit machines)
instance Bounded Size where
    {-# INLINE minBound #-}
    minBound = 0

    {-# INLINE maxBound #-}
    maxBound = Size (maxBound @Int)

-- | Conversions to and from `Int` using the `Enum` class will always do bounds checking.
--
-- On failure, an `Overflow` resp `Underflow` will be raised.
instance Enum Size where
    {-# INLINE succ #-}
    succ x
      | x /= maxBound = x + 1
      | otherwise = Size.Internal.Prim.overflowError

    {-# INLINE pred #-}
    pred x
      | x /= minBound = x - 1
      | otherwise = Size.Internal.Prim.underflowError

    {-# INLINE fromEnum #-}
    fromEnum = toInt

    {-# INLINE toEnum #-}
    toEnum x = toEnumImpl x

toEnumImpl :: Int -> Size
{-# INLINE [1] toEnumImpl #-}
#ifdef IGNORE_CHECKED_MATH
toEnumImpl x = Size x
#else
toEnumImpl = fromIntChecked
#endif

#if !defined(FORCE_CHECKED_MATH)
-- These rules will only trigger iff:
-- 1) FORCE_CHECKED_MATH is _not_ set
-- 2) The code is compiled with rewrite rules enabled, usually by using `-O1` or `-O2`.
{-# RULES
  "size/toEnum"    forall x. toEnumImpl x = Size x
#-}
#endif


-- | Converts a `Size` to an `Int`.
--
-- This will never fail, as the domain of `Size` is smaller than that of `Int`.
toInt :: Size -> Int
toInt (Size x) = x
{-# INLINE toInt #-}

-- | Attempts to convert an `Int` to a `Size`.
--
-- This will fail for negative `Int`s, in which case `Nothing` will be returned.
--
-- c.f. `fromIntChecked`.
fromIntSafe :: Int -> Maybe Size
{-# INLINE fromIntSafe #-}
fromIntSafe x
  | x < 0 = Nothing 
  | otherwise = Just (Size x)

-- | Attempts to convert an `Int` to a `Size`.
--
-- In case of failure (a negative `Int`), an `Underflow` exception will be raised.
--
-- c.f. `fromIntSafe`.
fromIntChecked :: HasCallStack => Int -> Size
fromIntChecked x =
  x 
  & fromIntSafe 
  & fromMaybe raiseErr
  where
    raiseErr = Size.Internal.Prim.underflowError

-- | Converts a `Size` to a `Word`.
-- This will never fail, as the domain of `Size` is smaller than that of `Word`.
toWord :: Size -> Word
{-# INLINE toWord #-}
toWord (Size x) = fromIntegral x

-- | Attempts to convert a `Word` to a `Size`.
--
-- This will fail for `Word`s larger than 'maxBound @Size' (usually 2^63 on 64-bit machines or 2^31 on 32-bit machines), 
-- in which case `Nothing` will be returned.
--
-- c.f. `fromWordChecked`.
fromWordSafe :: Word -> Maybe Size
{-# INLINE fromWordSafe #-}
fromWordSafe = fromIntSafe . fromIntegral

-- | Attempts to convert a `Word` to a `Size`.
--
-- In case of failure, an `Overflow` exception will be raised.
--
-- c.f. `fromWordSafe`.
fromWordChecked :: HasCallStack => Word -> Size
{-# INLINE fromWordChecked #-}
fromWordChecked = fromMaybe Size.Internal.Prim.overflowError . fromWordSafe

-- | Attempts to convert an `Integer` to a `Size`.
--
-- When negative or larger than `maxBound @Size`, `Nothing` will be returned.
fromIntegerSafe :: Integer -> Maybe Size
{-# INLINE fromIntegerSafe #-}
fromIntegerSafe x
  | x < 0 = Nothing
  | x > (toInteger (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromInteger x))

-- | Attempts to convert an `Integer` to a `Size`.
--
-- In case of failure, an `Overflow` or `Underflow` exception will be raised.
--
-- c.f. `fromIntegerSafe`.
fromIntegerChecked :: HasCallStack => Integer -> Size
{-# INLINE fromIntegerChecked #-}
fromIntegerChecked x =
  x
  & fromIntegerSafe
  & fromMaybe raiseErr
  where
    {-# NOINLINE raiseErr #-}
    raiseErr 
      | x < 0 = Size.Internal.Prim.underflowError 
      | otherwise = Size.Internal.Prim.overflowError


-- | Converts a `Size` into a `Natural`.
--
-- As the domain of `Natural` is larger than that of `Size`, this can never fail.
toNatural :: Size -> Natural
{-# INLINE toNatural #-}
toNatural x = x & toWord & Natural.wordToNatural

-- | Attempts to convert a `Natural` to a `Size`.
--
-- When larger than `maxBound @Size`, `Nothing` will be returned.
--
-- c.f. `fromNaturalChecked`.
fromNaturalSafe :: Natural -> Maybe Size 
{-# INLINE fromNaturalSafe #-}
fromNaturalSafe x
  | x > (fromIntegral (maxBound @Int)) = Nothing
  | otherwise = Just (Size (fromIntegral x))

-- | Attempts to convert a `Natural` to a `Size`.
--
-- In case of failure, an `Overflow` exception will be raised.
--
-- c.f. `fromNaturalSafe`.
fromNaturalChecked :: HasCallStack => Natural -> Size
fromNaturalChecked = fromMaybe Size.Internal.Prim.overflowError . fromNaturalSafe

-- | Adds two `Size`s, always checking for overflow (regardless of library or optimization flags).
-- An `Overflow` is raised if the result is too large to fit in a `Size`.
addChecked :: HasCallStack => Size -> Size -> Size
{-# INLINE addChecked #-}
addChecked (Size x) (Size y) = Size (Size.Internal.Prim.addChecked x y)

-- | Adds two `Size`s.
-- `Nothing` is returned if the result is too large to fit in a `Size`.
addSafe :: Size -> Size -> Maybe Size
addSafe (Size x) (Size y) = Size <$> Size.Internal.Prim.addSafe x y

-- | Subtracts two `Size`s, always checking for underflow (regardless of library or optimization flags).
-- An `Underflow` is raised if the result is negative.
subChecked :: HasCallStack => Size -> Size -> Size
{-# INLINE subChecked #-}
subChecked (Size x) (Size y) 
  = Size (fromIntegral (Size.Internal.Prim.subChecked (fromIntegral x)  (fromIntegral y)))

-- | Subtracts two `Size`s.
-- `Nothing` is returned if the result is negative.
subSafe :: Size -> Size -> Maybe Size
subSafe x y = do
  res <- Size.Internal.Prim.subSafe (toWord x) (toWord y)
  fromWordSafe res

-- | Multiplies two `Size`s, always checking for overflow (regardless of library or optimization flags).
-- An `Overflow` is raised if the result is too large to fit in a `Size`.
mulChecked :: HasCallStack => Size -> Size -> Size
{-# INLINE mulChecked #-}
mulChecked (Size x) (Size y) = Size (Size.Internal.Prim.mulChecked x y)

-- | Multiplies two `Size`s.
-- `Nothing` is returned if the result is too large to fit in a `Size`.
mulSafe :: Size -> Size -> Maybe Size
mulSafe (Size x) (Size y) = Size <$> Size.Internal.Prim.mulSafe x y
