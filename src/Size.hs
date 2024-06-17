{-# LANGUAGE GHC2021 #-}
module Size
  (
  -- * Main datatype
    Size
  -- * Conversions
  -- | `Size` supports all conversion functions of `Num` (`fromInteger`), `Integral` (`fromIntegral`) and `Enum` (`toEnum`, `fromEnum`).
  -- These will throw an `Overflow` or `Underflow` exception whenever the conversion would fail.
  -- Whether the overflow checks are active depends on the optimization flags and library flags.
  --
  -- For the checked variants below, the overflow checks are __always__ active.
  --
-- +--------------+--------------------+---------------------+---------------------+---------------------------------+
-- | Other type   | to                 | from (safe)         | from (always check) | from (check iff flags enabled ) |
-- +==============+====================+=====================+=====================+=================================+
-- | `Int`        | `toInt`, `fromEnum`| `fromIntSafe`       | `fromIntChecked`    | `toEnum`                        |
-- +--------------+--------------------+---------------------+---------------------+---------------------------------+
-- | `Word`       | `toWord`           | `fromWordSafe`      | `fromWordChecked`   | -                               |
-- +--------------+--------------------+---------------------+---------------------+---------------------------------+
-- | `Integer`    | `toInteger`        | `fromIntegerSafe`   | `fromIntegerChecked`| `fromInteger`                   |
-- +--------------+--------------------+---------------------+---------------------+---------------------------------+
-- | `Natural`    | `toNatural`        | `fromNaturalSafe`   | `fromNaturalChecked`| -                               |
-- +--------------+--------------------+---------------------+---------------------+---------------------------------+

  -- ** Int
  , toInt
  , fromIntSafe
  , fromIntChecked
  -- ** Word
  , toWord
  , fromWordSafe
  , fromWordChecked
  -- ** Integer
  , toInteger
  , fromIntegerSafe
  , fromIntegerChecked
  -- ** Natural
  , toNatural
  , fromNaturalSafe
  , fromNaturalChecked
  -- * Manually call checked math functions
  , addSafe
  , addChecked 
  , subSafe
  , subChecked
  , mulSafe
  , mulChecked
  -- * Overflow/Underflow exception types
  , Size.Internal.Overflow
  , Size.Internal.Underflow
  )
  where

import Size.Internal
import GHC.Natural (Natural)
