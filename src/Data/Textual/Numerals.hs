{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

-- | Parsers for numbers written in positional numeral systems.
module Data.Textual.Numerals
  (
  -- * Positional numeral systems
    PositionalSystem(..)
  , BitSystem(..)
  , Binary(..)
  , Octal(..)
  , Decimal(..)
  , Hexadecimal(..)
  , LowHex(..)
  , UpHex(..)
  -- * Single digits
  , digitIn
  , nzDigitIn
  , binDigit
  , nzBinDigit
  , octDigit
  , nzOctDigit
  , decDigit
  , nzDecDigit
  , hexDigit
  , nzHexDigit
  , lowHexDigit
  , nzLowHexDigit
  , upHexDigit
  , nzUpHexDigit
  -- * Numbers
  , nonNegative
  , nnCompact
  , nnUpTo
  , nncUpTo
  , nnBounded
  , nncBounded
  , nnBits
  , nncBits
  , nnBitsUpTo
  , nncBitsUpTo
  , nnbBits
  , nncbBits
  , nonPositive
  , npCompact
  , npUpTo
  , npcUpTo
  , npBounded
  , npcBounded
  , npBits
  , npcBits
  , npBitsUpTo
  , npcBitsUpTo
  , npbBits
  , npcbBits
  , Sign(..)
  , optMinus
  , optSign
  , number'
  , number
  , compact'
  , compact
  , numberUpTo'
  , numberUpTo
  , compactUpTo'
  , compactUpTo
  , bounded'
  , bounded
  , cBounded'
  , cBounded
  , bits'
  , bits
  , cBits'
  , cBits
  , bitsUpTo'
  , bitsUpTo
  , cBitsUpTo'
  , cBitsUpTo
  , bBits'
  , bBits
  , cbBits'
  , cbBits
  ) where

import Data.Typeable (Typeable)
import Data.Word (Word)
import Data.Bits (Bits(..))
import Control.Applicative
import Text.Printer.Numerals (
         PositionalSystem(..), BitSystem(..),
         Binary(..), Octal(..), Decimal(..), Hexadecimal(..),
         LowHex(..), UpHex(..))
import Text.Parser.Combinators (Parsing, (<?>))
import qualified Text.Parser.Combinators as PC
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC

-- | Parse a digit of the specified positional numeral system.
digitIn ∷ (PositionalSystem s, Num α, CharParsing μ) ⇒ s → μ α
digitIn s  =  unsafeFromDigitIn s <$> PC.satisfy (isDigitIn s)
          <?> systemName s ++ " digit"
{-# INLINE digitIn #-}

-- | Parse a non-zero digit of the specified positional numeral system.
nzDigitIn ∷ (PositionalSystem s, Num α, CharParsing μ) ⇒ s → μ α
nzDigitIn s  =  unsafeFromDigitIn s <$> PC.satisfy (isNzDigitIn s)
            <?> "non-zero " ++ systemName s ++ " digit"
{-# INLINE nzDigitIn #-}

-- | Parse a binary digit.
binDigit ∷ (Num α, CharParsing μ) ⇒ μ α
binDigit = digitIn Binary
{-# INLINE binDigit #-}

-- | Parse a non-zero binary digit (/'1'/).
nzBinDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzBinDigit = nzDigitIn Binary
{-# INLINE nzBinDigit #-}

-- | Parse a decimal digit.
decDigit ∷ (Num α, CharParsing μ) ⇒ μ α
decDigit = digitIn Decimal
{-# INLINE decDigit #-}

-- | Parse a non-zero decimal digit.
nzDecDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzDecDigit = nzDigitIn Decimal
{-# INLINE nzDecDigit #-}

-- | Parse an octal digit.
octDigit ∷ (Num α, CharParsing μ) ⇒ μ α
octDigit = digitIn Octal
{-# INLINE octDigit #-}

-- | Parse a non-zero octal digit.
nzOctDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzOctDigit = nzDigitIn Octal
{-# INLINE nzOctDigit #-}

-- | Parse a hexadecimal digit.
hexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
hexDigit = digitIn Hexadecimal
{-# INLINE hexDigit #-}

-- | Parse a non-zero hexadecimal digit.
nzHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzHexDigit = nzDigitIn Hexadecimal
{-# INLINE nzHexDigit #-}

-- | Parse a lower case hexadecimal digit.
lowHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
lowHexDigit = digitIn LowHex
{-# INLINE lowHexDigit #-}

-- | Parse a non-zero lower case hexadecimal digit.
nzLowHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzLowHexDigit = nzDigitIn LowHex
{-# INLINE nzLowHexDigit #-}

-- | Parse an upper case hexadecimal digit.
upHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
upHexDigit = digitIn UpHex
{-# INLINE upHexDigit #-}

-- | Parse a non-zero upper case hexadecimal digit.
nzUpHexDigit ∷ (Num α, CharParsing μ) ⇒ μ α
nzUpHexDigit = nzDigitIn UpHex
{-# INLINE nzUpHexDigit #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system.
nonNegative ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nonNegative s = digit >>= go <?> systemName s ++ " digits"
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix + d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s

-- | Parse a non-negative number written in the specified positional
--   numeral system. Leading zeroes are not allowed.
nnCompact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nnCompact s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go $ fromIntegral (r ∷ Word)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix + d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s

moreThan ∷ CharParsing μ ⇒ Int → μ α
moreThan n = PC.unexpected
           $ "more than " ++ show n ++
             case n of
               1 → " digit"
               _ → " digits"
{-# INLINE moreThan #-}

-- | Parse a non-negative number written in the specified positional
--   numeral system (up to /n/ digits).
nnUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
nnUpTo _ n | n <= 0 = empty
nnUpTo s n = digit >>= go (n - 1) <?> systemName s ++ " digits"
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix + d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-negative number written in the specified positional
--   numeral system (up to /n/ digits). Leading zeroes are not allowed.
nncUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
nncUpTo _ n | n <= 0 = empty
nncUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go (n - 1) $ fromIntegral (r ∷ Word)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix + d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-negative number written in the specified positional
--   numeral system, failing on overflow.
nnBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ) ⇒ s → μ α
nnBounded s = digit >>= go <?> systemName s ++ " digits"
  where (q, r) = quotRem maxBound radix
        go !n  = optional digit >>= \case
                   Just n1 → if n < q || (n == q && n1 <= r)
                             then go (n * radix + n1)
                             else fail "out of bounds"
                   Nothing → return n
        radix  = radixIn s
        digit  = digitIn s

-- | Parse a non-negative number written in the specified positional
--   numeral system, failing on overflow. Leading zeroes are not allowed.
nncBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
              Monad μ, CharParsing μ) ⇒ s → μ α
nncBounded s = (<?> systemName s ++ " digits") $ digit >>= \case
                 0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                       Just _  → PC.unexpected "leading zero"
                       Nothing → return 0
                 n → go n
  where (q, r) = quotRem maxBound radix
        go !n  = optional digit >>= \case
                   Just d  → if n < q || (n == q && d <= r)
                             then go (n * radix + d)
                             else fail "out of bounds"
                   Nothing → return n
        radix  = radixIn s
        digit  = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system.
nnBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
nnBits s = digit >>= go <?> systemName s ++ " digits"
  where go !r     = optional digit >>= \case
                      Just d  → go ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system. Leading zeroes are not allowed.
nncBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
nncBits s = (<?> systemName s ++ " digits") $ digit >>= \case
              0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → PC.unexpected "leading zero"
                    Nothing → return 0 
              r → go r
  where go !r     = optional digit >>= \case
                      Just d  → go ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system (up to /n/ digits).
nnBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
nnBitsUpTo _ n | n <= 0 = empty
nnBitsUpTo s n = digit >>= go (n - 1) <?> systemName s ++ " digits"
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d  → go (l - 1) ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system (up to /n/ digits). Leading zeroes are not
--   allowed.
nncBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
            ⇒ s → Int → μ α
nncBitsUpTo _ n | n <= 0 = empty
nncBitsUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                    0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                          Just _  → PC.unexpected "leading zero"
                          Nothing → return 0 
                    r → go (n - 1) $ fromIntegral (r ∷ Word)
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d  → go (l - 1) ((r `shiftL` digitBits) .|. d)
                      Nothing → return r
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system, failing on overflow.
nnbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ s → μ α
nnbBits s = digit >>= go <?> systemName s ++ " digits"
  where q = maxBound `shiftR` digitBits
        r = maxBound .&. digitMaskIn s
        go !n = optional digit >>= \case
                  Just d  → if n < q || (n == q && d <= r)
                            then go ((n `shiftL` digitBits) .|. d)
                            else fail "out of bounds"
                  Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-negative binary number written in the specified
--   positional numeral system, failing on overflow. Leading zeroes are not
--   allowed.
nncbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
            Monad μ, CharParsing μ)
         ⇒ s → μ α
nncbBits s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
               0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                     Just _  → PC.unexpected "leading zero"
                     Nothing → return 0
               n → go $ fromIntegral (n ∷ Word)
  where q = maxBound `shiftR` digitBits
        r = maxBound .&. digitMaskIn s
        go !n = optional digit >>= \case
                  Just d  → if n < q || (n == q && d <= r)
                            then go ((n `shiftL` digitBits) .|. d)
                            else fail "out of bounds"
                  Nothing → return n
        digitBits = digitBitsIn s
        digit     = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system. For example, parsing \"123\" as a decimal would produce
--   /-123/, not /123/.
nonPositive ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
nonPositive s = (<?> systemName s ++ " digits") $ do
                  r ← digitIn s
                  go $ fromIntegral $ negate (r ∷ Int)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix - d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system. Leading zeroes are not allowed.
npCompact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
npCompact s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go $ fromIntegral $ negate (r ∷ Int)
  where go !r = optional digit >>= \case
                  Just d  → go (r * radix - d)
                  Nothing → return r
        radix = radixIn s
        digit = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system (up to /n/ digits).
npUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
npUpTo _ n | n <= 0 = empty
npUpTo s n = (<?> systemName s ++ " digits") $ do
               r ← digitIn s
               go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix - d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system (up to /n/ digits). Leading zeroes are not allowed.
npcUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → Int → μ α
npcUpTo _ n | n <= 0 = empty
npcUpTo s n = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → PC.unexpected "leading zero"
                      Nothing → return 0
                r → go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r = optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → moreThan n
                    Nothing → return r
        go l !r = optional digit >>= \case
                    Just d  → go (l - 1) (r * radix - d)
                    Nothing → return r
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system, failing on overflow.
npBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ)
          ⇒ s → μ α
npBounded s = (<?> systemName s ++ " digits") $ do
                n ← digitIn s
                go $ fromIntegral $ negate (n ∷ Int)
  where (q, r1) = quotRem minBound radix
        !r      = negate r1
        go !n   = optional digit >>= \case
                    Just d  → if n > q || (n == q && d <= r)
                              then go (n * radix - d)
                              else fail "out of bounds"
                    Nothing → return n
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-positive number written in the specified positional
--   numeral system, failing on overflow. Leading zeroes are not allowed.
npcBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
              Monad μ, CharParsing μ)
           ⇒ s → μ α
npcBounded s = (<?> systemName s ++ " digits") $ digitIn s >>= \case
                 0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                       Just _  → PC.unexpected "leading zero"
                       Nothing → return 0
                 n → go $ fromIntegral $ negate (n ∷ Int)
  where (q, r1) = quotRem minBound radix
        !r      = negate r1
        go !n   = optional digit >>= \case
                    Just d  → if n > q || (n == q && d <= r)
                              then go (n * radix - d)
                              else fail "out of bounds"
                    Nothing → return n
        radix   = radixIn s
        digit   = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system.
npBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
npBits s = skipZeroes <?> systemName s ++ " digits"
  where skipZeroes  = optional zero >>= \case
                        Just _  → skipZeroes1
                        Nothing → do
                          r ← digit
                          go $ fromIntegral $ negate (r ∷ Int)
        skipZeroes1 = optional zero >>= \case
                        Just _  → skipZeroes1
                        Nothing → optional digit >>= \case
                          Just r  → go $ fromIntegral $ negate r
                          Nothing → return 0
        go !r       = optional digit >>= \case
                        Just d1 → go ((r `shiftL` digitBits) .|. d)
                          where !d = fromIntegral $ negate d1 .&. digitMask 
                        Nothing → return r
        digitBits   = digitBitsIn s
        digitMask   = digitMaskIn s
        zero        = PC.char $! intToDigitIn s 0
        digit       = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system (up to /n/ digits).
npBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
npBitsUpTo _ n | n <= 0 = empty
npBitsUpTo s n = skipZeroes <?> systemName s ++ " digits"
  where skipZeroes  = optional zero >>= \case
                        Just _  → skipZeroes1 (n - 1)
                        Nothing → do
                          r ← digit
                          go (n - 1) $ fromIntegral $ negate (r ∷ Int)
        skipZeroes1 0 = optional (PC.satisfy $ isDigitIn s) >>= \case
                          Just _  → moreThan n
                          Nothing → return 0
        skipZeroes1 l = optional zero >>= \case
                          Just _  → skipZeroes1 (l - 1)
                          Nothing → optional digit >>= \case
                            Just r  → go (l - 1) $ fromIntegral $ negate r
                            Nothing → return 0
        go 0 !r     = optional (PC.satisfy $ isDigitIn s) >>= \case
                        Just _  → moreThan n
                        Nothing → return r
        go l !r     = optional digit >>= \case
                        Just d1 → go (l - 1) ((r `shiftL` digitBits) .|. d)
                          where !d = fromIntegral $ negate d1 .&. digitMask 
                        Nothing → return r
        digitBits   = digitBitsIn s
        digitMask   = digitMaskIn s
        zero        = PC.char $! intToDigitIn s 0
        digit       = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system. Leading zeroes are not allowed.
npcBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
npcBits s = (<?> systemName s ++ " digits") $ digit >>= \case
              0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                    Just _  → PC.unexpected "leading zero"
                    Nothing → return 0
              r → go $ fromIntegral $ negate (r ∷ Int)
  where go !r     = optional digit >>= \case
                      Just d1 → go ((r `shiftL` digitBits) .|. d)
                        where !d = fromIntegral $ negate d1 .&. digitMask 
                      Nothing → return r
        digitBits = digitBitsIn s
        digitMask = digitMaskIn s
        digit     = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system (up to /n/ digits).
--   Leading zeroes are not allowed.
npcBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
            ⇒ s → Int → μ α
npcBitsUpTo _ n | n <= 0 = empty
npcBitsUpTo s n = (<?> systemName s ++ " digits") $ digit >>= \case
                    0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                          Just _  → PC.unexpected "leading zero"
                          Nothing → return 0
                    r → go (n - 1) $ fromIntegral $ negate (r ∷ Int)
  where go 0 !r   = optional (PC.satisfy $ isDigitIn s) >>= \case
                      Just _  → moreThan n
                      Nothing → return r
        go l !r   = optional digit >>= \case
                      Just d1 → go (l - 1) ((r `shiftL` digitBits) .|. d)
                        where !d = fromIntegral $ negate d1 .&. digitMask 
                      Nothing → return r
        digitBits = digitBitsIn s
        digitMask = digitMaskIn s
        digit     = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system, failing on overflow.
npbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ s → μ α
npbBits s = skipZeroes <?> systemName s ++ " digits"
  where q1 = minBound `shiftR` digitBits
        r  = minBound .&. digitMaskIn s
        q  = if r == 0 then q1 else q1 + 1
        skipZeroes  = optional zero >>= \case
                        Just _  → skipZeroes1
                        Nothing → do
                          n ← digit
                          go $ fromIntegral $ negate (n ∷ Int)
        skipZeroes1 = optional zero >>= \case
                        Just _  → skipZeroes1
                        Nothing → optional digit >>= \case
                          Just n  → go $ fromIntegral $ negate n
                          Nothing → return 0
        go !n       = optional digit >>= \case
                        Just d1 → if n > q || (n == q && d >= r)
                                  then go ((n `shiftL` digitBits) .|. d)
                                  else fail "out of bounds"
                          where !d = fromIntegral $ negate d1 .&. digitMask 
                        Nothing → return n
        digitBits   = digitBitsIn s
        digitMask   = digitMaskIn s
        zero        = PC.char $! intToDigitIn s 0
        digit       = digitIn s

-- | Parse a non-positive two\'s complement binary number written in
--   the specified positional numeral system, failing on overflow.
--   Leading zeroes are not allowed.
npcbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
            Monad μ, CharParsing μ)
        ⇒ s → μ α
npcbBits s = (<?> systemName s ++ " digits") $ digit >>= \case
               0 → optional (PC.satisfy $ isDigitIn s) >>= \case
                     Just _  → PC.unexpected "leading zero"
                     Nothing → return 0
               n → go $ fromIntegral $ negate (n ∷ Int)
  where q1 = minBound `shiftR` digitBits
        r  = minBound .&. digitMaskIn s
        q  = if r == 0 then q1 else q1 + 1
        go !n       = optional digit >>= \case
                        Just d1 → if n > q || (n == q && d >= r)
                                  then go ((n `shiftL` digitBits) .|. d)
                                  else fail "out of bounds"
                          where !d = fromIntegral $ negate d1 .&. digitMask 
                        Nothing → return n
        digitBits   = digitBitsIn s
        digitMask   = digitMaskIn s
        digit       = digitIn s

data Sign = NonNegative | NonPositive
            deriving (Typeable, Eq, Show, Read)

-- | Optional minus sign.
optMinus ∷ CharParsing μ ⇒ μ Sign
optMinus  =  (pure NonPositive <* PC.char '-') <|> pure NonNegative
         <?> "optional minus sign"
{-# INLINE optMinus #-}

-- | Optional minus or plus sign.
optSign ∷ CharParsing μ ⇒ μ Sign
optSign  =  (pure NonPositive <* PC.char '-')
        <|> (pure NonNegative <* PC.char '+')
        <|> pure NonNegative
        <?> "optional sign"
{-# INLINABLE optSign #-}

-- | Parse a number written in the specified positional numeral system.
--   The supplied parser is used to determine the sign of the number.
number' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
        ⇒ μ Sign → s → μ α
number' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nonNegative s
  NonPositive → nonPositive s
{-# INLINE number' #-}

-- | A shorthand for 'number\'' 'optMinus'.
number ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
number = number' optMinus
{-# INLINE number #-}

-- | Parse a number written in the specified positional numeral system.
--   The supplied parser is used to determine the sign of the number.
--   Leading zeroes are not allowed.
compact' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
         ⇒ μ Sign → s → μ α
compact' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnCompact s
  NonPositive → npCompact s
{-# INLINE compact' #-}

-- | A shorthand for 'compact\'' 'optMinus'.
compact ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ) ⇒ s → μ α
compact = compact' optMinus
{-# INLINE compact #-}

-- | Parse a number written in the specified positional numeral system
--   (up to /n/ digits). The supplied parser is used to determine the sign of
--   the number.
numberUpTo' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
            ⇒ μ Sign → s → Int → μ α
numberUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nnUpTo s n
  NonPositive → npUpTo s n
{-# INLINE numberUpTo' #-}

-- | A shorthand for 'numberUpTo\'' 'optMinus'.
numberUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
numberUpTo = numberUpTo' optMinus
{-# INLINE numberUpTo #-}

-- | Parse a number written in the specified positional numeral system
--   (up to /n/ digits). The supplied parser is used to determine the sign of
--   the number. Leading zeroes are not allowed.
compactUpTo' ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
             ⇒ μ Sign → s → Int → μ α
compactUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nncUpTo s n
  NonPositive → npcUpTo s n
{-# INLINE compactUpTo' #-}

-- | A shorthand for 'compactUpTo\'' 'optMinus'.
compactUpTo ∷ (PositionalSystem s, Num α, Monad μ, CharParsing μ)
           ⇒ s → Int → μ α
compactUpTo = compactUpTo' optMinus
{-# INLINE compactUpTo #-}

-- | Parse a number written in the specified positional numeral system,
--   failing on overflow. The supplied parser is used to determine the sign
--   of the number.
bounded' ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
            Monad μ, CharParsing μ)
         ⇒ μ Sign → s → μ α
bounded' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBounded s
  NonPositive → npBounded s
{-# INLINE bounded' #-}

-- | A shorthand for 'bounded\'' 'optMinus'.
bounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
           Monad μ, CharParsing μ) ⇒ s → μ α
bounded = bounded' optMinus
{-# INLINE bounded #-}

-- | Parse a number written in the specified positional numeral system,
--   failing on overflow. The supplied parser is used to determine the sign
--   of the number. Leading zeroes are not allowed.
cBounded' ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
             Monad μ, CharParsing μ)
          ⇒ μ Sign → s → μ α
cBounded' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBounded s
  NonPositive → npcBounded s
{-# INLINE cBounded' #-}

-- | A shorthand for 'cBounded\'' 'optMinus'.
cBounded ∷ (PositionalSystem s, Ord α, Bounded α, Integral α,
            Monad μ, CharParsing μ) ⇒ s → μ α
cBounded = cBounded' optMinus
{-# INLINE cBounded #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system. The supplied parser is used to determine
--   the sign of the number.
bits' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
      ⇒ μ Sign → s → μ α
bits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBits s
  NonPositive → npBits s
{-# INLINE bits' #-}

-- | A shorthand for 'bits\'' 'optMinus'.
bits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
bits = bits' optMinus
{-# INLINE bits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system. The supplied parser is used to determine
--   the sign of the number. Leading zeroes are not allowed.
cBits' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
       ⇒ μ Sign → s → μ α
cBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBits s
  NonPositive → npcBits s
{-# INLINE cBits' #-}

-- | A shorthand for 'cBits\'' 'optMinus'.
cBits ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ) ⇒ s → μ α
cBits = cBits' optMinus
{-# INLINE cBits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system (up to /n/ digits). The supplied parser is
--   used to determine the sign of the number.
bitsUpTo' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
          ⇒ μ Sign → s → Int → μ α
bitsUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nnBitsUpTo s n
  NonPositive → npBitsUpTo s n
{-# INLINE bitsUpTo' #-}

-- | A shorthand for 'bitsUpTo\'' 'optMinus'.
bitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
         ⇒ s → Int → μ α
bitsUpTo = bitsUpTo' optMinus
{-# INLINE bitsUpTo #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system (up to /n/ digits). The supplied parser is
--   used to determine the sign of the number. Leading zeroes are not
--   allowed.
cBitsUpTo' ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
           ⇒ μ Sign → s → Int → μ α
cBitsUpTo' neg s n = (<?> systemName s) $ neg >>= \case
  NonNegative → nncBitsUpTo s n
  NonPositive → npcBitsUpTo s n
{-# INLINE cBitsUpTo' #-}

-- | A shorthand for 'cBitsUpTo\'' 'optMinus'.
cBitsUpTo ∷ (BitSystem s, Num α, Bits α, Monad μ, CharParsing μ)
          ⇒ s → Int → μ α
cBitsUpTo = cBitsUpTo' optMinus
{-# INLINE cBitsUpTo #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system, failing on overflow. The supplied parser is
--   used to determine the sign of the number.
bBits' ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
          Monad μ, CharParsing μ)
       ⇒ μ Sign → s → μ α
bBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nnbBits s
  NonPositive → npbBits s
{-# INLINE bBits' #-}

-- | A shorthand for 'bBits\'' 'optMinus'.
bBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
         Monad μ, CharParsing μ)
      ⇒ s → μ α
bBits = bBits' optMinus
{-# INLINE bBits #-}

-- | Parse a (two\'s complement) binary number written in the specified
--   positional numeral system, failing on overflow. The supplied parser is
--   used to determine the sign of the number. Leading zeroes are not
--   allowed.
cbBits' ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
           Monad μ, CharParsing μ)
        ⇒ μ Sign → s → μ α
cbBits' neg s = (<?> systemName s) $ neg >>= \case
  NonNegative → nncbBits s
  NonPositive → npcbBits s
{-# INLINE cbBits' #-}

-- | A shorthand for 'cbBits\'' 'optMinus'.
cbBits ∷ (BitSystem s, Ord α, Bounded α, Num α, Bits α,
          Monad μ, CharParsing μ)
       ⇒ s → μ α
cbBits = cbBits' optMinus
{-# INLINE cbBits #-}

