{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck ((==>))

import Data.Word (Word)
import Data.Textual
import Data.Textual.Numerals
import Control.Applicative
import Text.Printer (StringBuilder)
import qualified Text.Printer as TP
import qualified Text.Printer.Numerals as TP
import Text.Parser.Combinators as PC
import Text.Parser.Char (CharParsing)

main = defaultMain
  [ testProperty "Word" $ \w →
      fromString (toString w) == Just (w ∷ Word)
  , testProperty "nnBounded Hexadecimal" $ \w →
      parse (nnBounded Hexadecimal) (TP.nnUpHex w) == Parsed (w ∷ Word)
  , testProperty "npBounded Hexadecimal" $ \i →
      (i <= 0) ==>
        parse (npBounded Hexadecimal) (TP.npUpHex i) == Parsed (i ∷ Int)
  , testProperty "bounded Hexadecimal" $ \i →
      parse (bounded Hexadecimal) (TP.upHex i) == Parsed (i ∷ Int)
  ]

parse ∷ (∀ μ . (Monad μ, CharParsing μ) ⇒ μ α) → StringBuilder → Parsed α
parse p b = builtInParser (p <* PC.eof) (TP.buildString b)

