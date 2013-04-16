{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Working with human-friendly (as opposed to the machine/compiler-friendly
--   'Show' and 'Read') textual representations.
module Data.Textual
  (
  -- * Printing
    Printable(..)
  , toString
  , toText
  , toLazyText
  , toAscii
  , toLazyAscii
  , toUtf8
  , toLazyUtf8
  -- * Parsing
  , Textual(..)
  -- ** Standard types proxies
  , hintType
  , hintType1
  , hintTypeArg
  , aUnit
  , aChar
  , anInteger
  , anInt
  , anInt8
  , anInt16
  , anInt32
  , anInt64
  , aWord
  , aWord8
  , aWord16
  , aWord32
  , aWord64
  , aFloat
  , aDouble
  -- ** Built-in parser
  , Parsed(..)
  , maybeParsed
  , builtInParser
  , parseString
  , parseStringAs
  , parseText
  , parseTextAs
  , parseLazyText
  , parseLazyTextAs
  , parseAscii
  , parseAsciiAs
  , parseLazyAscii
  , parseLazyAsciiAs
  , parseUtf8
  , parseUtf8As
  , parseLazyUtf8
  , parseLazyUtf8As
  , fromString
  , fromStringAs
  , fromText
  , fromTextAs
  , fromLazyText
  , fromLazyTextAs
  , fromAscii
  , fromAsciiAs
  , fromLazyAscii
  , fromLazyAsciiAs
  , fromUtf8
  , fromUtf8As
  , fromLazyUtf8
  , fromLazyUtf8As
  ) where

import Prelude hiding (print)
import Data.Typeable (Typeable)
import Data.Proxy (Proxy(..))
import Data.Foldable (Foldable)
import Data.Traversable (Traversable)
import Data.Int
import Data.Word
import Data.List (stripPrefix)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.Text as TS
import qualified Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Textual.Numerals
import Control.Applicative
import qualified Text.Printer as TP
import qualified Text.Printer.Numerals as TP
import Text.Parser.Combinators (Parsing, (<?>))
import qualified Text.Parser.Combinators as PC
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC

-- | The default printer for values of a type.
class Printable α where
  print ∷ TP.Printer p ⇒ α → p

instance Printable Char where
  print = TP.char
  {-# INLINE print #-}

instance Printable String where
  print = TP.string
  {-# INLINE print #-}

instance Printable TS.Text where
  print = TP.text
  {-# INLINE print #-}

instance Printable TL.Text where
  print = TP.lazyText
  {-# INLINE print #-}

instance Printable Integer where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Int where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Int8 where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Int16 where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Int32 where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Int64 where
  print = TP.decimal
  {-# INLINE print #-}

instance Printable Word where
  print = TP.nnDecimal
  {-# INLINE print #-}

instance Printable Word8 where
  print = TP.nnDecimal
  {-# INLINE print #-}

instance Printable Word16 where
  print = TP.nnDecimal
  {-# INLINE print #-}

instance Printable Word32 where
  print = TP.nnDecimal
  {-# INLINE print #-}

instance Printable Word64 where
  print = TP.nnDecimal
  {-# INLINE print #-}

instance Printable Float where
  print = TP.string7 . show
  {-# INLINE print #-}

instance Printable Double where
  print = TP.string7 . show
  {-# INLINE print #-}

-- | A shorthand for @'TP.buildString' . 'print'@.
toString ∷ Printable α ⇒ α → String
toString = TP.buildString . print
{-# INLINE toString #-}

-- | A shorthand for @'TP.buildText' . 'print'@.
toText ∷ Printable α ⇒ α → TS.Text
toText = TP.buildText . print
{-# INLINE toText #-}

-- | A shorthand for @'TP.buildLazyText' . 'print'@.
toLazyText ∷ Printable α ⇒ α → TL.Text
toLazyText = TP.buildLazyText . print
{-# INLINE toLazyText #-}

-- | A shorthand for @'TP.buildAscii' . 'print'@.
toAscii ∷ Printable α ⇒ α → BS.ByteString
toAscii = TP.buildAscii . print
{-# INLINE toAscii #-}

-- | A shorthand for @'TP.buildLazyAscii' . 'print'@.
toLazyAscii ∷ Printable α ⇒ α → BL.ByteString
toLazyAscii = TP.buildLazyAscii . print
{-# INLINE toLazyAscii #-}

-- | A shorthand for @'TP.buildUtf8' . 'print'@.
toUtf8 ∷ Printable α ⇒ α → BS.ByteString
toUtf8 = TP.buildUtf8 . print
{-# INLINE toUtf8 #-}

-- | A shorthand for @'TP.buildLazyUtf8' . 'print'@.
toLazyUtf8 ∷ Printable α ⇒ α → BL.ByteString
toLazyUtf8 = TP.buildLazyUtf8 . print
{-# INLINE toLazyUtf8 #-}

-- | The default parser for values of a type, must satisfy
--   @
--     'fromString' ('toString' /x/) = 'Just' /x/
--   @
class Printable α ⇒ Textual α where
  textual ∷ (Monad μ, CharParsing μ) ⇒ μ α

instance Textual Char where
  textual = PC.anyChar
  {-# INLINE textual #-}

instance Textual Integer where
  textual = number Decimal
  {-# INLINE textual #-}

instance Textual Int where
  textual = bounded Decimal
  {-# INLINE textual #-}

instance Textual Int8 where
  textual = bounded Decimal
  {-# INLINE textual #-}

instance Textual Int16 where
  textual = bounded Decimal
  {-# INLINE textual #-}

instance Textual Int32 where
  textual = bounded Decimal
  {-# INLINE textual #-}

instance Textual Int64 where
  textual = bounded Decimal
  {-# INLINE textual #-}

instance Textual Word where
  textual = nnBounded Decimal
  {-# INLINE textual #-}

instance Textual Word8 where
  textual = nnBounded Decimal
  {-# INLINE textual #-}

instance Textual Word16 where
  textual = nnBounded Decimal
  {-# INLINE textual #-}

instance Textual Word32 where
  textual = nnBounded Decimal
  {-# INLINE textual #-}

instance Textual Word64 where
  textual = nnBounded Decimal
  {-# INLINE textual #-}

-- | Hint the type system about the type of the first argument.
hintType ∷ α → Proxy α → α
hintType a _ = a
{-# INLINE hintType #-}

-- | Hint the type system about the type constructor.
hintType1 ∷ f α → Proxy f → f α
hintType1 f _ = f
{-# INLINE hintType1 #-}

-- | Hint the type system about the type argument. 
hintTypeArg ∷ f α → Proxy α → f α
hintTypeArg f _ = f
{-# INLINE hintTypeArg #-}

infixl 1 `hintType`, `hintType1`, `hintTypeArg`

-- | /()/ proxy value.
aUnit ∷ Proxy ()
aUnit = Proxy

-- | 'Char' proxy value.
aChar ∷ Proxy Char
aChar = Proxy

-- | 'Integer' proxy value.
anInteger ∷ Proxy Integer
anInteger = Proxy

-- | 'Int' proxy value.
anInt ∷ Proxy Int
anInt = Proxy

-- | 'Int8' proxy value.
anInt8 ∷ Proxy Int8
anInt8 = Proxy

-- | 'Int16' proxy value.
anInt16 ∷ Proxy Int16
anInt16 = Proxy

-- | 'Int32' proxy value.
anInt32 ∷ Proxy Int32
anInt32 = Proxy

-- | 'Int64' proxy value.
anInt64 ∷ Proxy Int64
anInt64 = Proxy

-- | 'Word' proxy value.
aWord ∷ Proxy Word
aWord = Proxy

-- | 'Word8' proxy value.
aWord8 ∷ Proxy Word8
aWord8 = Proxy

-- | 'Word16' proxy value.
aWord16 ∷ Proxy Word16
aWord16 = Proxy

-- | 'Word32' proxy value.
aWord32 ∷ Proxy Word32
aWord32 = Proxy

-- | 'Word64' proxy value.
aWord64 ∷ Proxy Word64
aWord64 = Proxy

-- | 'Float' proxy value.
aFloat ∷ Proxy Float
aFloat = Proxy

-- | 'Double' proxy value.
aDouble ∷ Proxy Double
aDouble = Proxy

-- | Parsing result.
data Parsed α = Parsed α
              | Malformed [String] String
              deriving (Typeable, Functor, Foldable, Traversable, Eq, Show)

instance Applicative Parsed where
  pure = Parsed
  {-# INLINE pure #-}
  Parsed f       <*> Parsed a       = Parsed (f a)
  Malformed ls e <*> _              = Malformed ls e
  _              <*> Malformed ls e = Malformed ls e
  {-# INLINABLE (<*>) #-}

instance Alternative Parsed where
  empty = Malformed [] "Alternative.empty"
  {-# INLINE empty #-}
  p@(Parsed _) <|> _ = p
  _            <|> p = p
  {-# INLINABLE (<|>) #-}

-- | Map 'Parsed' values to 'Just' and 'Malformed' to 'Nothing'.
maybeParsed ∷ Parsed α → Maybe α
maybeParsed (Parsed a) = Just a
maybeParsed _          = Nothing
{-# INLINABLE maybeParsed #-}

data Parser α =
  Parser { runParser ∷ ∀ r
                     . [String] → Word → String
                     → ([String] → Word → String → α → Parsed r)
                     → ([String] → Word → String → String → Parsed r)
                     → Parsed r }

instance Functor Parser where
  fmap f p = Parser $ \ls n i c h →
               runParser p ls n i (\ls' n' i' a → c ls' n' i' (f a)) h
  {-# INLINE fmap #-}

instance Applicative Parser where
  pure a = Parser $ \ls n i c _ → c ls n i a
  {-# INLINE pure #-}
  p <*> p' = Parser $ \ls n i c h →
               runParser p ls n i 
                 (\ls' n' i' f →
                    runParser p' ls' n' i'
                      (\ls'' n'' i'' a → c ls'' n'' i'' (f a)) h)
                 h
  {-# INLINE (<*>) #-}
  p *> p' = Parser $ \ls n i c h →
              runParser p ls n i (\ls' n' i' _ → runParser p' ls' n' i' c h) h
  {-# INLINE (*>) #-}
  p <* p' = Parser $ \ls n i c h →
              runParser p ls n i
                        (\ls' n' i' a →
                           runParser p' ls' n' i'
                                     (\ls'' n'' i'' _ → c ls'' n'' i'' a) h)
                        h
  {-# INLINE (<*) #-}

instance Alternative Parser where
  empty = PC.unexpected "Alternative.empty"
  {-# INLINE empty #-}
  p <|> p' = Parser $ \ls n i c h →
               runParser p ls n i c $ \ls' n' i' e → 
                 if n' == n then runParser p' ls n' i' c h
                            else h ls' n' i' e
  {-# INLINE (<|>) #-}

instance Parsing Parser where
  try p = Parser $ \ls n i c h →
            runParser p ls n i c (\ls' _ _ e → h ls' n i e)
  {-# INLINE try #-}
  p <?> l = Parser $ \ls n i c h →
              runParser p (l : ls) n i (\_ n' i' a → c ls n' i' a) h
  {-# INLINE (<?>) #-}
  skipMany p = Parser $ \ls n i c h →
                 runParser p ls n i
                   (\ls' n' i' _ → runParser (PC.skipMany p) ls' n' i' c h)
                   (\ls' n' i' _ → c ls' n' i' ())
  skipSome p = p *> PC.skipMany p
  {-# INLINE skipSome #-}
  unexpected e = Parser $ \ls n i _ h → h ls n i e
  {-# INLINE unexpected #-}
  eof = Parser $ \ls n i c h → case i of
                   [] → c ls n i ()
                   _  → h ls n i "Parsing.eof"
  {-# INLINABLE eof #-}
  notFollowedBy p = Parser $ \ls n i c h →
                      runParser p ls n i
                                (\_ _ _ _ → h ls n i "Parsing.notFollowedBy")
                                (\_ _ _ _ → c ls n i ())
  {-# INLINE notFollowedBy #-}

instance CharParsing Parser where
  satisfy f = Parser $ \ls n i c h → case i of
                         x : xs | f x → c ls n' xs x
                                          where !n' = n + 1
                         _ → h ls n i "CharParsing.satisfy"
  {-# INLINABLE satisfy #-}
  string s = Parser $ \ls n i c h → case stripPrefix s i of
                        Just i' → c ls n' i' s
                                    where !n' = n + fromIntegral (length s)
                        Nothing → h ls n i "CharParsing.string"
  {-# INLINABLE string #-}

instance Monad Parser where
  return = pure
  {-# INLINE return #-}
  p >>= f = Parser $ \ls n i c h →
              runParser p ls n i
                        (\ls' n' i' a → runParser (f a) ls' n' i' c h) h
  {-# INLINE (>>=) #-}
  (>>) = (*>)
  {-# INLINE (>>) #-}
  fail = PC.unexpected
  {-# INLINE fail #-}

parse ∷ Parser α → String → Parsed α
parse p i = runParser p [] 0 i (\_  _ _ a → Parsed a)
                               (\ls _ _ e → Malformed (reverse ls) e)
{-# INLINE parse #-}

-- | Use the built-in parser to parse a string. Intended for testing only.
builtInParser ∷ (∀ μ . (Monad μ, CharParsing μ) ⇒ μ α) → String → Parsed α
builtInParser p = parse p
{-# INLINE builtInParser #-}

-- | Parse a 'String' to extract the 'Textual' value.
parseString ∷ Textual α ⇒ String → Parsed α
parseString = parse $ textual <* PC.eof
{-# INLINE parseString #-}

-- | Provide a hint for the type system when using 'parseString'.
parseStringAs ∷ Textual α ⇒ Proxy α → String → Parsed α
parseStringAs _ = parseString
{-# INLINE parseStringAs #-}

-- | Parse a 'TS.Text' to extract the 'Textual' value.
parseText ∷ Textual α ⇒ TS.Text → Parsed α
parseText = parseString . TS.unpack
{-# INLINE parseText #-}

-- | Provide a hint for the type system when using 'parseText'.
parseTextAs ∷ Textual α ⇒ Proxy α → TS.Text → Parsed α
parseTextAs _ = parseText
{-# INLINE parseTextAs #-}

-- | Parse a lazy 'TL.Text' to extract the 'Textual' value.
parseLazyText ∷ Textual α ⇒ TL.Text → Parsed α
parseLazyText = parseString . TL.unpack
{-# INLINE parseLazyText #-}

-- | Provide a hint for the type system when using 'parseLazyText'.
parseLazyTextAs ∷ Textual α ⇒ Proxy α → TL.Text → Parsed α
parseLazyTextAs _ = parseLazyText
{-# INLINE parseLazyTextAs #-}

-- | Decode and parse an ASCII 'BS.ByteString' to extract the 'Textual' value.
parseAscii ∷ Textual α ⇒ BS.ByteString → Parsed α
parseAscii = parseString . BS8.unpack
{-# INLINE parseAscii #-}

-- | Provide a hint for the type system when using 'parseAscii'.
parseAsciiAs ∷ Textual α ⇒ Proxy α → BS.ByteString → Parsed α
parseAsciiAs _ = parseAscii
{-# INLINE parseAsciiAs #-}

-- | Decode and parse a lazy ASCII 'BL.ByteString' to extract
--   the 'Textual' value.
parseLazyAscii ∷ Textual α ⇒ BL.ByteString → Parsed α
parseLazyAscii = parseString . BL8.unpack
{-# INLINE parseLazyAscii #-}

-- | Provide a hint for the type system when using 'parseLazyAscii'.
parseLazyAsciiAs ∷ Textual α ⇒ BL.ByteString → Parsed α
parseLazyAsciiAs = parseString . BL8.unpack
{-# INLINE parseLazyAsciiAs #-}

-- | Decode and parse a UTF-8 'BS.ByteString' to extract the 'Textual' value.
parseUtf8 ∷ Textual α ⇒ BS.ByteString → Parsed α
parseUtf8 = parseLazyText . decodeUtf8 . BL.fromStrict
{-# INLINE parseUtf8 #-}

-- | Provide a hint for the type system when using 'parseUtf8'.
parseUtf8As ∷ Textual α ⇒ Proxy α → BS.ByteString → Parsed α
parseUtf8As _ = parseUtf8
{-# INLINE parseUtf8As #-}

-- | Decode and parse a lazy UTF-8 'BL.ByteString' to extract
--   the 'Textual' value.
parseLazyUtf8 ∷ Textual α ⇒ BL.ByteString → Parsed α
parseLazyUtf8 = parseLazyText . decodeUtf8
{-# INLINE parseLazyUtf8 #-}

-- | Provide a hint for the type system when using 'parseLazyUtf8'.
parseLazyUtf8As ∷ Textual α ⇒ Proxy α → BL.ByteString → Parsed α
parseLazyUtf8As _ = parseLazyUtf8
{-# INLINE parseLazyUtf8As #-}

-- | A shorthand for @'maybeParsed' . 'parseString'@
fromString ∷ Textual α ⇒ String → Maybe α
fromString = maybeParsed . parseString
{-# INLINE fromString #-}

-- | Provide a hint for the type system when using 'fromString'.
fromStringAs ∷ Textual α ⇒ Proxy α → String → Maybe α
fromStringAs _ = fromString
{-# INLINE fromStringAs #-}

-- | A shorthand for @'maybeParsed' . 'parseText'@
fromText ∷ Textual α ⇒ TS.Text → Maybe α
fromText = maybeParsed . parseText
{-# INLINE fromText #-}

-- | Provide a hint for the type system when using 'fromText'.
fromTextAs ∷ Textual α ⇒ Proxy α → TS.Text → Maybe α
fromTextAs _ = fromText
{-# INLINE fromTextAs #-}

-- | A shorthand for @'maybeParsed' . 'parseLazyText'@
fromLazyText ∷ Textual α ⇒ TL.Text → Maybe α
fromLazyText = maybeParsed . parseLazyText
{-# INLINE fromLazyText #-}

-- | Provide a hint for the type system when using 'fromLazyText'.
fromLazyTextAs ∷ Textual α ⇒ Proxy α → TL.Text → Maybe α
fromLazyTextAs _ = fromLazyText
{-# INLINE fromLazyTextAs #-}

-- | A shorthand for @'maybeParsed' . 'parseAscii'@
fromAscii ∷ Textual α ⇒ BS.ByteString → Maybe α
fromAscii = maybeParsed . parseAscii
{-# INLINE fromAscii #-}

-- | Provide a hint for the type system when using 'fromAscii'.
fromAsciiAs ∷ Textual α ⇒ Proxy α → BS.ByteString → Maybe α
fromAsciiAs _ = fromAscii
{-# INLINE fromAsciiAs #-}

-- | A shorthand for @'maybeParsed' . 'parseLazyAscii'@
fromLazyAscii ∷ Textual α ⇒ BL.ByteString → Maybe α
fromLazyAscii = maybeParsed . parseLazyAscii
{-# INLINE fromLazyAscii #-}

-- | Provide a hint for the type system when using 'fromLazyAscii'.
fromLazyAsciiAs ∷ Textual α ⇒ Proxy α → BL.ByteString → Maybe α
fromLazyAsciiAs _ = fromLazyAscii
{-# INLINE fromLazyAsciiAs #-}

-- | A shorthand for @'maybeParsed' . 'parseUtf8'@
fromUtf8 ∷ Textual α ⇒ BS.ByteString → Maybe α
fromUtf8 = maybeParsed . parseUtf8
{-# INLINE fromUtf8 #-}

-- | Provide a hint for the type system when using 'fromUtf8'.
fromUtf8As ∷ Textual α ⇒ Proxy α → BS.ByteString → Maybe α
fromUtf8As _ = fromUtf8
{-# INLINE fromUtf8As #-}

-- | A shorthand for @'maybeParsed' . 'parseLazyUtf8'@
fromLazyUtf8 ∷ Textual α ⇒ BL.ByteString → Maybe α
fromLazyUtf8 = maybeParsed . parseLazyUtf8
{-# INLINE fromLazyUtf8 #-}

-- | Provide a hint for the type system when using 'fromLazyUtf8'.
fromLazyUtf8As ∷ Textual α ⇒ Proxy α → BL.ByteString → Maybe α
fromLazyUtf8As _ = fromLazyUtf8
{-# INLINE fromLazyUtf8As #-}
