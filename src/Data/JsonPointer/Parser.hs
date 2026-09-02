-- |
-- Attoparsec parser.
module Data.JsonPointer.Parser
  ( parseJsonPointer
  , jsonPointerParser
  )
where

import Control.Applicative
import Data.Attoparsec.Text
import Data.Functor
import Data.Maybe ()
import Data.Semigroup ()
import Data.Text qualified as T

import Data.JsonPointer.Model

-- |
-- Parse JSON Pointer accepting both of the forms defined by the spec:
-- the plain one (@\/foo\/bar@) and the relative URI one (@#\/foo\/bar@).
--
-- No URL-decoding is performed on either form,
-- so the percent-escapes of a pointer taken out of a URI
-- have to be decoded by the caller beforehand.
parseJsonPointer :: T.Text -> Either T.Text JsonPointer
parseJsonPointer input =
  either (Left . T.pack) Right $ parseOnly (jsonPointerParser <* endOfInput) input

jsonPointerParser :: Parser JsonPointer
jsonPointerParser = optional (char '#') *> referenceTokens

referenceTokens :: Parser JsonPointer
referenceTokens = foldMany referenceToken

referenceToken :: Parser JsonPointer
referenceToken = char '/' *> (keyToModel <$> key)
  where
    key = T.pack <$> referenceTokenChars
    keyToModel !text = atIndexOrKey (textToIndexMaybe text) text
    textToIndexMaybe = either (const Nothing) Just . parseOnly parser
      where
        parser = decimal <* endOfInput

-- |
-- Reference token chars as per the definition in the JSON Pointer spec.
referenceTokenChars :: Parser [Char]
referenceTokenChars = many $ escapeSequence <|> unescapedChar
  where
    unescapedChar = satisfy $ \c -> c /= '/' && c /= '~'
    escapeSequence = char '~' *> (tilde <|> slash)
      where
        tilde = char '0' $> '~'
        slash = char '1' $> '/'

foldMany :: (Alternative m, Monoid a) => m a -> m a
foldMany consume = step <|> end
  where
    step = mappend <$> consume <*> foldMany consume
    end = pure mempty
