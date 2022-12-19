-- |
-- Attoparsec parser.
module JSONPointer.Parser
  ( Parser
  , parse
  , jsonPointer
  , jsonPointerUriFragment
  )
where

import Data.Attoparsec.Text hiding (parse)
import Data.Functor
import Data.Semigroup ()
import Data.Maybe ()
import Data.Text qualified as T
import Control.Applicative

import JSONPointer.Model

-- |
-- Uses the parser to parse the input text in whole.
parse :: Parser a -> T.Text -> Either T.Text a
parse parser input = either (Left . T.pack) Right $ parseOnly (parser <* endOfInput) input

-- |
-- JSON Pointer parser in the relative URI format.
jsonPointerUriFragment :: Parser JSONPointer
jsonPointerUriFragment = char '#' *> jsonPointer

-- |
-- JSON Pointer parser.
jsonPointer :: Parser JSONPointer
jsonPointer = foldMany referenceToken

referenceToken :: Parser JSONPointer
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
referenceTokenChars = many $ escapeSequence <|> notChar '/'
  where
    escapeSequence = char '~' *> (tilde <|> slash <|> other)
      where
        tilde = char '0' $> '~'
        slash = char '1' $> '/'
        other = fail "Illegal escape sequence"

foldMany :: (Alternative m, Monoid a) => m a -> m a
foldMany consume = step <|> end
  where
    step = mappend <$> consume <*> foldMany consume
    end = pure mempty
