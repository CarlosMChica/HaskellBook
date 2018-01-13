module UnitOfSuccess where

import           Control.Applicative
import           Text.Trifecta

parseIntegerEof :: Parser Integer
parseIntegerEof = integer <* eof
