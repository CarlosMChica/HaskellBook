module ChapterExercises.SemVer where

import           Control.Applicative
import           Control.Monad
import           Data.Monoid
import           Text.Trifecta

data NumberOrString = NOSS String | NOSI Integer deriving (Show, Eq, Ord)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq)

instance Ord SemVer where
  (SemVer major minor patch release _) `compare` (SemVer major' minor' patch' release' _) =
       (major `compare` major')
    <> (minor `compare` minor')
    <> (patch `compare` patch')
    <> (release `compare` release')
--  (SemVer major minor patch _ _) `compare` (SemVer major' minor' patch' _ _)
--    | major < major' || minor < minor' || patch < patch' = LT
--    | major == major' && minor == minor' && patch == patch' = EQ
--    | otherwise                                        = GT

skipSeparator :: Parser ()
skipSeparator = skipMany $ oneOf "."

parseNumberOrString :: Parser NumberOrString
parseNumberOrString = (NOSS <$> some letter) <|> (NOSI <$> decimal)

parseNumberOrStringList :: Parser [NumberOrString]
parseNumberOrStringList = some (parseNumberOrString <* skipSeparator)

parseRelease :: Parser Release
parseRelease = (char '-' *> parseNumberOrStringList) <|> return []

parseMetadata :: Parser Metadata
parseMetadata = (char '+' *> parseNumberOrStringList) <|> return []

parseVersionNumber :: Parser Integer
parseVersionNumber = decimal <* try skipSeparator

parseMajor :: Parser Major
parseMajor = parseVersionNumber

parseMinor:: Parser Minor
parseMinor = parseVersionNumber

parsePatch :: Parser Patch
parsePatch = parseVersionNumber

parseSemVer :: Parser SemVer
parseSemVer = liftM5 SemVer parseMajor parseMinor parsePatch parseRelease parseMetadata

main :: IO ()
main = do
  tryParsing "2.1.1"
  tryParsing "1.0.0-x.7.z.92"
  tryParsing "1.0.0+exp.sha.5114f85"
  tryParsing "1.0.0-beta+exp.sha.5114f85"
  print $ (SemVer 2 1 0 [] []) < (SemVer 2 1 1 [] [])
  print $ (SemVer 2 1 0 [] []) == (SemVer 2 1 0 [] [])
  print $ (SemVer 2 1 1 [] []) > (SemVer 2 1 0 [] [])
  print $ (SemVer 2 1 0 [NOSS "alpha"] []) < (SemVer 2 1 0 [NOSS "beta"]  [])
  print $ (SemVer 2 1 0 [NOSS "beta"]  []) > (SemVer 2 1 0 [NOSS "alpha"] [])
  print $ (SemVer 2 1 0 [NOSS "alpha"] []) == (SemVer 2 1 0 [NOSS "alpha"] [])
  print $ (SemVer 2 1 0 [NOSS "alpha", NOSI 1] []) < (SemVer 2 1 0 [NOSS "alpha", NOSI 2] [])
  print $ (SemVer 2 1 0 [NOSS "alpha", NOSI 1] []) == (SemVer 2 1 0 [NOSS "alpha", NOSI 1] [])
  print $ (SemVer 2 1 0 [NOSS "alpha", NOSI 3] []) > (SemVer 2 1 0 [NOSS "alpha", NOSI 1] [])

tryParsing :: String -> IO ()
tryParsing = print . parseString parseSemVer mempty
