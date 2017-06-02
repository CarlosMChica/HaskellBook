module ValidateTheWord where

newtype Word' = Word' String deriving (Eq, Show)

vowels = "aAeEiIoOuU"
consonants = "bBcCdDfFgGhHjJkKmMnNpPqQrRsStTvVwWxXyYzZ"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

isConsonant :: Char -> Bool
isConsonant x = x `elem` consonants

vowelsCount :: String -> Int
vowelsCount = count isVowel

consonantCount :: String -> Int
consonantCount = count isConsonant

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

isValidWord :: String -> Bool
isValidWord xs = vowelsCount xs < consonantCount xs

mkWord :: String -> Maybe Word'
mkWord xs
  | isValidWord xs = Just $ Word' xs
  | otherwise      = Nothing
