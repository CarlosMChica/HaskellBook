module StringProcessing where

notThe :: String -> Maybe String
notThe xs
  | xs == "the" = Nothing
  | otherwise = Just xs

replaceThe :: String -> String
replaceThe = unwords . fmap (convert . notThe) . words
  where convert :: Maybe String -> String
        convert Nothing = "a"
        convert (Just x)  = x

isVowel :: Char -> Bool
isVowel x = x `elem` "aeiou"

theBeforeVowel :: String -> Char -> Bool
theBeforeVowel xs y = case notThe xs of
  Nothing -> isVowel y
  Just _  -> False

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel xs = go 0 (words xs)
  where go :: Integer -> [String] -> Integer
        go count (x1 : x2 : xs)
          | theBeforeVowel x1 (head x2) = go (count + 1) xs
          | otherwise                   = go count xs
        go count _                      = count

countVowels :: String -> Integer
countVowels = toInteger . length . filter isVowel
