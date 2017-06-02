module EitherLibrary where

lefts' :: [Either a b] -> [a]
lefts' = foldr onlyLeft []
  where onlyLeft (Left x) xs = x : xs
        onlyLeft _        xs = xs

-- lefts' = fmap (\(Left x) -> x) . filter go
--   where go (Left _) = True
--         go _        = False

rights' :: [Either a b] -> [b]
rights' = foldr onlyRight []
  where onlyRight (Right x) xs = x : xs
        onlyRight _         xs = xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr partition ([], [])
  where partition (Left x)  (xs, ys) = (x : xs, ys)
        partition (Right y) (xs, ys) = (xs, y : ys)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just $ f x
eitherMaybe' f _         = Nothing

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ g (Right x) = g x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (Just . f)

