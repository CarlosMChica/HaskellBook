module MaybeLibrary where

isJust :: Maybe a -> Bool
isJust x = case x of
  Nothing   -> False
  _         -> True

isNothing :: Maybe a -> Bool
isNothing = not . isJust

mayybe :: b -> (a -> b) -> Maybe a -> b
mayybe acc _ Nothing  = acc
mayybe acc f (Just x) = f x

fromMaybe :: a -> Maybe a -> a
fromMaybe x = mayybe x id

listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

maybeToList :: Maybe a -> [a]
maybeToList = mayybe [] (: [])

catMaybes :: [Maybe a] -> [a]
catMaybes xs = xs >>= maybeToList

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = traverse id 

