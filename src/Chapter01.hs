module Chapter01 where

function :: (Ord a, Num a) => a -> a -> a
function x y =
  if x > y
    then x + 10
    else y

function2 :: (Ord a, Num a) => a -> a -> a
function2 x y =
  case x > y of
    False -> y
    True  -> x + 10

absVal :: (Num a, Ord a) => a -> a
absVal x =
  case x < 0 of
    False -> x
    True  -> negate x

validateUsernamePassword :: String -> String -> String
validateUsernamePassword username password =
  case (null username, null password) of
    (True, True)   -> "Empty username and password"
    (False, True)  -> "Empty password"
    (True, False)  -> "Empty username"
    (False, False) -> "Okay"

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:xs) = Just xs

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x
