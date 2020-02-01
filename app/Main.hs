module Main where

import           Data.Char
import           Data.Either

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print (validatePassword password)

validatePassword :: String -> Either String String
validatePassword password = cleanWhitespace password >>= requireAlphaNum >>= checkPasswordLength

checkPasswordLength :: String -> Either String String
checkPasswordLength password =
  if length password > 20 || length password < 10
    then Left "Your password must be at least 10 characters long and may not be longer than 20 characters long"
    else Right password

checkPasswordLength2 :: String -> Maybe String
checkPasswordLength2 password =
  if length password `elem` [10 .. 20]
    then Just password
    else Nothing

requireAlphaNum :: String -> Either String String
requireAlphaNum xs =
  if all isAlphaNum xs
    then Right xs
    else Left "Your password cannot contain special characters."

cleanWhitespace :: String -> Either String String
cleanWhitespace "" = Left "Your password may not start with a whitespace character"
cleanWhitespace str@(x:xs) =
  if isSpace x
    then cleanWhitespace xs
    else Right str

bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe Nothing _  = Nothing
bindMaybe (Just a) f = f a

data StringOrValue a
  = Str String
  | Val a
  deriving (Show)

bindStringOrValue :: StringOrValue a -> (a -> StringOrValue b) -> StringOrValue b
bindStringOrValue (Str s) _ = Str s
bindStringOrValue (Val a) f = f a

printTestResult :: Either String () -> IO ()
printTestResult r =
  case r of
    Left err -> putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either String ()
eq n actual expected =
  if actual == expected
    then Right ()
    else Left (unlines ["Test " ++ show n, " Expected: " ++ show expected, " But got: " ++ show actual])

test :: IO ()
test =
  printTestResult $ do
    eq 0 (checkPasswordLength "") (Left "Your password must be at least 10 characters long and may not be longer than 20 characters long")
    eq
      1
      (checkPasswordLength "123456789012345678901")
      (Left "Your password must be at least 10 characters long and may not be longer than 20 characters long")
    eq 2 (checkPasswordLength "julielovesbooks") (Right "julielovesbooks")
    eq 3 (cleanWhitespace "") (Left "Your password may not start with a whitespace character")
    eq 4 (cleanWhitespace " foo") (Right "foo")
    eq 5 (cleanWhitespace "foo") (Right "foo")
    eq 6 (cleanWhitespace "     foo") (Right "foo")
    eq 7 (cleanWhitespace "     f o o") (Right "f o o")
    eq 8 (requireAlphaNum "abcd") (Right "abcd")
    eq 9 (requireAlphaNum "abcd;") (Left "Your password cannot contain special characters.")
    eq 10 (requireAlphaNum ";") (Left "Your password cannot contain special characters.")
