module Main where

import           Data.Char

main :: IO ()
main = do
  putStrLn "Please enter a password"
  password <- getLine
  print (validatePassword password)

validatePassword :: String -> Maybe String
validatePassword password = cleanWhitespace password >>= requireAlphaNum

checkPasswordLength :: String -> Maybe String
checkPasswordLength password =
  if length password > 20 || length password < 10
    then Nothing
    else Just password

checkPasswordLength2 :: String -> Maybe String
checkPasswordLength2 password =
  if length password `elem` [10 .. 20]
    then Just password
    else Nothing

requireAlphaNum :: String -> Maybe String
requireAlphaNum xs =
  if all isAlphaNum xs
    then Just xs
    else Nothing

cleanWhitespace :: String -> Maybe String
cleanWhitespace "" = Nothing
cleanWhitespace str@(x:xs) =
  if isSpace x
    then cleanWhitespace xs
    else Just str
