{-# LANGUAGE ApplicativeDo              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Data.Char
import           Data.Either
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Validation

main :: IO ()
main = do
  putStr "Please enter a username.\n> "
  username <- Username <$> T.getLine
  putStr "Please enter a password.\n> "
  password <- Password <$> T.getLine
  print (makeUser username password)

validatePassword :: Password -> Validation Error Password
validatePassword (Password password) =
  case cleanWhitespace password of
    Failure err -> Failure err
    Success password2 -> requireAlphaNum password2 *> checkPasswordLength password2

validateUsername :: Username -> Validation Error Username
validateUsername (Username username) =
  case cleanWhitespace username of
    Failure err -> Failure err
    Success username2 -> requireAlphaNum username2 *> checkUsernameLength username2

checkPasswordLength :: T.Text -> Validation Error Password
checkPasswordLength password =
  if T.length password > 20 || T.length password < 10
    then Failure (Error ["Your password must be at least 10 characters long and may not be longer than 20 characters long"])
    else Success (Password password)

checkPasswordLength2 :: T.Text -> Maybe T.Text
checkPasswordLength2 password =
  if T.length password `elem` [10 .. 20]
    then Just password
    else Nothing

checkUsernameLength :: T.Text -> Validation Error Username
checkUsernameLength name =
  if T.length name > 15
    then Failure (Error ["Username cannot be longer than 15 characters."])
    else Success (Username name)

checkLength :: Int -> T.Text -> Validation Error T.Text
checkLength n s =
  if T.length s > n
    then Failure (Error [T.pack ("Input must not be longer than " ++ show n ++ " characters")])
    else Success s

requireAlphaNum :: T.Text -> Validation Error T.Text
requireAlphaNum xs =
  if T.all isAlphaNum xs
    then Success xs
    else Failure (Error ["Your password cannot contain special characters."])

cleanWhitespace :: T.Text -> Validation Error T.Text
cleanWhitespace input =
  if T.null stripped
    then Failure (Error ["Cannot be empty."])
    else Success stripped
  where
    stripped = T.strip input

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

printTestResult :: Either T.Text () -> IO ()
printTestResult r =
  case r of
    Left err -> T.putStrLn err
    Right () -> putStrLn "All tests passed."

eq :: (Eq a, Show a) => Int -> a -> a -> Either T.Text ()
eq n actual expected =
  if actual == expected
    then Right ()
    else Left (T.unlines (map T.pack ["Test " ++ show n, " Expected: " ++ show expected, " But got: " ++ show actual]))

test :: IO ()
test =
  printTestResult $ do
    eq
      0
      (checkPasswordLength "")
      (Failure (Error ["Your password must be at least 10 characters long and may not be longer than 20 characters long"]))
    eq
      1
      (checkPasswordLength "123456789012345678901")
      (Failure (Error ["Your password must be at least 10 characters long and may not be longer than 20 characters long"]))
    eq 2 (checkPasswordLength "julielovesbooks") (Success (Password "julielovesbooks"))
    eq 3 (cleanWhitespace "") (Failure (Error ["Cannot be empty."]))
    eq 4 (cleanWhitespace " foo") (Success "foo")
    eq 5 (cleanWhitespace "foo") (Success "foo")
    eq 6 (cleanWhitespace "     foo") (Success "foo")
    eq 7 (cleanWhitespace "     f o o") (Success "f o o")
    eq 8 (requireAlphaNum "abcd") (Success "abcd")
    eq 9 (requireAlphaNum "abcd;") (Failure (Error ["Your password cannot contain special characters."]))
    eq 10 (requireAlphaNum ";") (Failure (Error ["Your password cannot contain special characters."]))

newtype Password =
  Password T.Text
  deriving (Eq, Show)

newtype Username =
  Username T.Text
  deriving (Eq, Show)

newtype Error =
  Error [T.Text]
  deriving (Eq, Semigroup, Show)

data User =
  User Username Password
  deriving (Show)

makeUser :: Username -> Password -> Validation Error User
makeUser name password = do
  user <- validateUsername name
  pass <- validatePassword password
  pure $ User user pass

makeUserTmpPassword :: Username -> Validation Error User
makeUserTmpPassword name = User <$> validateUsername name <*> pure (Password "temporaryPassword")
-- 6.4: the Eq deriving is missing for the newtypes. Otherwise, the tests won't typecheck
