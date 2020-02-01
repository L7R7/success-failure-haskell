module Chapter02 where

import           Data.Char
import           Data.List

isAnagram :: String -> String -> Bool
isAnagram word1 word2 = sort word1 == sort word2

isWord :: String -> Maybe String
isWord s =
  case null s of
    True -> Nothing
    False ->
      case all isAlpha s of
        False -> Nothing
        True  -> Just s

-- HLint suggests:
isWord2 :: String -> Maybe String
isWord2 s
  | null s = Nothing
  | all isAlpha s = Just s
  | otherwise = Nothing

checkAnagram :: String -> String -> String
checkAnagram word1 word2 =
  case (isWord word1) of
    Nothing -> "The first word is invalid."
    Just word1 ->
      case (isWord word2) of
        Nothing -> "The second word is invalid."
        Just word2 ->
          case (isAnagram word1 word2) of
            False -> "These words are not anagrams."
            True  -> "These words are anagrams."

mainAnagram :: IO ()
mainAnagram = do
  putStrLn "Please enter a word."
  word1 <- getLine
  putStrLn "Please enter a second word."
  word2 <- getLine
  print (checkAnagram word1 word2)

isPalindrome :: String -> Bool
isPalindrome word = simplified == reverse simplified
  where
    simplified = filter isAlphaNum $ map toLower word

isPalindrome2 :: String -> Bool
isPalindrome2 word =
  let simplified = filter isAlphaNum $ map toLower word
   in simplified == reverse simplified

checkPalindrome :: String -> String
checkPalindrome word =
  if null word
    then "Word may not be empty."
    else (case isWord word of
            Nothing -> "The word is invalid"
            Just w ->
              if isPalindrome w
                then "This word is a palindrome."
                else "This word is no palindrome.")

substituteChar :: Char -> Char
substituteChar c =
  case c of
    'e' -> '3'
    'a' -> '4'
    _   -> c

translateWord :: String -> String
translateWord = map substituteChar
