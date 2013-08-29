module Main where

import Parser (parseSentence)
import Tagger (TrainingInstance(TrainingInstance), Token, Tag, readFreqTagMap, ruleTagSentence)
import TagSimplifier (simplifyTag)

import Data.Char (isAlphaNum, toLower)
import qualified Data.Map as M

main :: IO ()
main = do
  putStrLn "Greetings puny human"
  freqMap <- readFreqTagMap
  tagged <- tagInput freqMap
  putStrLn $ unwords (map show tagged) -- TODO tulosta vain jos verbose
  case parseSentence tagged of
    Nothing   -> putStrLn "I don't understand"
    Just tree -> print tree


tagInput :: M.Map Token Tag -> IO [(Token, Tag)]
tagInput m = do
  tokens <- fmap words getLine
  let tagged = ruleTagSentence m (map cleanToken tokens)
  let simplified = map simplifyInstance tagged
  return simplified

cleanToken :: Token -> Token
cleanToken = map toLower . filter (\c -> isAlphaNum c || (c `elem` "'-"))

simplifyInstance :: TrainingInstance -> (Token, Tag)
simplifyInstance (TrainingInstance token tag) = (token, (simplifyTag tag))
