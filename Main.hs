{-
    Yksinkertaista englantia puhuva chatterbot.

    TIEA241 Automaatit ja kieliopit-kurssin harjoitustyö
    Aleksi Pekkala (aleksi.v.a.pekkala@student.jyu.fi)
    Kesä 2013
-}

module Main where

import Parser (parseSentence)
import Tagger (TrainingInstance(TrainingInstance), Token, Tag, readFreqTagMap, ruleTagSentence)
import TagSimplifier (simplifyTag)
import Patterns (answer)

import           Control.Monad    (forever)
import Data.Char (isAlphaNum, toLower)
import qualified Data.Map as M

main = do
  putStrLn "Greetings puny human"
  freqMap <- readFreqTagMap
  forever (talk freqMap)

talk :: M.Map Token Tag -> IO ()
talk m = do
  tagged <- tagInput m
  putStrLn $ unwords (map show tagged) -- TODO tulosta vain jos verbose
  case parseSentence (map toTokenTuple tagged) of
    Nothing   -> putStrLn "I don't understand"
    Just tree -> do
        print tree
        putStrLn $ answer tree


tagInput :: M.Map Token Tag -> IO [TrainingInstance]
tagInput m = do
  tokens <- fmap words getLine
  let tagged = ruleTagSentence m (map cleanToken tokens)
  return $ map simplifyInstance tagged

cleanToken :: Token -> Token
cleanToken = map toLower . filter (\c -> isAlphaNum c || (c `elem` "'-"))

simplifyInstance :: TrainingInstance -> TrainingInstance
simplifyInstance (TrainingInstance token tag) =
    TrainingInstance token (simplifyTag tag)

toTokenTuple :: TrainingInstance -> (Token, Tag)
toTokenTuple (TrainingInstance token tag) = (token, tag)
