{-
    Yksinkertaista englantia puhuva chatterbot.

    TIEA241 Automaatit ja kieliopit-kurssin harjoitustyö
    Aleksi Pekkala (aleksi.v.a.pekkala@student.jyu.fi)
    Kesä 2013
-}

module Main where

import           Parser             (parseSentence)
import           Patterns           (answer)
import           Tagger             (Tag, Token,
                                     TrainingInstance (TrainingInstance),
                                     readFreqTagMap, ruleTagSentence)
import           TagSimplifier      (simplifyTag)
import           Control.Monad      (forever)
import           Data.Char          (isAlphaNum, toLower)
import qualified Data.Map           as M
import           System.Environment (getArgs)

main :: IO a
main = do
  args <- getArgs
  let verbose = not (null args) && head args == "-v"
  putStrLn "Greetings puny human"
  freqMap <- readFreqTagMap
  forever (talk freqMap verbose)

talk :: M.Map Token Tag -> Bool -> IO ()
talk m v = do
  tagged <- tagInput m
  case parseSentence (map toTokenTuple tagged) of
    Nothing   -> do
                  putStrLn $ unwords (map show tagged)
                  putStrLn "I don't understand\n"
    Just tree -> if v then
                   (do
                     putStrLn $ unwords (map show tagged)
                     print tree
                     putStrLn $ answer tree ++ "\n")
                 else putStrLn $ answer tree ++ "\n"


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
