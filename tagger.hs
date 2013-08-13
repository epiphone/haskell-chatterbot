-- | POS taggauksen alkeita
module Main where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set as S
import qualified Data.List.Zipper as Z
import Control.Monad (forM)

type Token = String
type Tag = String

data TrainingInstance = TrainingInstance Token Tag
                        deriving Show

data Replacement = Replacement Tag Tag
                   deriving (Eq, Ord, Show)

data TransformationRule =
      NextTagRule Replacement Tag -- NextTagRule (Replacement a b) c = replace a with b when next tag is c
    | PrevTagRule Replacement Tag
    | SurroundTagRule Replacement Tag Tag
      deriving (Eq, Ord, Show)


-- Tarkistetaan freqTaggerin tarkkuus
main :: IO ()
main = do
    putStrLn "Arvioidaan..."
    freqMap <- readFreqTagMap
    testTxt <- readFile "brown-pos-test.txt"
    let testTs = map toTrainingInstance $ words testTxt
    let (n, c, u) = evalTagger (freqTag freqMap) testTs
    putStrLn $ "yhteensä:       " ++ show n
    putStrLn $ "oikein:         " ++ show c
    putStrLn $ "ei tunnistettu: " ++ show u
    putStrLn $ show (100 * fromIntegral c / fromIntegral n) ++ " %"

-- Selvitetään kymmenen yleisintä transformaatiosääntöä
main' :: IO [()]
main' = do
  putStrLn "Selvitetään 10 yleisintä transformaatiosääntöä..."
  ls <- fmap initialLearningState readTrainingInstances
  let ruleMap = instRules ruleInstantiators ls
  let sorted = L.sortBy (\(_,a) (_,b) -> compare b a) $ M.toList ruleMap
  forM (take 10 sorted) $ \(ts, count) -> do
      putStrLn $ show count ++ "kpl: " ++ show ts



rsplit :: (Eq a) => a -> [a] -> ([a], [a])
rsplit sep xs = let (l, r, _) = foldr go ([], [], False) xs in (l, r)
  where
    go x (l, r, True)  = (x:l, r, True)
    go x (l, r, False) = if x == sep then (l, r, True) else (l, x:r, False)

toTrainingInstance :: String -> TrainingInstance
toTrainingInstance s = TrainingInstance token tag
  where (token, tag) = rsplit '/' s


tokenTagFreqs :: [TrainingInstance] -> M.Map Token (M.Map Tag Int)
tokenTagFreqs = L.foldl' go M.empty
  where
    go acc (TrainingInstance token tag) =
        M.insertWith (innerInsert tag) token (M.singleton tag 1) acc
    innerInsert tag _ = M.insertWith (+) tag 1

tokenMostFreqTag :: M.Map Token (M.Map Tag Int) -> M.Map Token Tag
tokenMostFreqTag = M.map (fst . M.foldlWithKey go ("", 0))
  where
    go acc@(_, maxFreq) k v = if v > maxFreq then (k, v) else acc


trainFreqTagger :: [TrainingInstance] -> M.Map Token Tag
trainFreqTagger = tokenMostFreqTag . tokenTagFreqs


readTrainingInstances :: IO [TrainingInstance]
readTrainingInstances =
    fmap (map toTrainingInstance . words) $ readFile "brown-pos-train.txt"

readFreqTagMap :: IO (M.Map Token Tag)
readFreqTagMap = fmap trainFreqTagger readTrainingInstances

freqTag :: M.Map Token Tag -> Token -> Maybe Tag
freqTag m t = M.lookup t m

backupTag :: (Token -> Maybe Tag) -> Tag -> Token -> Tag
backupTag tagger backup token = case tagger token of
                                  Nothing -> backup
                                  Just t  -> t

evalTagger :: (Token -> Maybe Tag) -> [TrainingInstance] -> (Int, Int, Int)
evalTagger tagger = L.foldl' eval (0, 0, 0)
  where
    eval (n, c, u) (TrainingInstance token correctTag) =
      case tagger token of
        Just tag -> if tag == correctTag
                      then (n+1, c+1, u)
                      else (n+1, c, u)
        Nothing  -> (n+1, c, u+1)


-- Sääntöhommat:

instNextTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instNextTagRule z = do
  (_, nextIncorrect) <- rightCursor z
  (correct, incorrect) <- Z.safeCursor z
  return $ NextTagRule (Replacement incorrect correct) nextIncorrect

instPrevTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instPrevTagRule z = do
  (_, prevIncorrect) <- leftCursor z
  (correct, incorrect) <- Z.safeCursor z
  return $ PrevTagRule (Replacement incorrect correct) prevIncorrect

instSurroundTagRule :: Z.Zipper (Tag, Tag) -> Maybe TransformationRule
instSurroundTagRule z = do
  (_, prevIncorrect) <- leftCursor z
  (_, nextIncorrect) <- rightCursor z
  (correct, incorrect) <- Z.safeCursor z
  return $ SurroundTagRule (Replacement incorrect correct)
           prevIncorrect nextIncorrect


-- Apufunktiot vasemmalle ja oikealle kursorille Maybe-monadissa:

rightCursor :: Z.Zipper a -> Maybe a
rightCursor = Z.safeCursor . Z.right

leftCursor :: Z.Zipper a -> Maybe a
leftCursor z = if Z.beginp z then Nothing else Just $ Z.cursor $ Z.left z


instRules' :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
              Z.Zipper (Tag, Tag) -> S.Set TransformationRule
instRules' fs = Z.foldlz' getRules S.empty
  where
    getRules acc zipper
      | correct == incorrect = acc -- taggeri oli oikeassa, ei poimita korvaussääntöä
      | otherwise = foldl (applyFunc zipper) acc fs
      where
        (correct, incorrect) = Z.cursor zipper
        applyFunc z acc f = case f z of
                              Nothing -> acc
                              Just r  -> S.insert r acc

instRules :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
              Z.Zipper (Tag, Tag) -> M.Map TransformationRule Int
instRules fs = Z.foldlz' getRules M.empty
  where
    getRules acc zipper
      | correct == incorrect = acc -- taggeri oli oikeassa, ei poimita korvaussääntöä
      | otherwise = foldl (applyFunc zipper) acc fs
      where
        (correct, incorrect) = Z.cursor zipper
        applyFunc z acc f = case f z of
                              Nothing -> acc
                              Just r  -> M.insertWith (+) r 1 acc

-- "sana/sanaluokka"-listasta Zipper, jossa alkioina (oikea sanaluokka, arvattu sanaluokka)
initialLearningState :: [TrainingInstance] -> Z.Zipper (Tag, Tag)
initialLearningState ts = Z.fromList $ map toTagPair ts
  where
    toTagPair (TrainingInstance token tag) = (tag, tagFunc token)
    tagFunc = backupTag (freqTag (trainFreqTagger ts)) "NN"



taggingState = Z.fromList $ zip ["AT","NN","TO"] ["AT","VB","TO"]
ruleInstantiators = [instNextTagRule, instPrevTagRule, instSurroundTagRule]
