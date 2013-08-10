-- | POS taggauksen alkeita
module Main where

import qualified Data.List as L
import qualified Data.Map  as M

type Token = String
type Tag = String

data TrainingInstance = TrainingInstance Token Tag
                        deriving Show

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

evalTagger :: (Token -> Maybe Tag) -> [TrainingInstance] -> (Int, Int, Int)
evalTagger tagger = L.foldl' eval (0, 0, 0)
  where
    eval (n, c, u) (TrainingInstance token correctTag) =
      case tagger token of
        Just tag -> if tag == correctTag
                      then (n+1, c+1, u)
                      else (n+1, c, u)
        Nothing  -> (n+1, c, u+1)
