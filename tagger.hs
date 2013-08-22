-- | POS taggauksen alkeita
module Main where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Set as S
import qualified Data.List.Zipper as Z
import Control.Monad (forM)
import Data.Maybe (fromMaybe)

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
main' :: IO ()
main' = do
    putStrLn "Arvioidaan..."
    freqMap <- readFreqTagMap
    testTxt <- readFile "brown-pos-test.txt"
    let testTs = map toTrainingInstance $ words testTxt
    let (n, c, u) = evalTagger (freqTag freqMap) testTs
    putStrLn $ "yhteensä:       " ++ show n
    putStrLn $ "oikein:         " ++ show c
    putStrLn $ "ei tunnistettu: " ++ show u
    putStrLn $ show (100 * fromIntegral c / fromIntegral n) ++ " %"

-- Selvitetään kymmenen parasta transformaatiosääntöä
main :: IO [()]
main = do
  putStrLn "Selvitetään 10 parasta transformaatiosääntöä..."
  ls <- fmap initialLearningState readTrainingInstances
  let rules = transformationRules ruleInstantiators ls
  forM (take 10 rules) $ \r ->
    print r



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
backupTag tagger backup token = fromMaybe backup (tagger token)

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


instRules :: [Z.Zipper (Tag, Tag) -> Maybe TransformationRule] ->
              Z.Zipper (Tag, Tag) -> M.Map TransformationRule Int
instRules fs = Z.foldlz' getRules M.empty
  where
    getRules acc zipper
      | correct == incorrect = acc -- taggeri oli oikeassa, ei poimita korvaussääntöä
      | otherwise = foldl (applyFunc zipper) acc fs
      where
        (correct, incorrect) = Z.cursor zipper
        applyFunc z acc' f = case f z of
                              Nothing -> acc'
                              Just r  -> M.insertWith (+) r 1 acc'

sortRules :: M.Map TransformationRule Int -> [(TransformationRule, Int)]
sortRules = L.sortBy (\(_,a) (_,b) -> compare b a) . M.toList

-- "sana/sanaluokka"-listasta Zipper, jossa alkioina (oikea sanaluokka, arvattu sanaluokka)
initialLearningState :: [TrainingInstance] -> Z.Zipper (Tag, Tag)
initialLearningState ts = Z.fromList $ map toTagPair ts
  where
    toTagPair (TrainingInstance token tag) = (tag, tagFunc token)
    tagFunc = backupTag (freqTag (trainFreqTagger ts)) "NN"


ruleInstantiators :: [Z.Zipper (Tag, Tag) -> Maybe TransformationRule]
ruleInstantiators = [instNextTagRule, instPrevTagRule, instSurroundTagRule]

-- soveltaa transformaatiosääntöä yksittäiseen tagipariin
ruleApplication :: TransformationRule -> Z.Zipper (Tag, Tag) -> Maybe Tag
ruleApplication (NextTagRule (Replacement old new) next) z = do
  (_, proposed) <- Z.safeCursor z
  (_, nextProposed) <- rightCursor z
  if nextProposed == next && proposed == old then Just new else Nothing

ruleApplication (PrevTagRule (Replacement old new) prev) z = do
  (_, proposed) <- Z.safeCursor z
  (_, prevProposed) <- leftCursor z
  if prevProposed == prev && proposed == old then Just new else Nothing

ruleApplication (SurroundTagRule (Replacement old new) prev next) z = do
  (_, proposed) <- Z.safeCursor z
  (_, nextProposed) <- rightCursor z
  (_, prevProposed) <- leftCursor z
  if proposed == old && prevProposed == prev &&
    nextProposed == next then Just new else Nothing

scoreRule :: TransformationRule -> Z.Zipper (Tag, Tag) -> Int
scoreRule r z = nCorrect - nIncorrect
  where (nCorrect, nIncorrect) = scoreRule_ r z

scoreRule_ :: TransformationRule -> Z.Zipper (Tag, Tag) -> (Int, Int)
scoreRule_ r = Z.foldlz' (checkRule r) (0, 0)
  where checkRule r' (corr, incorr) z =
          case ruleApplication r' z of
            Nothing  -> (corr, incorr)
            Just res -> if res == fst (Z.cursor z)
                          then (corr+1, incorr)
                          else (corr, incorr+1)

selectRule :: [(TransformationRule, Int)] -> Z.Zipper (Tag, Tag) ->
              (TransformationRule, Int)
selectRule ((rule, _):rs) z = selectRule_ z rs (rule, scoreRule rule z)

selectRule_ :: Z.Zipper (Tag, Tag) -> [(TransformationRule, Int)] ->
               (TransformationRule, Int) -> (TransformationRule, Int)
selectRule_ _ [] best = best
selectRule_ z ((rule, ruleN):rs) best@(bestRule, bestScore)
  | bestScore >= ruleN = best
  | bestScore >= score = selectRule_ z rs best
  | otherwise          = selectRule_ z rs (rule, score)
  where score = scoreRule rule z

-- soveltaa transformaatiosääntöä jokaiseen zipperin tagipariin
--updateState :: TransformationRule -> Z.Zipper (Tag, Tag) -> Z.Zipper (Tag, Tag)
--updateState rule z = Z.reversez $ Z.foldlz' (applyRule rule) Z.empty z
--  where applyRule r acc z = case ruleApplication r z of
--                              Just tag -> Z.insert (tag, tag) acc
--                              Nothing  -> Z.insert (Z.cursor z) acc

updateState :: TransformationRule -> Z.Zipper (Tag, Tag) ->
               Z.Zipper (Tag, Tag)
updateState r = Z.fromList . reverse . Z.foldlz' (update r) []
    where update r xs z =
              case ruleApplication r z of
                Just tag -> (correct, tag):xs
                Nothing  -> e:xs
              where e@(correct, _) =  Z.cursor z

transformationRules :: [(Z.Zipper (Tag, Tag) -> Maybe TransformationRule)] ->
                       Z.Zipper (Tag, Tag) -> [TransformationRule]
transformationRules [] _ = []
transformationRules fs z = bestRule : transformationRules fs nextState
  where (bestRule, _) = selectRule (sortRules $ instRules fs z) z
        nextState = updateState bestRule z

-- sääntöjen selvittäminen on tuhottoman hidasta, siksi nämä ovat tässä valmiina.
-- samat säännöt selviävät main-funktiota ajamalla.
tenBestRules :: [TransformationRule]
tenBestRules = [NextTagRule (Replacement "TO" "IN") "AT",
                PrevTagRule (Replacement "NN" "VB") "TO",
                NextTagRule (Replacement "TO" "IN") "NP",
                PrevTagRule (Replacement "VBN" "VBD") "PPS",
                PrevTagRule (Replacement "NN" "VB") "MD",
                NextTagRule (Replacement "TO" "IN") "PP$",
                PrevTagRule (Replacement "VBN" "VBD") "NP",
                PrevTagRule (Replacement "PPS" "PPO") "VB",
                NextTagRule (Replacement "TO" "IN") "JJ",
                NextTagRule (Replacement "TO" "IN") "NNS"]
