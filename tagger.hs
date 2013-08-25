-- | POS taggauksen alkeita
module Main where

import qualified Data.List        as L
import qualified Data.List.Zipper as Z
import qualified Data.Map         as M
import           Control.Monad    (forM, forever)
import           Data.Maybe       (fromMaybe)
import TagSimplifier (simplifyTag)

type Token = String
type Tag = String

data TrainingInstance = TrainingInstance Token Tag

instance Show TrainingInstance where
  show (TrainingInstance token tag) = token ++ "/" ++ tag

data Replacement = Replacement Tag Tag
                   deriving (Eq, Ord, Show)

data TransformationRule =
    -- NextTagRule (Replacement A B) C = korvataan A B:llä kun seuraava tagi on C
      NextTagRule Replacement Tag
    | PrevTagRule Replacement Tag
    | SurroundTagRule Replacement Tag Tag
      deriving (Eq, Ord, Show)


-- tulostaa annetun syötteen sanaluokkineen
main' :: IO [()]
main' = do
  freqMap <- readFreqTagMap
  putStrLn "Syötä lause"
  forever (tagInput freqMap)

tagInput :: M.Map Token Tag -> IO ()
tagInput m = do
  tokens <- fmap words getLine
  let tagged = ruleTagSentence m tokens
  let simplified = map simplifyInstance tagged
  putStrLn $ unwords (map show simplified)

simplifyInstance :: TrainingInstance -> TrainingInstance
simplifyInstance (TrainingInstance token tag) =
  TrainingInstance token (simplifyTag tag)

-- arvioidaan POS-taggereiden tarkkuutta.
-- freqTagger                          88,5%
-- freqTagger + transformaatiosäännöt  90,4%
main :: IO [()]
main = do
  putStrLn "Arvioidaan..."
  freqMap <- readFreqTagMap
  testTxt <- readFile "brown-pos-test.txt"
  let testTs = map toTrainingInstance $ words testTxt
  let taggedPlain = freqTagMany freqMap (map takeToken testTs)
  let taggedWithRules = applyRules bestRules taggedPlain
  let results = map (evalTagged testTs) [taggedPlain, taggedWithRules]
  forM (zip ["freq", "rules"] results) $ \(name, (c, i)) -> do
    putStrLn $ "\n" ++ name ++ ":"
    putStrLn $ "yhteensä: " ++ show (c + i)
    putStrLn $ "oikein:   " ++ show c
    putStrLn $ "väärin:   " ++ show i
    putStrLn . take 5 $ show (100 * fromIntegral c / fromIntegral (c+i)) ++ "%"



-- selvitetään parhaat transformaatiosäännöt
main'' :: IO [()]
main'' = do
  putStrLn "Selvitetään 20 parasta transformaatiosääntöä..."
  ls <- fmap initialLearningState readTrainingInstances
  let rules = transformationRules ruleInstantiators ls
  forM (take 20 rules) $ \(rule, score) -> do
    print rule
    print score
    putStrLn ""



rsplit :: (Eq a) => a -> [a] -> ([a], [a])
rsplit sep xs = let (l, r, _) = foldr go ([], [], False) xs in (l, r)
  where
    go x (l, r, True)  = (x:l, r, True)
    go x (l, r, False) = if x == sep then (l, r, True) else (l, x:r, False)

toTrainingInstance :: String -> TrainingInstance
toTrainingInstance s = TrainingInstance token tag
  where (token, tag) = rsplit '/' s

takeToken :: TrainingInstance -> Token
takeToken (TrainingInstance token _) = token

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

freqTagMany :: M.Map Token Tag -> [Token] -> [TrainingInstance]
freqTagMany m = map tagFunc
  where
    tagFunc t = TrainingInstance t (backupTag freqTagger "NN" t)
    freqTagger = freqTag m

ruleTagSentence :: M.Map Token Tag -> [Token] -> [TrainingInstance]
ruleTagSentence m ts = applyRules bestRules freqTagged
  where
    freqTagged = freqTagMany m ts

evalTagger :: (Token -> Maybe Tag) -> [TrainingInstance] -> (Int, Int, Int)
evalTagger tagger = L.foldl' eval (0, 0, 0)
  where
    eval (n, c, u) (TrainingInstance token correctTag) =
      case tagger token of
        Just tag -> if tag == correctTag
                      then (n+1, c+1, u)
                      else (n+1, c, u)
        Nothing  -> (n+1, c, u+1)

evalTagged :: [TrainingInstance] -> [TrainingInstance] -> (Int, Int)
evalTagged correct proposed = foldl countHits (0, 0) (zip correct proposed)
  where
    countHits (c, u) (TrainingInstance _ corr, TrainingInstance _ prop) =
      if prop == corr then (c+1, u) else (c, u+1)


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
selectRule_ z ((rule, ruleN):rs) best@(_, bestScore)
  | bestScore >= ruleN = best
  | bestScore >= score = selectRule_ z rs best
  | otherwise          = selectRule_ z rs (rule, score)
  where score = scoreRule rule z


updateState :: TransformationRule -> Z.Zipper (Tag, Tag) ->
               Z.Zipper (Tag, Tag)
updateState rule = Z.fromList . reverse . Z.foldlz' (update rule) []
    where update r xs z =
              case ruleApplication r z of
                Just tag -> (correct, tag):xs
                Nothing  -> e:xs
              where e@(correct, _) =  Z.cursor z

transformationRules :: [Z.Zipper (Tag, Tag) -> Maybe TransformationRule] ->
                       Z.Zipper (Tag, Tag) -> [(TransformationRule, Int)]
transformationRules [] _ = []
transformationRules fs z = rule : transformationRules fs nextState
  where rule@(bestRule, _) = selectRule (sortRules $ instRules fs z) z
        nextState = updateState bestRule z

applyRules :: [TransformationRule] -> [TrainingInstance]-> [TrainingInstance]
applyRules rs ts = zipperToTs $ foldl (flip updateState) zipper rs
  where
    zipper = Z.fromList $ map (\(TrainingInstance _ tag) -> ("", tag)) ts
    zipperToTs z = map (\(TrainingInstance token _, (_, tag)) ->
                    TrainingInstance token tag) $ zip ts (Z.toList z)

-- sääntöjen selvittäminen on tuhottoman hidasta, siksi nämä ovat tässä valmiina.
-- samat säännöt selviävät main''-funktiota ajamalla.
bestRules :: [TransformationRule]
bestRules = [NextTagRule (Replacement "TO" "IN") "AT",
             PrevTagRule (Replacement "NN" "VB") "TO",
             NextTagRule (Replacement "TO" "IN") "NP",
             PrevTagRule (Replacement "VBN" "VBD") "PPS",
             PrevTagRule (Replacement "NN" "VB") "MD",
             NextTagRule (Replacement "TO" "IN") "PP$",
             PrevTagRule (Replacement "VBN" "VBD") "NP",
             PrevTagRule (Replacement "PPS" "PPO") "VB",
             NextTagRule (Replacement "TO" "IN") "JJ",
             NextTagRule (Replacement "TO" "IN") "NNS",
             NextTagRule (Replacement "TO" "IN") "CD",
             PrevTagRule (Replacement "VBN" "VBD") "PPSS",
             PrevTagRule (Replacement "VB" "NN") "AT",
             NextTagRule (Replacement "TO" "IN") "PPO",
             PrevTagRule (Replacement "NN" "VB") "PPSS",
             PrevTagRule (Replacement "VBD" "VBN") "BE",
             PrevTagRule (Replacement "VBD" "VBN") "HVD",
             PrevTagRule (Replacement "PPS" "PPO") "IN",
             SurroundTagRule (Replacement "CS" "DT") "IN" "NN",
             PrevTagRule (Replacement "VBD" "VBN") "HV"]
