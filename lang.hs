{-# LANGUAGE BangPatterns #-}
-- | Luonnollisen kielen käsittelyä, esimerkkien (<http://nlpwp.org/>) mukaan.
module Lang where

import           Data.List   (foldl', sortBy, tails)
import qualified Data.Map    as M
import           Data.Maybe  (isJust)
import           Data.Ord    (comparing)
import qualified Data.Set    as S
import qualified Data.Vector as V


data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
                     deriving Show

main :: IO ()
main = do
    putStrLn "The most frequent bigram and trigram in the Brown corpus:"
    !sa <- fmap (saFromList . words) $ readFile "brown.txt"
    print $ mostFrequentNgram sa 2

bigrams :: [a] -> [[a]]
bigrams (x:y:xs) = [x,y] : bigrams (y:xs)
bigrams _        = []


freqList :: Ord a => [a] -> M.Map a Int
freqList = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty


ngrams :: Int -> [a] -> [[a]]
ngrams n = filter ((==) n . length) . map (take n) . tails


skipBigrams :: [a] -> [(a, a)]
skipBigrams xs = concat [zip (repeat t) ts | (t:ts) <- tails xs]


saCompare :: Ord a => (V.Vector a -> V.Vector a -> Ordering) ->
             V.Vector a -> Int -> Int -> Ordering
saCompare cmp d a b = cmp (V.drop a d) (V.drop b d)


suffixArrayBy :: Ord a => (V.Vector a -> V.Vector a -> Ordering) ->
                 V.Vector a -> SuffixArray a
suffixArrayBy cmp d = SuffixArray d (V.fromList srtIndex)
    where
      usrtIndex = [0..V.length d - 1]
      srtIndex  = sortBy (saCompare cmp d) usrtIndex

suffixArray :: Ord a => V.Vector a -> SuffixArray a
suffixArray = suffixArrayBy compare


saFromList :: Ord a => [a] -> SuffixArray a
saFromList = suffixArray . V.fromList


saToList :: SuffixArray a -> [[a]]
saToList (SuffixArray ds is) = V.foldr go [] is
    where go i acc = V.toList (V.drop i ds) : acc


saElems :: SuffixArray a -> V.Vector (V.Vector a)
saElems (SuffixArray ds is) = V.map (`V.drop` ds) is


binarySearchByBounded :: (Ord a) => (a -> a -> Ordering) -> V.Vector a ->
                       a -> Int -> Int -> Maybe Int

binarySearchByBounded cmp v e lower upper
    | V.null v      = Nothing
    | upper < lower = Nothing
    | otherwise     = case cmp e (v V.! middle) of
                        LT -> binarySearchByBounded cmp v e lower (middle - 1)
                        EQ -> Just middle
                        GT -> binarySearchByBounded cmp v e (middle + 1) upper
    where middle = (lower + upper) `div` 2


binarySearchBy :: (Ord a) => (a -> a -> Ordering) -> V.Vector a -> a ->
                  Maybe Int
binarySearchBy cmp v e = binarySearchByBounded cmp v e 0 (V.length v - 1)


binarySearch :: (Ord a) => V.Vector a -> a -> Maybe Int
binarySearch = binarySearchBy compare

contains :: (Ord a) => SuffixArray a -> V.Vector a -> Bool
contains sa v = isJust $ binarySearch (elements sa) v
    where elements = V.map (V.take $ V.length v) . saElems


lowerBoundByBounds :: (Ord a) => (a -> a -> Ordering) -> V.Vector a ->
                       a -> Int -> Int -> Maybe Int
lowerBoundByBounds cmp v e lower upper
    | V.null v = Nothing
    | upper == lower = case cmp e (v V.! lower) of
                         EQ -> Just lower
                         _  -> Nothing
    | otherwise = case cmp e (v V.! middle) of
                    GT -> lowerBoundByBounds cmp v e (middle + 1) upper
                    _  -> lowerBoundByBounds cmp v e lower middle
    where middle = (upper + lower) `div` 2

lowerBoundBy :: (Ord a) => (a -> a -> Ordering) -> V.Vector a -> a -> Maybe Int
lowerBoundBy cmp v e = lowerBoundByBounds cmp v e 0 (V.length v - 1)

lowerBound :: (Ord a) => V.Vector a -> a -> Maybe Int
lowerBound = lowerBoundBy compare


upperBoundByBounds :: (Ord a) => (a -> a -> Ordering) -> V.Vector a ->
                       a -> Int -> Int -> Maybe Int
upperBoundByBounds cmp v e lower upper
    | V.null v = Nothing
    | upper == lower = case cmp e (v V.! lower) of
                         EQ -> Just lower
                         _  -> Nothing
    | otherwise = case cmp e (v V.! middle) of
                    LT -> upperBoundByBounds cmp v e lower (middle - 1)
                    _  -> upperBoundByBounds cmp v e middle upper
    where middle = ((upper + lower) `div` 2) + 1

upperBoundBy :: (Ord a) => (a -> a -> Ordering) -> V.Vector a -> a -> Maybe Int
upperBoundBy cmp v e = upperBoundByBounds cmp v e 0 (V.length v - 1)

upperBound :: (Ord a) => V.Vector a -> a -> Maybe Int
upperBound = upperBoundBy compare


narrowSearchBy :: (Ord a) => (a -> a -> Ordering) ->  V.Vector a -> a ->
                  Int -> Int -> Maybe (Int, Int, Int)
narrowSearchBy cmp v e lower upper
    | V.null v  = Nothing
    | otherwise = case cmp e (v V.! middle) of
                    LT -> narrowSearchBy cmp v e lower (middle - 1)
                    EQ -> Just (lower, middle, upper)
                    GT -> narrowSearchBy cmp v e (middle +1) upper
    where middle = (upper + lower) `div` 2

frequencyByBounds :: (Ord a) => (a -> a -> Ordering) -> V.Vector a ->
                     a -> Int -> Int -> Maybe Int
frequencyByBounds cmp v e lower upper = do
    (l,m,u)    <- narrowSearchBy compare v e lower upper
    firstIndex <- lowerBoundByBounds cmp v e l     m
    lastIndex  <- upperBoundByBounds cmp v e m     u
    return $ lastIndex - firstIndex + 1

frequencyBy :: (Ord a) => (a -> a -> Ordering) -> V.Vector a -> a -> Maybe Int
frequencyBy cmp v e = frequencyByBounds cmp v e 0 (V.length v - 1)

frequency :: (Ord a) => V.Vector a -> a -> Int
frequency v e = case frequencyBy compare v e of
                  Just x -> x
                  _      -> 0

saFrequency :: (Ord a) => SuffixArray a -> V.Vector a -> Int
saFrequency sa v
    | V.null elems = 0
    | otherwise    = frequency (V.map (V.take $ V.length v) elems) v
    where elems = saElems sa

removeConsDuplicates :: (Eq a) => V.Vector a -> V.Vector a
removeConsDuplicates = V.foldr' f V.empty
    where f x acc = if V.null acc || x /= V.head acc then x `V.cons` acc else acc

removeDuplicates :: (Eq a) => V.Vector a -> V.Vector a
removeDuplicates = V.fromList . S.toList . S.fromAscList . V.toList

mostFrequentNgram :: Ord a => SuffixArray a -> Int -> Maybe (V.Vector a, Int)
mostFrequentNgram sa n
    | V.null elems = Nothing
    | otherwise    = Just $ V.maximumBy (comparing snd) freqs
    where
      elems = V.map (V.take n) (saElems sa)
      setElems = removeDuplicates elems
      freqs = V.map (\x -> (x, frequency elems x)) setElems

