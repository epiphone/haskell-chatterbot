module Main where

import qualified Data.Map as M
import qualified Data.Vector as V
import Data.List (foldl', tails, sortBy)
import Data.Maybe (isJust)

data SuffixArray a = SuffixArray (V.Vector a) (V.Vector Int)
                     deriving Show

main :: IO ()
main = do
    putStrLn "Enter n words to find the number of collocations\n>>>"
    ws <- fmap words getLine
    contents <- fmap words $ readFile "brown.txt"
    let freqs = freqList (ngrams (length ws) contents)
    case M.lookup ws freqs of
        Just n  -> putStrLn $ "Collocations found: " ++ show n
        Nothing -> putStrLn "No collocations found"

ws = ["Colorless", "green", "ideas", "sleep", "furiously"] :: [String]


bigrams :: [a] -> [[a]]
bigrams (x:y:xs) = [x,y] : bigrams (y:xs)
bigrams _        = []


freqList :: Ord a => [a] -> M.Map a Int
freqList xs = foldl' (\acc x -> M.insertWith (+) x 1 acc) M.empty xs


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
