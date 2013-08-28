{-
Testiparseri kieliopille

S   -> NP | NP VP
NP  -> NOM | Det NOM
NOM -> Noun | Noun NOM
VP  -> Verb | Verb NP | Verb PP
PP  -> Pre NP

Terminaalit
Noun
Verb
Det
Pre

-}
module SimpleParser where

import Control.Monad (MonadPlus, mzero, mplus)


newtype Parser a = Parser ([(Token, Tag)] -> [(a, [(Token, Tag)])])

parse :: Parser a -> [(Token, Tag)] -> [(a, [(Token, Tag)])]
parse (Parser p) = p

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadPlus Parser where
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
    mzero = Parser (\cs -> [])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                           [] -> []
                           (x:xs) -> [x])


data Noun = Noun String deriving (Show)
data Verb = Verb String deriving (Show)
data Det = Det String deriving (Show)
data Pre = Pre String deriving (Show)

data S = Declarative NP VP | Descriptive NP deriving Show
data NP = DetNP Det NOM | NonDetNP NOM deriving Show
data NOM = NounNOM Noun NOM | SimpleNOM Noun deriving Show
data VP = NounVP Verb NP | PrepVP Verb PP | SimpleVP Verb deriving Show
data PP = NounPP Pre NP deriving Show

type Token = String
type Tag = String


data Tree = Branch String [Tree] |Leaf String deriving Show

item :: Parser (Token, Tag)
item = Parser (\cs -> case cs of
                        [] -> []
                        (c:cs) -> [(c,cs)])

sat :: ((Token, Tag) -> Bool) -> Parser (Token, Tag)
sat p = do {t <- item; if p t then return t else mzero}

tag :: Tag -> Parser (Token, Tag)
tag t = sat (\(tok, tag) -> tag == t)

noun, verb, det, pre :: Parser (Token, Tag)
noun = tag "Noun"
verb = tag "Verb"
det = tag "Det"
pre = tag "Pre"


many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = do {f <- op; b <- p; rest (f a b)} +++ return a

s :: Parser S
s = do {a <- np; b <- vp; return (Declarative a b)} +++
    do {a <- np; return (Descriptive a)}

np :: Parser NP
np = do {(t, _) <- det; a <- nom; return (DetNP (Det t) a)} +++
     do {a <- nom; return (NonDetNP a)}

nom :: Parser NOM
nom = do {(t, _) <- noun; b <- nom; return (NounNOM (Noun t) b)} +++
      do {(t, _) <- noun; return (SimpleNOM (Noun t))}

vp :: Parser VP
vp = do {(t, _) <- verb; a <- np; return (NounVP (Verb t) a)} +++
     do {(t, _) <- verb; a <- pp; return (PrepVP (Verb t) a)} +++
     do {(t, _) <- verb; return (SimpleVP (Verb t))}

pp :: Parser PP
pp = do {(t, _) <- pre; a <- np; return (NounPP (Pre t) a)}



