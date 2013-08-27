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


newtype Parser a = Parser ([Tag] -> [(a, [Tag])])

item :: Parser Tag
item = Parser (\cs -> case cs of
                        [] -> []
                        (c:cs) -> [(c,cs)])

parse :: Parser a -> [Tag] -> [(a, [Tag])]
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


data S = Descriptive NP | Declarative NP VP
data NP = Determiner Det NOM | Nondeterminer NOM
data NOM = NounNOM Noun NOM | NOM Noun
data VP = NounVP Verb NP | PrepVP Verb PP | VP Verb
data PP = NounPP Pre NP

type Noun = String
type Verb = String
type Det = String
type Pre = String

type Tag = String

data Tree = Branch String [Tree] |Leaf String deriving Show

sat :: (Tag -> Bool) -> Parser Tag
sat p = do {t <- item; if p t then return t else mzero}

tag :: Tag -> Parser Tag
tag t = sat (== t)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = do {f <- op; b <- p; rest (f a b)} +++ return a

s = do {a <- np; b <- vp; return (Branch "S" [a, b])} +++
    do {a <- np; return (Branch "S" [a])}

np = do {tag "Det"; a <- nom; return (Branch "NP" [Leaf "Det", a])} +++
     do {a <- nom; return (Branch "NP" [a])}

nom = do {tag "Noun"; a <- nom; return (Branch "NOM" [Leaf "Noun", a])} +++
      do {a <- tag "Noun"; return (Branch "NOM" [Leaf "Noun"])}

vp = do {tag "Verb"; a <- np; return (Branch "VP" [Leaf "Verb", a])} +++
     do {tag "Verb"; a <- pp; return (Branch "VP" [Leaf "Verb", a])} +++
     do {tag "Verb"; return (Branch "VP" [Leaf "Verb"])}

pp = do {tag "Pre"; a <- np; return (Branch "PP" [Leaf "Pre", a])}



tree = np +++ nom
tree2 = np `mplus` nom


