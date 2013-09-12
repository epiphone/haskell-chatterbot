{-
    Parsii kieliopin

    S   -> NP                 the green dog
         | VP                 take it with you
         | NP VP              a green dog jumped on the roof
         | Aux Pro ONP         are you a computer? was she a computer?
         | Aux NP VP          did the green dog jump on the roof?
         | WH VP              what is your name?
         | WH NP VP           which student wrote the program?
         | WH Aux NP VP       what do you mean?
         | WH NP Aux NP       which car is the best?
         | WH NP Aux NP VP    what flights do you have?
    NP  -> NOM                green dog
         | Det NOM            the green dog, many green dogs
    NOM -> NW                 beautiful, ice, they
         | NW NOM
    VP  -> QW                 do, can, drive, smile
         | QW NP              drive a car
         | Aux VP             will drive, is driving
         | Adv VP             often drives
         | Verb To VP         want to help
         | Verb PP            travel to Spain
         | QW NP PP           drive my car to Spain
    PP  -> Pre NP             in Spain, from Spain
         | To NP              to Spain

    QW  -> Verb | Aux
    NW  -> Noun | Pro | Adj
    WH  -> Wh | Wh Adj | Wh Adv | Wh Det | Wh Pre


    Päätemerkit:
    Noun, Pro, Verb, Adj, Adv, Wh, Det, Pre, Aux, To

    Parseri perustuu artikkeliin
    Hutton & Meijer - Monadic Parsing in Haskell (cs.nott.ac.uk/~gmh/pearl.pdf)
-}

module Parser where

import           Control.Monad (MonadPlus, mplus, mzero)
import           Tagger        (Tag, Token)


newtype Parser a = Parser ([(Token, Tag)] -> [(a, [(Token, Tag)])])

parse :: Parser a -> [(Token, Tag)] -> [(a, [(Token, Tag)])]
parse (Parser p) = p

instance Functor Parser where
    fmap f p = do
        result <- p
        return (f result)

instance Monad Parser where
    return a = Parser (\cs -> [(a,cs)])
    p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

instance MonadPlus Parser where
    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
    mzero = Parser (const [])

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
                           [] -> []
                           (x:_) -> [x])


data Noun = Noun String deriving (Show, Eq)
data Pro  = Pro String deriving (Show, Eq)
data Verb = Verb String deriving (Show, Eq)
data Adj  = Adj String deriving (Show, Eq)
data Adv  = Adv String deriving (Show, Eq)
data Wh   = Wh String deriving (Show, Eq)
data Det  = Det String deriving (Show, Eq)
data Pre  = Pre String deriving (Show, Eq)
data Aux  = Aux String deriving (Show, Eq)
data To   = To String deriving (Show, Eq)

data S = Descriptive NP
       | Imperative VP
       | Declarative NP VP
       | SimpleInterrogative Aux Pro NP
       | Interrogative Aux NP VP
       | SimpleWhQuestion WH VP
       | WhQuestion WH Aux NP VP
       | WhSubject WH NP VP
       | WhSimpleNonSubject WH NP Aux NP
       | WhNonSubject WH NP Aux NP VP deriving (Show, Eq)

data NP = DetNP Det NOM
        | NonDetNP NOM deriving (Show, Eq)

data NOM = NounNOM NW NOM
         | SimpleNOM NW deriving (Show, Eq)

data VP = SimpleVP QW
        | NounVP QW NP
        | AuxVP Aux VP
        | AdvVP Adv VP
        | ToVP Verb To VP
        | PrepVP Verb PP
        | ComplexVP QW NP PP deriving (Show, Eq)

data PP = PrePP Pre NP
        | ToPP To NP deriving (Show, Eq)

data QW = VerbQW Verb | AuxQW Aux deriving (Show, Eq)

data NW = NounNW Noun | ProNW Pro | AdjNW Adj deriving (Show, Eq)

data WH = SimpleWH Wh
        | AdjWH Wh Adj
        | AdvWH Wh Adv
        | DetWH Wh Det
        | PreWH Wh Pre deriving (Show, Eq)


-- apufunktioita:

item :: Parser (Token, Tag)
item = Parser (\cs -> case cs of
                        [] -> []
                        (x:xs) -> [(x, xs)])

sat :: ((Token, Tag) -> Bool) -> Parser (Token, Tag)
sat p = do {t <- item; if p t then return t else mzero}

tag :: Tag -> Parser Token
tag t = fmap fst $ sat (\(_, tag') -> tag' == t)


-- päätesymbolit:

toT, adjT, advT, nounT, proT, verbT, detT, preT, whT, auxT :: Parser Token
toT   = tag "TO"
adjT  = tag "ADJ"
advT  = tag "ADV"
nounT = tag "N"
proT  = tag "PRO"
verbT = tag "V"
detT  = tag "DET"
preT  = tag "P"
whT   = tag "WH"
auxT  = tag "AUX"

-- välikesymbolit:

s :: Parser S
s = do {a <- wh; b <- np; c <- auxT; d <- np; e <- vp; return (WhNonSubject a b (Aux c) d e)} +++
    do {a <- wh; b <- np; c <- auxT; d <- np; return (WhSimpleNonSubject a b (Aux c) d)} +++
    do {a <- wh; b <- auxT; c <- np; d <- vp; return (WhQuestion a (Aux b) c d)} +++
    do {a <- wh; b <- np; c <- vp; return (WhSubject a b c)} +++
    do {a <- wh; b <- vp; return (SimpleWhQuestion a b)} +++
    do {a <- auxT; b <- proT; c <- np; return (SimpleInterrogative (Aux a) (Pro b) c)} +++
    do {a <- auxT; b <- np; c <- vp; return (Interrogative (Aux a) b c)} +++
    do {a <- np; b <- vp; return (Declarative a b)} +++
    do {a <- vp; return (Imperative a)} +++
    do {a <- np; return (Descriptive a)}

np :: Parser NP
np = do {a <- detT; b <- nom; return (DetNP (Det a) b)} +++
     do {a <- nom; return (NonDetNP a)}

nom :: Parser NOM
nom = do {a <- nw; b <- nom; return (NounNOM a b)} +++
      do {a <- nw; return (SimpleNOM a)}

vp :: Parser VP
vp = do {a <- qw; b <- np;  c <- pp; return (ComplexVP a b c)} +++
     do {a <- verbT; b <- toT; c <- vp; return (ToVP (Verb a) (To b) c)} +++
     do {a <- verbT; b <- pp; return (PrepVP (Verb a) b)} +++
     do {a <- advT;  b <- vp; return (AdvVP (Adv a) b)} +++
     do {a <- auxT;  b <- vp; return (AuxVP (Aux a) b)} +++
     do {a <- qw; b <- np; return (NounVP a b)} +++
     do {a <- qw; return (SimpleVP a)}

pp :: Parser PP
pp = do {a <- preT; b <- np; return (PrePP (Pre a) b)} +++
     do {a <- toT; b <- np; return (ToPP (To a) b)}

qw :: Parser QW
qw = do {a <- verbT; return (VerbQW (Verb a))} +++
     do {a <- auxT; return (AuxQW (Aux a))}

nw :: Parser NW
nw = do {a <- nounT; return (NounNW (Noun a))} +++
     do {a <- proT; return (ProNW (Pro a))} +++
     do {a <- adjT; return (AdjNW (Adj a))}

wh :: Parser WH
wh = do {a <- whT; b <- preT; return (PreWH (Wh a) (Pre b))} +++
     do {a <- whT; b <- detT; return (DetWH (Wh a) (Det b))} +++
     do {a <- whT; b <- advT; return (AdvWH (Wh a) (Adv b))} +++
     do {a <- whT; b <- adjT; return (AdjWH (Wh a) (Adj b))} +++
     do {a <- whT; return (SimpleWH (Wh a))}


parseSentence :: [(Token, Tag)] -> Maybe S
parseSentence ts = case parse s ts of
                     []          -> Nothing
                     (res, []):_ -> Just res
                     (_, _):_    -> Nothing
