{-
  Parsii kieliopin

  S   -> NP                 the green dog
       | VP                 take it with you
       | NP VP              a green dog jumped on the roof
       | Aux NP VP          did the green dog jump on the roof?
       | WH VP              what is your name?
       | WH NP VP           which name is the best?
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

-}

module Parser where

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

data Noun = Noun String deriving (Show, Eq)
data Pro  = Pro String deriving (Show, Eq)
data Verb = Verb String deriving (Show, Eq)
data Adj  = Adj String deriving (Show, Eq)
data Adv  = Adv String deriving (Show, Eq)
data Wh   = Wh String deriving (Show, Eq)
data Det  = Det String deriving (Show, Eq)
data Pre  = Pre String deriving (Show, Eq)
data Aux  = Aux String deriving (Show, Eq)
data To  = To String deriving (Show, Eq)

data S = Descriptive NP
       | Imperative VP
       | Declarative NP VP
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


type Token = String -- TODO importtaa tagger.hs:stä
type Tag = String


-- apufunktioita

item :: Parser (Token, Tag)
item = Parser (\cs -> case cs of
                        [] -> []
                        (c:cs) -> [(c,cs)])

sat :: ((Token, Tag) -> Bool) -> Parser (Token, Tag)
sat p = do {t <- item; if p t then return t else mzero}

tag :: Tag -> Parser (Token, Tag)
tag t = sat (\(tok, tag) -> tag == t)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do {a <- p; as <- many p; return (a:as)}

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
    where rest a = do {f <- op; b <- p; rest (f a b)} +++ return a


-- päätesymbolit

toT, adjT, advT, nounT, proT, verbT, detT, preT, whT, auxT :: Parser (Token, Tag)
toT = tag "TO"
adjT = tag "ADJ"
advT = tag "ADV"
nounT = tag "N"
proT = tag "PRO"
verbT = tag "V"
detT = tag "DET"
preT = tag "P"
whT = tag "WH"
auxT = tag "AUX"



-- välikkeet

s :: Parser S
s = do {a <- wh; b <- np; c <- auxT; d <- np; e <- vp; return (WhNonSubject a b (Aux (fst c)) d e)} +++
    do {a <- wh; b <- np; c <- auxT; d <- np; return (WhSimpleNonSubject a b (Aux (fst c)) d)} +++
    do {a <- wh; b <- auxT; c <- np; d <- vp; return (WhQuestion a (Aux (fst b)) c d)} +++
    do {a <- wh; b <- np; c <- vp; return (WhSubject a b c)} +++
    do {a <- wh; b <- vp; return (SimpleWhQuestion a b)} +++
    do {a <- auxT; b <- np; c <- vp; return (Interrogative (Aux (fst a)) b c)} +++
    do {a <- np; b <- vp; return (Declarative a b)} +++
    do {a <- vp; return (Imperative a)} +++
    do {a <- np; return (Descriptive a)}

np :: Parser NP
np = do {a <- detT; b <- nom; return (DetNP (Det (fst a)) b)} +++
     do {a <- nom; return (NonDetNP a)}

nom :: Parser NOM
nom = do {a <- nw; b <- nom; return (NounNOM a b)} +++
      do {a <- nw; return (SimpleNOM a)}

vp :: Parser VP
vp = do {a <- qw; b <- np;  c <- pp; return (ComplexVP a b c)} +++
     do {a <- verbT; b <- toT; c <- vp; return (ToVP (Verb (fst a)) (To (fst b)) c)} +++
     do {a <- verbT; b <- pp; return (PrepVP (Verb (fst a)) b)} +++
     do {a <- advT;  b <- vp; return (AdvVP (Adv (fst a)) b)} +++
     do {a <- auxT;  b <- vp; return (AuxVP (Aux (fst a)) b)} +++
     do {a <- qw; b <- np; return (NounVP a b)} +++
     do {a <- qw; return (SimpleVP a)}

pp :: Parser PP
pp = do {a <- preT; b <- np; return (PrePP (Pre (fst a)) b)} +++
     do {a <- toT; b <- np; return (ToPP (To (fst a)) b)}

qw :: Parser QW
qw = do {a <- verbT; return (VerbQW (Verb (fst a)))} +++
     do {a <- auxT; return (AuxQW (Aux (fst a)))}

nw :: Parser NW
nw = do {a <- nounT; return (NounNW (Noun (fst a)))} +++
     do {a <- proT; return (ProNW (Pro (fst a)))} +++
     do {a <- adjT; return (AdjNW (Adj (fst a)))}

wh :: Parser WH
wh = do {a <- whT; b <- preT; return (PreWH (Wh (fst a)) (Pre (fst b)))} +++
     do {a <- whT; b <- detT; return (DetWH (Wh (fst a)) (Det (fst b)))} +++
     do {a <- whT; b <- advT; return (AdvWH (Wh (fst a)) (Adv (fst b)))} +++
     do {a <- whT; b <- adjT; return (AdjWH (Wh (fst a)) (Adj (fst b)))} +++
     do {a <- whT; return (SimpleWH (Wh (fst a)))}


parseSentence :: [(Token, Tag)] -> Maybe S
parseSentence ts = case parse s ts of
                     []  -> Nothing
                     res -> Just . fst . head $ res
