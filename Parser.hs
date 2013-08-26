module Parser where

--import Control.Monad

--newtype Parser a = Parser (String -> [(a, String)])

--parse :: Parser a -> String -> [(a, String)]
--parse (Parser p) = p

--instance Monad Parser where
--    return a = Parser (\cs -> [(a,cs)])
--    p >>= f = Parser (\cs -> concat [parse (f a) cs' | (a,cs') <- parse p cs])

--instance MonadPlus Parser where
--    p `mplus` q = Parser (\cs -> parse p cs ++ parse q cs)
--    mzero = Parser (const [])

--(+++) :: Parser a -> Parser a -> Parser a
--p +++ q = Parser (\cs -> case parse (p `mplus` q) cs of
--                           [] -> []
--                           (x:_) -> [x])

data Tree nt t = Leaf t | Node nt [Tree nt t]

data S = Declarative NP VP
       | Descriptive NP
       | Imperative VP
       | Interrogative V NP VP

data NP = SimpleNP NOM
        | DetNP Det NOM

data NOM = SimpleNOM Noun
         | ComplexNOM Noun NOM
         | ProNOM Pro
         | ComplexProNOM Pro NOM
         | SimpleAdjNOM Adj
         | ComplexAdjNOM Adj NOM

data VP = SimpleVP V
        | NounVP V NP
        | PreAdvVP Adv VP
        | PostAdvVP VP Adv

data V = AuxV Aux | OtherV Verb


type Det = String
type Noun = String
type Pro = String
type Adj = String
type Adv = String
type Aux = String
type Verb = String

{-
1. Jokaista välikettä X kohden funktio fX, joka ottaa parametrin X:n produktiosta

2. Jos X:llä on vaihtoehtoisia produktioita, funktiolla fX on versio jokaisesta
   vaihtoehdosta

3. Jos X:llä on produktio johon sisältyy välike Y, niin
   a) fX:n tulee kutsua funktiota fY, joka käsittelee Y:n kuvaamaa dataa, ja
   b) jokainen kutsu fX:stä fY:hyn vie fY:hyn dataa jonka fX sai parametriksi, ainakin sen
      osan mitä Y kuvaa.
-}
