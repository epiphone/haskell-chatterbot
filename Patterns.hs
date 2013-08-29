{-
    Muodostaa vastauksen jäsennyspuun mukaan.
-}
module Patterns where

import Parser
import qualified Data.Map as M

answer :: S -> String
answer (Descriptive np) = "Tell me more about " ++ pluckNP np

answer (Imperative (SimpleVP (VerbQW (Verb v)))) = "Why should I " ++ v ++ "?"
answer (Imperative (NounVP (VerbQW (Verb v)) np)) = "Why should I " ++ v ++ " " ++ pluckNP np ++ "?"
answer (Imperative _) = "Why would anyone do that?"

answer (Declarative np@(DetNP _ _) vp) = "What else do you know about " ++ pluckNP np ++ "?"
answer (Declarative np (NounVP (AuxQW (Aux aux)) vnp))
  | aux == "has" = "Maybe you have " ++ pluckNP vnp
  | aux == "is"  = "Maybe you are " ++ pluckNP vnp
  | otherwise = "Fascinating. Tell me more."
answer (Declarative np _)
  | subject == "i" = "That doesn't sound like me. Are you sure about that?"
  | subject == "you" = "Good for you. Now tell me something else."
  | otherwise = "Should " ++ subject ++ " be doing something like that?"
  where subject = pluckNP np

answer (Interrogative (Aux aux) np _) = pluckNP np ++ " sure " ++ aux ++ ". What about it?"

answer (SimpleWhQuestion (SimpleWH (Wh wh)) (NounVP (AuxQW (Aux aux)) np))
  | subject == "i" = "That's personal."
  | wh == "what" = subject ++ " " ++ aux ++ " none of your business."
  | otherwise = "I don't know. How does " ++ subject ++ " make you feel?"
  where subject = pluckNP np


answer _ = "Tell me more."


-- poimii NP-rakenteen sanat merkkijonoksi
pluckNP :: NP -> String
pluckNP (DetNP det nom) = unwords [pluckDet det, pluckNOM nom]
pluckNP (NonDetNP nom) = pluckNOM nom

pluckNOM :: NOM -> String
pluckNOM (NounNOM nw nom) = unwords [pluckNW nw, pluckNOM nom]
pluckNOM (SimpleNOM nw) = pluckNW nw

pluckNW :: NW -> String
pluckNW (NounNW n) = pluckNoun n
pluckNW (ProNW n) = pluckPro n
pluckNW (AdjNW n) = pluckAdj n

pluckNoun :: Noun -> String
pluckNoun (Noun w) = w

pluckPro :: Pro -> String
pluckPro (Pro w) = M.findWithDefault w w pronounOpposites

pluckAdj :: Adj -> String
pluckAdj (Adj w) = w

pluckDet :: Det -> String
pluckDet (Det w) = M.findWithDefault w w determinerOpposites


pronounOpposites, determinerOpposites :: M.Map String String
pronounOpposites = M.fromList [("i", "you"), ("you", "i")]
determinerOpposites = M.fromList [("my", "your"), ("your", "my")]
