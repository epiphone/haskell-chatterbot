{-
    Muodostaa vastauksen jäsennyspuun mukaan.
-}
module Patterns where

import Parser
import qualified Data.Map as M

answer :: S -> String
answer (Descriptive np) = "Tell me more about " ++ pluckNP np

answer (Imperative (NounVP (AuxQW _) _)) = "What do you think?"
answer (Imperative (SimpleVP (VerbQW (Verb v)))) = "Why should I " ++ v ++ "?"
answer (Imperative (NounVP (VerbQW (Verb v)) np)) = "Why should I " ++ v ++ " " ++ pluckNP np ++ "?"
answer (Imperative _) = "Why would anyone do that?"

answer (Declarative np@(DetNP _ _) vp) = "What else do you know about " ++ pluckNP np ++ "?"
answer (Declarative np (NounVP (AuxQW (Aux aux)) vnp))
  | aux == "has" = "Maybe you have " ++ pluckNP vnp
  | aux `elem` ["is", "are", "am"] = "Maybe you are " ++ pluckNP vnp
  | otherwise = "Fascinating. Tell me more."
answer (Declarative np _)
  | sub == "i" = "That doesn't sound like me. Are you sure about that?"
  | sub == "you" = "Good for you. Now tell me something else."
  | otherwise = "Should " ++ sub ++ " be doing something like that?"
  where sub = pluckNP np

answer (SimpleInterrogative (Aux aux) (Pro pro) np)
  | aux == "are" && pro == "you" = "How would you feel if I were " ++ obj ++ "?"
  | aux == "am" && pro =="i" = "Yes. You are " ++ obj
  | otherwise = "I'm certain that " ++ pro ++ " " ++ aux ++ " " ++ obj ++ "."
  where obj = pluckNP np

answer (Interrogative (Aux aux) np _)
  | aux == "am" = sub ++ " sure are. But why?"
  | aux == "are" = sub ++ " sure am. Are you?"
  | otherwise = sub ++ " sure " ++ aux ++ ". What about it?"
  where sub = pluckNP np

answer (SimpleWhQuestion (SimpleWH (Wh wh)) (NounVP (AuxQW (Aux aux)) np))
  | sub == "i" = "That's personal."
  | wh == "what" = sub ++ " " ++ aux ++ " none of your business."
  | otherwise = "I don't know. How does " ++ sub ++ " make you feel?"
  where sub = pluckNP np
answer (SimpleWhQuestion _ _) = "I have no idea. What do you think?"

answer (WhQuestion (SimpleWH (Wh wh)) (Aux aux) np (SimpleVP (VerbQW (Verb verb))))
  | sub == "i" && aux == "are" = "Why do you want to know " ++ wh ++ " I am " ++ verb ++ "?"
  | sub == "i" = "Why do you want to know " ++ wh ++ " I " ++ verb ++ "?"
  | sub == "you" = "Shouldn't you know that?"
  | aux == "are" = "..." ++ wh ++ " are you " ++ verb ++ "?"
  | aux == "do" = sub ++ " are a mystery to me."
  | aux == "does" = sub ++ " is a mystery to me."
  | otherwise = "Why don't you ask " ++ sub ++ "?"
  where sub = pluckNP np
answer (WhQuestion _ (Aux aux) np _)
  | sub == "i" = "That's for me to know."
  | sub == "you" = "Why are you asking me?"
  | otherwise = sub ++ " might know the answer."
  where sub = pluckNP np

answer (WhSubject _ np _) = "The best one. Why do you ask?"

answer (WhSimpleNonSubject (SimpleWH (Wh wh)) np1 (Aux aux) np2)
  | wh `elem` ["what", "which"] = unwords ["You know full well", wh, sub, aux, obj ++ "."]
  | otherwise = "Why should I tell you that?"
  where sub = pluckNP np1
        obj = pluckNP np2
answer (WhSimpleNonSubject _ _ _ _) = "What made you ask that?"

answer (WhNonSubject (SimpleWH (Wh wh)) np1 (Aux aux) np2 (SimpleVP (VerbQW (Verb verb))))
  | wh `elem` ["what", "which"] = unwords [sub, aux, verb, "any", obj]
  | otherwise = "Depends on the " ++ obj
  where sub = pluckNP np2
        obj = pluckNP np1
answer (WhNonSubject wh np1 _ np2 _)
  | wh == DetWH (Wh "how") (Det "many") = "As many as you please."
  | sub == "i" = obj ++ " are none of my concern."
  | sub == "you" = obj ++ " are none of your concern."
  | otherwise = "Only " ++ sub ++ " know the answer."
  where sub = pluckNP np2
        obj = pluckNP np1


-- apufunktioita:

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
