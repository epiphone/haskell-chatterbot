{-
    Palauttaa Brown Corpus-sanaluokkatagia vastaavan yksinkertaistetun
    tagin.

    Brown Corpus käyttää 82 tagia (http://icame.uib.no/brown/bcm.html),
    jotka yksinkertaistetaan johonkin seuraavista 12 tagista:

    - ADJ adjective                 new, good, high, special, big, local
    - ADV adverb                    really, already, still, early, now
    - CNJ conjunction               and, or, but, if, while, although
    - DET determiner                the, a, some, most, every, no
    - AUX auxiliary & modal verbs   be, have, will, can, would, may, need
    - N   noun                      year, home, costs, time, education
    - PRO pronoun                   he, their, her, its, my, I, us
    - P   preposition               on, of, at, with, by, into, under
    - TO  the word to               to
    - V   (VD, VG, VN) verb         is, has, get, do, make, see, run
    - WH  wh determiner             who, which, when, what, where, how
    - X   catch-all for interjections, numbers, foreign words etc.
-}

module TagSimplifier (simplifyTag) where

import qualified Data.Map as M
import Data.Char (toLower)
import Data.Maybe (catMaybes)

mapping :: M.Map String String
mapping = M.fromList [
    ("j", "ADJ"), ("p", "PRO"), ("m", "AUX"), ("q", "DET"), ("w", "WH"),
    ("r", "ADV"), ("i", "P"), ("u", "X"), ("e", "X"), ("o", "X"),
    ("b", "AUX"), ("h", "AUX"), ("f", "X"), ("a", "DET"), ("t", "TO"),
    ("cc", "CNJ"), ("cs", "CNJ"), ("cd", "X"), ("do", "V"), ("dt", "DET"),
    ("nn", "N"), ("nr", "N"), ("np", "N"), ("nc", "N"), ("v", "V")]


simplifyTag :: String -> String
simplifyTag t = head $ catMaybes [M.lookup (take 1 tag) mapping,
                                  M.lookup (take 2 tag) mapping,
                                  Just t]
    where tag = map toLower t