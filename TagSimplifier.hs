{-
    Palauttaa Brown Corpus-sanaluokkatagia vastaavan yksinkertaistetun
    tagin.

    Brown Corpus käyttää 82 tagia (http://icame.uib.no/brown/bcm.html),
    jotka yksinkertaistetaan johonkin seuraavista 11 tagista:

    - ADJ adjektiivi                 new, good, high, special, big, local
    - ADV adverbi                    really, already, still, early, now
    - DET määrittelijä               the, a, some, every, no, my, her, two, 13
    - AUX apuverbi                   be, have, will, can, would, may, need
    - N   substantiivi               year, home, costs, time, education
    - PRO pronomini                  he, their, her, its, my, I, us
    - P   prepositio                 on, of, at, with, by, into, under
    - TO  'to'                       to
    - V   verbi                      get, make, see, run
    - WH  wh-kysymyssana             who, which, when, what, where, how
    - X   muut: mm. konjuktiot, interjektiot, vieraskieliset sanat
-}

module TagSimplifier (simplifyTag) where

import qualified Data.Map as M
import Data.Char (toLower)
import Data.Maybe (catMaybes)

mapping1, mapping2 :: M.Map String String
mapping1 = M.fromList [
    ("pp$", "DET")]

mapping2 = M.fromList [
    ("j", "ADJ"), ("p", "PRO"), ("m", "AUX"), ("q", "DET"), ("w", "WH"),
    ("r", "ADV"), ("i", "P"), ("u", "X"), ("e", "X"), ("o", "DET"),
    ("b", "AUX"), ("h", "AUX"), ("f", "X"), ("a", "DET"), ("t", "TO"),
    ("cc", "X"), ("cs", "X"), ("cd", "DET"), ("do", "AUX"), ("dt", "DET"),
    ("nn", "N"), ("nr", "N"), ("np", "N"), ("nc", "N"), ("v", "V")]


simplifyTag :: String -> String
simplifyTag t = head $ catMaybes [M.lookup (take 3 tag) mapping1,
                                  M.lookup (take 1 tag) mapping2,
                                  M.lookup (take 2 tag) mapping2,
                                  Just t]
    where tag = map toLower t
