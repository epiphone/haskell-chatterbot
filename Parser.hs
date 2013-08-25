module Parser where

data Tree nt t = Leaf t | Node nt [Tree nt t]

--type Grammar c t = (Set c, c, t -> Set c, Set (Production c))

type Production = (String, [String])

--data Nonterminal = S | WH | NP | NOM | VP | V | PP | VQ
--data Terminal = Aux | Verb | P | N | Adv | Adj | Wh | Det

--data S = NP VP | VP | VQ NP | VQ NP VP | VQ NP VP PP | WH Aux NP | WH Aux NP VP | WH NP VP | WH NP Aux NP VP

--data WH =  Wh | Wh Det

--data NP = NOM | Det NOM

--data NOM = N | N NOM | Pro | Pro NOM | Adj NOM

--data VP = V | V NP | V NP PP | VP PP | VP Adv | Adv VP | V To VP

--data V = Verb | Aux

--data PP = P NP


type State = (Production, Int, Int)