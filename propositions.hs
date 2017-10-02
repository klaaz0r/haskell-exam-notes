import Data.Maybe
import Data.List (nub)

data Prop = Basic Bool
          | Var Char
          | Not Prop
          | Prop :/\: Prop
          | Prop :\/: Prop
          | Prop :=>: Prop
          deriving (Show)

type Map k v = [(k,v)]

tv :: Map Char Bool -> Prop -> Bool
tv _ (Basic b)    = b
tv m (Var v)      = fromJust (lookup v m)
tv m (Not p)      = not (tv m p)
tv m (p1 :/\: p2) = tv m p1 && tv m p2
tv m (p1 :\/: p2) = tv m p1 || tv m p2
tv m (p1 :=>: p2) = not (tv m p1) || tv m p2

vars :: Prop -> [Char]
vars = nub . vars'
  where vars' (Basic b)   = []
        vars' (Var v)      = [v]
        vars' (Not p)      = vars p
        vars' (p1 :/\: p2) = vars p1 ++ vars p2
        vars' (p1 :\/: p2) = vars p1 ++ vars p2
        vars' (p1 :=>: p2) = vars p1 ++ vars p2

assigns :: [Char] -> [Map Char Bool]
assigns []     = [[]] -- remember to keep the structure
-- make a list of all possible True and False values
assigns (v:vs) = [(v, True) : as | as <- assigns vs] ++
                  [(v, False) : as | as <- assigns vs]

taut :: Prop -> Bool
-- get the vars from a prop and make the assigns
-- the use tv on that
taut p = and [tv as p | as <- assigns (vars p)]
