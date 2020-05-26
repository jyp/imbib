module Diff where

-- A diff algorithm adapted from a edit-distance algorithm.

import Data.List
import Data.Function
data Source = L | R | B
              deriving (Show, Eq)

type Choice a = (Source, a)

type Diff a = [Choice a]

weight0 :: Num t => Source -> t
weight0 B = 0
weight0 _ = 1

weight :: [(Source, b)] -> Int
weight = sum . map (weight0 . fst)

diff' :: Eq a => [a] -> [a] -> Diff a
diff' = diff (==) (const)

diff :: (a -> a -> Bool) -- ^ compare two values
     -> (a -> a -> a)    -- ^ merge two values in the same equivalent class
     -> [a] -> [a] -> Diff a
diff (=?=) (=+=) a b = 
    tail $ reverse $ 
             last (if lab == 0 then mainDiag
                   else if lab > 0 then lowers !! (lab - 1)
                        else{- < 0 -}   uppers !! (-1 - lab))
    where mainDiag = oneDiag L R (error "diff: head element should not be forced") a b (head uppers) ([] : head lowers)
          uppers = eachDiag L R a b (mainDiag : uppers) -- upper diagonals
          lowers = eachDiag R L b a (mainDiag : lowers) -- lower diagonals
          eachDiag centerSrc edgeSrc a [] diags = []
          eachDiag centerSrc edgeSrc a (bch0:bs) (lastDiag:diags) = oneDiag centerSrc edgeSrc bch0 a bs nextDiag lastDiag 
                                                                    : eachDiag centerSrc edgeSrc a bs diags
              where nextDiag = head (tail diags)
          oneDiag centerSrc edgeSrc f a b diagAbove diagBelow = thisdiag
              where doDiag [] b nw n w = []
                    doDiag a [] nw n w = []
                    doDiag (ach:as) (bch:bs) nw n w = me : (doDiag as bs me (tail n) (tail w))
                        where me = if ach =?= bch then (B,ach =+= bch) : nw
                                                  else minBy weight ((edgeSrc,bch) : head w) ((centerSrc,ach) : head n)
                    firstelt = (edgeSrc,f) : head diagBelow
                    thisdiag = firstelt : doDiag a b firstelt diagAbove (tail diagBelow)
          lab = length a - length b

minBy :: Ord a => (t -> a) -> t -> t -> t
minBy f x y = if (f x) < (f y) then x else y

simplify :: Diff a -> Diff [a]
simplify = map simplOne . groupBy ((==) `on` fst)
           where simplOne l@((src,_):_) = (src,map snd l)

diffS :: Eq a => [a] -> [a] -> Diff a
diffS = diff (==) (const)

-- >>> simplify (diffS "test me" "tes me")
-- [(B,"tes"),(L,"t"),(B," me")]


