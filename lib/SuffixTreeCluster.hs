-----------------------------------------------------------------------------
-- |
-- Module      :  Data.SuffixTreeCluster
-- Copyright   :  (c) Bryan O'Sullivan 2007, JP Bernardy 2010
-- License     :  BSD-style
-- Maintainer  :  nobody
-- Stability   :  experimental
-- Portability :  portable
--
-- A suffix tree cluster implementation.
--
-- Since many function names (but not the type name) clash with
-- "Prelude" names, this module is usually imported @qualified@, e.g.
--
-- >  import Data.SuffixTreeCluster (STree)
-- >  import qualified Data.SuffixTreeCluster as T
--
-- This implementation constructs the suffix tree lazily, so subtrees
-- are not created until they are traversed.  
--
-- Estimates are given for performance.  The value /k/ is a constant;
-- /n/ is the length of a query string; and /t/ is the number of
-- elements (nodes and leaves) in a suffix tree.

module SuffixTreeCluster
    (
    -- * Types
      Alphabet
    , Edge
    , Prefix
    , STree(..)

    -- * Construction
    , construct

    -- * Querying
    , elem
    , findEdge
    , findTree
    , findPath

    -- * Other useful functions
    , mkPrefix
    , prefix
    , suffixes
    , select
    , commonStrings

    -- * Debug
    , printTree
    ) where
                   
import Prelude hiding (elem, foldl, foldr)
import qualified Data.Map as M
import Control.Arrow (second)
import qualified Data.ByteString as SB
import qualified Data.ByteString.Lazy as LB
import qualified Data.List as L
import Data.Maybe (listToMaybe, mapMaybe, catMaybes)
import Data.Monoid hiding (Sum)
-- import Text.Groom
import Data.Function (on)
import qualified Data.Tree as T

-- | The length of a prefix list.  This type is formulated to do cheap
-- work eagerly (to avoid constructing a pile of deferred thunks),
-- while deferring potentially expensive work (computing the length of
-- a list).
data Length a = Exactly {-# UNPACK #-} !Int
              | Sum {-# UNPACK #-} !Int [a]
              deriving (Show)

-- | The list of symbols that 'constructWith' can possibly see in its
-- input.
type Alphabet a = [a]

-- | The prefix string associated with an 'Edge'.  Use 'mkPrefix' to
-- create a value of this type, and 'prefix' to deconstruct one.
newtype Prefix a = Prefix ([a], Length a)

instance (Eq a) => Eq (Prefix a) where
    a == b = prefix a == prefix b

instance (Ord a) => Ord (Prefix a) where
    compare a b = compare (prefix a) (prefix b)

instance (Show a) => Show (Prefix a) where
    show a = "mkPrefix " ++ show (prefix a)

-- | An edge in the suffix tree.
type Edge a b = (Prefix a, STree a b)

-- | /O(1)/. Construct a 'Prefix' value.
mkPrefix :: [a] -> Prefix a
mkPrefix xs = Prefix (xs, Sum 0 xs)

pmap :: (a -> b) -> Prefix a -> Prefix b
pmap f = mkPrefix . map f . prefix

instance Functor Prefix where
    fmap = pmap

-- | The suffix tree type.  The implementation is exposed to ease the
-- development of custom traversal functions.  Note that @('Prefix' a,
-- 'STree' a b)@ pairs are not stored in any order.
data STree a b = Node b [Edge a b]
               deriving (Show)

smap :: (a -> b) -> STree a c -> STree b c
smap f (Node b es) = Node b (map (\(p, t) -> (fmap f p, smap f t)) es)

lmap :: (a -> b) -> STree c a -> STree c b
lmap f (Node b es) = Node (f b) (map (second (lmap f)) es)

instance Functor (STree a) where
    fmap = lmap

-- | /O(n)/. Obtain the list stored in a 'Prefix'.
prefix :: Prefix a -> [a]
prefix (Prefix (ys, Exactly n)) = take n ys
prefix (Prefix (ys, Sum n xs)) = tk n ys
    where tk 0 ys = zipWith (const id) xs ys
          tk n (y:ys) = y : tk (n-1) ys



-- | Increments the length of a prefix.
inc :: Length a -> Length a
inc (Exactly n) = Exactly (n+1)
inc (Sum n xs)  = Sum (n+1) xs

-- | /O(n)/. Returns all non-empty suffixes of the argument, longest
-- first.  Behaves as follows:
--
-- >suffixes xs == init (tails xs)
suffixes :: [a] -> [[a]]
suffixes (xs@(_:xs')) = (xs) : suffixes (xs')
suffixes _ = []

constr :: (Eq b, Monoid b, Ord a) => [(b,[[a]])] -> STree a b
constr is = Node label (suf xs)
    where xs = filter (not . null . snd) $ map (second (filter (not . null))) is
          label = (mconcat $ map fst is)

construct :: (Eq b, Monoid b, Ord a) => [([a],b)] -> STree a b
construct is = constr [(b,L.tails a) | (a,b) <- is]

suf :: (Eq b, Monoid b, Ord a) => [(b,[[a]])] -> [Edge a b]
suf ss = [(Prefix (a:sa, inc cpl), constr ssr)
              | (a, n@((_,sa:_):_)) <- suffixMap ss,
              let (cpl,ssr) = cst n]

regroup :: (Eq b) => [(b, a)] -> [(b, [a])]
regroup xs = [(b,map snd as) | as@((b,_):_) <- L.groupBy ((==) `on` fst) xs]

-- 

suffixMap :: (Eq b, Ord a) => [(b,[[a]])] -> [(a, [(b, [[a]])])]
suffixMap sss = map (second (regroup . reverse)) .
                M.toList . L.foldl' step M.empty $ [(b,s) | (b,ss) <- sss, s <- ss]
    where step m (b,x:xs) = M.alter (f (b,xs)) x m
          step m _ = m
          f x Nothing = Just [x]
          f x (Just xs) = Just (x:xs)

cst :: (Monoid b, Eq b, Eq a) => [(b, [[a]])] -> (Length a, [(b, [[a]])])

cst bss | or [null s | (_,ss) <- bss, s <- ss] = (Exactly 0, bss)
cst ss0@((_,[s]):ss) | all ((== [s]) . snd) ss = (Sum 0 s, [(b, []) | (b,_) <- ss0])
cst awss@((_,(a:w):_):ss)
    | null [c | (_,x) <- ss, (c:_) <- x, a /= c] 
        = let cpl' = inc cpl
          in seq cpl' (cpl', rss)
    | otherwise = (Exactly 0, awss)
    where (cpl, rss) = cst $ map (second (map tail')) awss
          tail' (_:t) = t -- so we see the pattern fail here instead of a generic tail.


suffix :: (Eq a) => [a] -> [a] -> Maybe [a]
suffix (p:ps) (x:xs) | p == x = suffix ps xs
                     | otherwise = Nothing
suffix _ xs = Just xs

{-# SPECIALISE elem :: [Char] -> STree Char b -> Bool #-}
{-# SPECIALISE elem :: [[Char]] -> STree [Char] b -> Bool #-}
{-# SPECIALISE elem :: [SB.ByteString] -> STree SB.ByteString b -> Bool #-}
{-# SPECIALISE elem :: [LB.ByteString] -> STree LB.ByteString b -> Bool #-}
{-# SPECIALISE elem :: (Eq a) => [[a]] -> STree [a] b -> Bool #-}

-- | /O(n)/.  Indicates whether the suffix tree contains the given
-- sublist.  Performance is linear in the length /n/ of the
-- sublist.
elem :: (Eq a) => [a] -> STree a b -> Bool
elem [] _ = True
elem xs (Node mb es) = any pfx es
    where pfx (e, t) = maybe False (`elem` t) (suffix (prefix e) xs)

lkup :: (Monoid b, Eq a) => [a] -> STree a b -> b
lkup [] (Node b es) = b
lkup xs (Node _ es) = mconcat $ map pfx es
    where pfx (e, t) = maybe mempty (`lkup` t) (suffix (prefix e) xs)

{-# SPECIALISE findEdge :: [Char] -> STree Char b
                        -> Maybe (Edge Char b, Int) #-}
{-# SPECIALISE findEdge :: [String] -> STree String b
                        -> Maybe (Edge String b, Int) #-}
{-# SPECIALISE findEdge :: [SB.ByteString] -> STree SB.ByteString b
                        -> Maybe (Edge SB.ByteString b, Int) #-}
{-# SPECIALISE findEdge :: [ LB.ByteString] -> STree LB.ByteString b
                        -> Maybe (Edge LB.ByteString b, Int) #-}
{-# SPECIALISE findEdge :: (Eq a) => [[a]] -> STree [a] b
                        -> Maybe (Edge [a] b, Int) #-}

-- | /O(n)/.  Finds the given subsequence in the suffix tree.  On
-- failure, returns 'Nothing'.  On success, returns the 'Edge' in the
-- suffix tree at which the subsequence ends, along with the number of
-- elements to drop from the prefix of the 'Edge' to get the \"real\"
-- remaining prefix.
--
-- Here is an example:
--
-- >> find "ssip" (construct "mississippi")
-- >Just ((mkPrefix "ppi",Leaf),1)
--
-- This indicates that the edge @('mkPrefix' \"ppi\",'Leaf')@ matches,
-- and that we must strip 1 character from the string @\"ppi\"@ to get
-- the remaining prefix string @\"pi\"@.
--
-- Performance is linear in the length /n/ of the query list.
findEdge :: (Eq a) => [a] -> STree a b -> Maybe (Edge a b, Int)
findEdge xs (Node mb es) = listToMaybe (mapMaybe pfx es)
    where pfx e@(p, t) = let p' = prefix p
                         in suffix p' xs >>= \suf ->
            case suf of
              [] -> return (e, length (zipWith const xs p'))
              s -> findEdge s t

-- | /O(n)/. Finds the subtree rooted at the end of the given query
-- sequence.  On failure, returns 'Nothing'.
--
-- Performance is linear in the length /n/ of the query list.
findTree :: (Eq a) => [a] -> STree a b -> Maybe (STree a b)
findTree s t = (snd . fst) `fmap` findEdge s t

-- | /O(n)/. Returns the path from the 'Edge' in the suffix tree at
-- which the given subsequence ends, up to the root of the tree.  If
-- the subsequence is not found, returns the empty list.
--
-- Performance is linear in the length of the query list.
findPath :: (Eq a) => [a] -> STree a b -> [Edge a b]
findPath = go []
    where go me xs (Node mb es) = pfx me es
              where pfx _ [] = []
                    pfx me (e@(p, t):es) =
                        case suffix (prefix p) xs of
                          Nothing -> pfx me es
                          Just [] -> e:me
                          Just s -> go (e:me) s t


-- select :: SC.STree a b -> Tree Int
selectPairs (p0,Node entries sub) = T.Node (p0,entries) $
              [select (p0 ++ prefix p,n) | (p,n) <- sub, isRelevantNode n]

selectLen (T.Node (shared,entries) sub) = T.Node here (map selectLen sub)
   where here | length shared < 7 = ([],mempty) 
              | otherwise = (shared,entries)


selectLongest (T.Node (shared,entries) sub) = T.Node here (map selectLongest sub)
   where here | subsumed = ([],[]) 
              | otherwise = (shared,entries)
         subsumed = all (`L.elem` (concatMap (snd . T.rootLabel) sub)) entries 
    

select :: Eq b => ([a],STree a [b]) -> T.Tree ([a],[b])
select = selectLongest . selectLen . selectPairs

buildMap :: (Eq a, Ord b) => T.Tree ([a],[b]) -> M.Map [b] [[a]]
buildMap t = L.foldl' step M.empty $ filter (not . null . snd) $ T.flatten t
    where step m (x,bs) = M.alter (lub x) bs m

commonStrings :: (Eq b, Ord a, Ord b) => [([a],[b])] -> M.Map [b] [[a]]
commonStrings xs = buildMap $ select ([],construct xs)

lub y Nothing = Just [y]
lub y (Just xs)
    | (y `L.isSuffixOf`) `any` xs = Just xs
    | otherwise = Just (y : filter (not . (`L.isSuffixOf` y)) xs)

printTree :: Show a => T.Tree a -> IO ()
printTree = putStrLn . T.drawTree . fmap show

isRelevant [] = False
isRelevant [_] = False
isRelevant _ = True

isRelevantNode (Node e _) = isRelevant e

emptyPrefix :: Prefix a
emptyPrefix = mkPrefix []