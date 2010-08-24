{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}

{- Markus Liedl 2010-08-16 -}
module Memo1 where

import Data.List(isPrefixOf, isSuffixOf)
import Debug.Trace

class Memo1 f where
    type Arg f
    type Res f
    data Cache f
    apply :: f -> Arg f -> Cache f
    reapply :: Cache f -> Arg f -> Cache f
    argument :: Cache f -> Arg f
    result :: Cache f -> Res f
    changed :: (Eq (Res f)) => Cache f -> Cache f -> Bool
    changed a b = result a /= result b

instance (Eq a) => Memo1 (a->b) where
    type Arg (a->b) = a
    type Res (a->b) = b
    data Cache (a->b) = CacheFun (a->b) a b
    apply f a = let b = f a in CacheFun f a b
    reapply (CacheFun f a b) a' = if a == a'
                                  then CacheFun f a b
                                  else apply f a'
    argument (CacheFun _ a _) = a
    result (CacheFun _ _ b) = b

data Comp a b where
    Comp :: (Memo1 a, Memo1 b, Arg a ~ Res b)
            => a -> b -> Comp a b

instance (Memo1 a, Memo1 b, Eq (Arg a), Eq (Arg b), Arg a ~ Res b)
    => Memo1 (Comp a b) where
    type Arg (Comp a b) = Arg b
    type Res (Comp a b) = Res a
    data Cache (Comp a b) = CacheComp (Cache a) (Cache b)
    apply (Comp n m) x =
        let cm = apply m x
            cn = apply n (result cm)
        in CacheComp cn cm
    reapply cc@(CacheComp cn cm) x =
        if argument cm == x
        then cc
        else let cm' = reapply cm x
                 cn' = reapply cn (result cm')
             in CacheComp cn' cm'
    argument (CacheComp _ b) = argument b
    result (CacheComp a _) = result a

data Tree a = Leaf a
            | Node a [Tree a]
treeTop (Leaf a) = a
treeTop (Node a _) = a

leaves :: Tree a -> [a]
leaves (Leaf a) = [a]
leaves (Node _ as) = concatMap leaves as

data Unfold a = Unfold (a->[a])
-- empty list result makes no sense.
-- single list result means no-more-unfolding-possible.
instance (Eq a) => Memo1 (Unfold a) where
    type Arg (Unfold a) = a
    type Res (Unfold a) = [a]
    data Cache (Unfold a) = CU (a->[a]) (Tree (Cache (a->[a]))) [a]
    apply (Unfold f) a = let ra = rec a
                             cra = res ra
                         in CU f ra cra
        where rec aa = let c = apply f aa 
                       in case result c of
                            [_] -> Leaf c
                            bs  -> Node c (map rec bs)
              res = concatMap result . leaves

    reapply (CU f c r) a' =
        let ct = treeTop c
        in if argument ct == a'
           then CU f c r
           else let c' = xrec c a'
                in CU f c' (res c')
        where xrec (Leaf n) a'' = let e = reapply n a''
                                  in case result e of
                                       [_] -> Leaf e
                                       bs -> Node e (map rec bs)
              xrec (Node n ns) a'' = let e = reapply n a''
                                     in case result e of
                                          [_] -> Leaf e
                                          bs -> Node e (zipWithThen xrec rec ns bs)
              rec a = let cc = apply f a  -- same as above...
                      in case result cc of
                           [_] -> Leaf cc
                           bs -> Node cc (map rec bs)
              res = concatMap result . leaves
    argument (CU _ c _) = argument (treeTop c)
    result (CU _ _ r) = r

-- How can the code duplication in Unfold's reapply method be avoided?

zipWithThen f g as bs = rec as bs
    where rec (a:as') (b:bs') = f a b : rec as' bs'
          rec [] (b:bs') = g b : rec [] bs'
          rec _ [] = []


data Map a b = Map (a->b)
instance (Eq a) => Memo1 (Map a b) where
    type Arg (Map a b) = [a]
    type Res (Map a b) = [b]
    data Cache (Map a b) = CacheMap [Cache (a->b)]
    apply = undefined
    reapply = undefined
    argument = undefined
    result = undefined



-- If function returns Just, both inputs get replaced by the result. If Nothing
-- both inputs get reapplied to their other respective neighbours.
data Fold2 a = Fold2 ((a, a) -> Maybe a)
data F2 a = F2Node [a] [a] (F2 a, F2 a)
          | F2Leaf [a] [a]
instance (Eq a, Show a) => Memo1 (Fold2 a) where
    type Arg (Fold2 a) = [a]
    type Res (Fold2 a) = [a]
    data Cache (Fold2 a) = CacheFold2 ((a, a) -> Maybe a) (F2 a)
    apply (Fold2 f) as = CacheFold2 f (rec as)
        where rec as = case as of
                         [] -> F2Leaf [] []
                         [_] -> F2Leaf as as
                         _ -> let (l,r) = splitMid as
                                  (tl, tr) = (rec l, rec r)
                              in F2Node as (applr f (rs tl) (rs tr)) (tl, tr)
              rs (F2Node _ r _) = r
              rs (F2Leaf _ r) = r
    reapply (CacheFold2 f t) xs0 = CacheFold2 f (rre t xs0)
        where rre c [] = F2Leaf [] []
              rre c xs@[_] = F2Leaf xs xs
              rre c@(F2Leaf as rs) xs | as == xs = c
              rre (F2Leaf _ _) xs = rec xs
              rre _ xs | trace (show xs) False = undefined
              rre c@(F2Node as0 _ (l,r)) xs =
                  case (args l `isPrefixOf` xs, args r `isSuffixOf` xs) of
                    (False, False) -> editsOnBothSides l r xs
                    (True, False) -> noEditsLeft l r xs
                    (False, True) -> noEditsRight l r xs
                    (True, True) ->
                        let lxs = length xs
                            las0 = length as0
                            lar = length (args r)
                            lal = length (args l)
                        in case (compare lxs las0, (compare lal lar, lal >= lxs, lar >= lxs)) of
                             (EQ, _) -> trace "exactly same input" c -- exactly same input
                             (GT, _) -> -- new elements inserted exactly between the halves
                                        noEditsLeft l r xs -- arbitrarily keep left side
                             {-(LT, (GT, True, _)) -> trace "add new right" (noEditsLeft l r xs)
                             (LT, (LT, _, True)) -> trace "add new left" (noEditsRight l r xs)
                             (LT, (_, True, _)) -> trace "add new right" (noEditsLeft l r xs)
                             (LT, (_, _, True)) -> trace "add new left" (noEditsRight l r xs)-}
                             _ -> rec xs
              editsOnBothSides l r xs = let (xsl, xsr) = splitMid xs -- better split point possible?
                                            (tl, tr) = (rre l xsl, rre r xsr)
                                        in F2Node xs (applr f (rs tl) (rs tr)) (tl, tr)
              noEditsLeft l r xs = let (_, xsr) = splitAt (length $ args l) xs
                                       (tl, tr) = (l, rre r xsr)
                                   in F2Node xs (applr f (rs tl) (rs tr)) (tl, tr)
              noEditsRight l r xs = let (xsl, _) = splitAt (length xs - length (args r)) xs
                                        (tl, tr) = (rre l xsl, r)
                                   in F2Node xs (applr f (rs tl) (rs tr)) (tl, tr)

              args (F2Leaf as _) = as
              args (F2Node as _ _) = as
              rs (F2Node _ r _) = r
              rs (F2Leaf _ r) = r
              rec as = case as of -- REP!
                         [] -> F2Leaf [] []
                         [_] -> F2Leaf as as
                         _ -> let (l,r) = splitMid as
                                  (tl, tr) = (rec l, rec r)
                              in F2Node as (applr f (rs tl) (rs tr)) (tl, tr)

    argument (CacheFold2 _ (F2Node a _ _)) = a
    argument (CacheFold2 _ (F2Leaf a _)) = a
    result (CacheFold2 _ (F2Node _ r _)) = r
    result (CacheFold2 _ (F2Leaf _ r)) = r

-- apply f via a specialized list zipper
applr :: (Show a) => ((a, a) -> Maybe a) -> [a] -> [a] -> [a]
applr f as bs | trace ("  applr " ++ show as ++ " " ++ show bs) False = undefined
applr _ as [] = as
applr _ [] bs = bs
applr f as bs = let (x:ras) = reverse as
                in r ras x bs
    where l (a:as) x bs = case f (a, x) of
                            Just x' -> l as x' bs
                            Nothing -> r (a:as) x bs
          l [] x bs = r [] x bs
          r as x (b:bs) = case f (x, b) of
                            Just x' -> l as x' bs
                            Nothing -> reverse as ++ x : b : bs
          r as x [] = reverse as ++ [x]

splitMid :: [a] -> ([a], [a])
splitMid [] = ([], [])
splitMid [x] = ([x], [])
splitMid xs = let (a,b) = splitAt (length xs `div` 2) xs
                 in (a,b)


data Fold3 a = Fold3 (a -> a -> a -> Maybe a)
data Fold a = Fold ([a] -> [a])

data Shuffle a k b = Shuffle { sKey :: a -> Maybe k
                             , sNeeds :: a -> [k]
                             , sProject :: a -> b
                             , sProcess :: a -> [b] -> a
                             }

{-
reapply' :: ... -> (res, Bool {-hasChanged-}, cache)



noch von der data trennen:
  o decomposition
  o test auf equal (hasChangedFrom??)
    dann koennen beliebige datentypen eigene timestamps mitkriegen


further instances of Memo1
 fold     [a] -> [a]
 map      [[a]] -> [[b]]
 foldz    zipper a -> b
 fix      a -> Maybe a    // apply till no more changes

examples of incremental computation:
  incremental sorting
  sum of a list of numbers given as a space separated string
  parse and type check a simple language (literal numbers, arithmetic, variables, assignment)
  strictnes analysis ???
  balanced binary tree construction

open:
  rename vars, need to thread state, the next id, through all computations
  the id's needn't be consecutive though
-}
