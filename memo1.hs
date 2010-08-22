{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

{- Markus Liedl 2010-08-16 -}

module Memo1 where

import Debug.Trace

class Memo1 f where
    type Arg f
    type Res f
    data Cache f
    apply :: f -> Arg f -> (Res f, Cache f)
    reapply :: Cache f -> Arg f -> (Res f, Cache f)
    argument :: Cache f -> Arg f
    result :: Cache f -> Res f
    changed :: (Eq (Res f)) => Cache f -> Cache f -> Bool
    changed a b = result a /= result b

instance (Eq a) => Memo1 (a->b) where
    type Arg (a->b) = a
    type Res (a->b) = b
    data Cache (a->b) = CacheFun (a->b) a b
    apply f a = let b = f a in (b, CacheFun f a b)
    reapply (CacheFun f a b) a' = if a == a'
                                  then (b, CacheFun f a b)
                                  else apply f a'
    argument (CacheFun _ a _) = a
    result (CacheFun _ _ b) = b

data Comp a b c = Comp (b->c) (a->b) 
instance (Eq a, Eq b) => Memo1 (Comp a b c) where
    type Arg (Comp a b c) = a
    type Res (Comp a b c) = c
    data Cache (Comp a b c) = CacheComp (Cache (b->c)) (Cache (a->b))
    apply (Comp f g) a =
        let (b, gw) = apply g a
            (c, fw) = apply f b
        in (c, CacheComp fw gw)
    reapply cc@(CacheComp fw gw) a' =
        if argument gw == a'
        then (result fw, cc)
        else let (b, gw') = reapply gw a'
                 (c, fw') = reapply fw b
             in (c, CacheComp fw gw)
    argument (CacheComp _ ab) = argument ab
    result (CacheComp bc _) = result bc


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
                         in (cra, CU f ra cra)
        where rec a = case apply f a of
                        ([_], c) -> Leaf c
                        (bs, c) -> Node c (map rec bs)
              res = concatMap result . leaves

    reapply (CU f c r) a' =
        let ct = treeTop c
        in if argument ct == a'
           then (r, CU f c r)
           else let c' = xrec c a'
                    rc' = res c'
                in (rc', CU f c' rc')
        where xrec (Leaf n) a' = case reapply n a' of
                                   ([_], e) -> Leaf e
                                   (bs, e) -> Node e (map rec bs)
              xrec (Node n ns) a' = case reapply n a' of
                                      ([_], e) -> Leaf e
                                      (bs, e) -> Node e (zipWithThen xrec rec ns bs)
              rec a = case apply f a of  -- same as above...
                        ([_], c) -> Leaf c
                        (bs, c) -> Node c (map rec bs)
              res = concatMap result . leaves
    argument (CU _ c _) = argument (treeTop c)
    result (CU _ _ r) = r

-- How can the code duplication in Unfold's reapply method be avoided?

zipWithThen f g as bs = rec as bs
    where rec (a:as) (b:bs) = f a b : rec as bs
          rec [] (b:bs) = g b : rec [] bs
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
{-
data Fold2 a = Fold2 ((a, a) -> Maybe a)
data F2 a = F2 [a] [a] [F2 a]
instance (Eq a) => Memo1 (Fold2 a) where
    type Arg (Fold2 a) = [a]
    type Res (Fold2 a) = [a]
    data Cache (Fold2 a) = CacheFold2 (F2 a)
    apply (Fold2 f) as = rec as
        where rec as = let (res, subs) =
                               case as of
                                 [] -> ([], [])
                                 [a] -> ([a], [])
                                 _ -> let (l,r) = halves as
                                          (tl, tr) = (rec l, rec r)
                                      in ( applr f (results tl) (results tr)
                                         , [tl, tr] )
                  CacheFold2 (F2 as res subs)
              results (F2 _ r _) = r

        -- ganze liste als root des trees
        -- beim reapply testen, ob alles gleich, wenn ja, cache result
        -- wenn nicht, zwei teile bilden, nur die geaenderten teile neu bearbeiten

    reapply = undefined
    argument (CacheFold2 (F2 a _ _)) = a
    result (CacheFold2 (F2 _ r _)) = r

-- apply f via a specialized list zipper
applr :: (a -> a -> Maybe a) -> [a] -> [a] -> [a]
applr f as bs = let (x:ras) = reverse as
                in r1 ras x bs
    where l (a:as) x bs = case f a x of
                            Just x' -> l as x' bs
                            Nothing -> r (a:as) x bs
          l [] x bs = r [] x bs
          l1 (a:as) x bs = case f a x of
                             Just x' -> l as x' bs
                             Nothing -> reverse as ++ a : x : bs
          l1 [] x bs = r [] x bs
          r as x (b:bs) = case f x b of
                            Just x' -> l as x' bs
                            Nothing -> reverse as ++ x : b : bs
          r as x [] = reverse as ++ [x]
          r1 as x (b:bs) = case f x b of
                             Just x' -> l as x' bs
                             Nothing -> reverse as ++ x : b : bs
          r1 as x [] = reverse as ++ [x]

halves :: [a] -> ([a], [a])
halves [] = ([], [])
halves [x] = ([x], [])
halves xs = let (a,b) = splitAt (length xs `div` 2) xs
                 in (a,b)
-}


data Fold3 a = Fold3 (a -> a -> a -> Maybe a)
data Fold a = Fold ([a] -> [a])

data Shuffle a k b = Shuffle { sKey :: a -> Maybe k
                             , sNeeds :: a -> [k]
                             , sProject :: a -> b
                             , sProcess :: a -> [b] -> a
                             }

{-
noch von der data trennen:
  o decomposition
  o test auf equal (hasChangedFrom??)
    dann koennen beliebige datentypen eigene timestamps mitkriegen
-}

{- further instances of Memo1
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
