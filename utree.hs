module UTree where
import Pretty

-- Binary tree with a synthesized (upwards propagating) attribute of type d.
-- Navigation and modification with the zipper keeps the attribute up-to-date.


-- data of type a
-- attribute of type d
data Tree a d = Leaf d a
              | Node d (Tree a d) (Tree a d)

-- Context holds derived attribute as long as no child was modified.
-- Upon modification `prune' is used to drop the attribute, as it needs to
-- be recalculated all the path up (in a pessimistic setting).
data Ctx a d = Top
             | L    (Ctx a d) (Tree a d)
             | LD d (Ctx a d) (Tree a d)
             | R    (Tree a d) (Ctx a d)
             | RD d (Tree a d) (Ctx a d)


-- `FS a d' contains two functions
-- the first (a->d) applied to the leafs, like a map
-- the second (d->d->d) folds the results pairwise up to the tree top
type FS a d = (a -> d, d -> d -> d)

type Loc a d = (Tree a d, Ctx a d, FS a d)


updatingZipperOnList :: [a] -> FS a d -> Loc a d
updatingZipperOnList xs fs = (enter fs xs, Top, fs)

enter (dmap, dfold) = rec
      where rec [x] = Leaf (dmap x) x
            rec xs = let (l, r) = splitList xs
                         (el, er) = (rec l, rec r)
                         a = dfold (attr el) (attr er)
                     in Node a el er

attr (Leaf d _) = d
attr (Node d _ _) = d

splitList [] = error "can't split empty list"
splitList xs = splitAt (length xs `div` 2) xs

tree :: Loc a d -> Tree a d
tree l = let (t, _, _) = upmost l in t

left, right, up, upmost :: Loc a d -> Loc a d
left  (Node d l r, c, fs) = (l, LD d c r, fs)
right (Node d l r, c, fs) = (r, RD d l c, fs)

up (t, p, fs) = case p of
                  L c r -> (mknode fs t r, c, fs)
                  R l c -> (mknode fs l t, c, fs)
                  -- keep attribute of non-modified subtree
                  LD d c r -> (Node d t r, c, fs)
                  RD d l c -> (Node d l t, c, fs)

mknode (_, dfold) l r = Node (dfold (attr l) (attr r)) l r

upmost l@(_, Top, _) = l
upmost l = upmost (up l)

put :: Loc a d -> a -> Loc a d
put (_, c, fs@(dmap, _)) x = (Leaf (dmap x) x, prune c, fs)

-- replace current item with a list of elements
-- putList :: Loc a d -> [a] -> Loc a d

-- remove all attributes
prune (LD _ c r) = L (prune c) r
prune (RD _ l c) = R l (prune c)
prune Top = Top


instance (Pretty a, Pretty d) => Pretty (Tree a d) where
  pretty (Leaf d a) = sep [text "Leaf", pretty d, pretty a]
  pretty (Node d l r) = sep [ sep [text "Node", nest 8 $ pretty d]
                            , nest 2 $ sep [ parens $ pretty l
                                           , parens $ pretty r]]

