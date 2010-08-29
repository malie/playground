{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternGuards #-}
module Memo1.Tests where

import Control.Monad (liftM, liftM2)
import Data.List(sort)
import Debug.Trace
import Memo1
import System.Environment(getArgs)
import Test.QuickCheck

main = do
  args <- getArgs
  let n = case args of 
            x:_ -> read x
            _ -> 100
  let s = (stdArgs { maxSuccess = n })
  mapM_ ($s) tests

qc t args = quickCheckWith args t

tests =
    [ qc testComposition1
    , qc testComposition2
    , qc $ eq1 apply1 elems
    , qc $ eq12 apply1 reapply2
    , qc (\x-> let a = x ++ [123] in tsortApply a == sort a)
    , qc tsortReapply
    ]

eq1 f g x = f x == g x
eq2 f g x y = f x y == g x y
eq12 f g x y = f y == g x y

elems a = map (\x->[x]) a

apply1 :: String -> [String]
apply1 x = result $ apply (Unfold halves) x

reapply2 :: String -> String -> [String]
reapply2 x y = let c = apply (Unfold halves) x
                   cc = reapply c y
               in result cc


halves :: (Show a) => [a] -> [[a]]
-- halves x | trace ("halves " ++ show x) False = undefined
halves [] = []
halves [c] = [[c]]
halves str = let (a,b) = splitAt (length str `div` 2) str
                 in [a,b]



cf :: Int -> (Int, Int)
cf x = (2*x-1, x+1)
cg (x,y) = (x, x+y, x+2*y+7)

testComposition1 x =
    let r = result $ apply (Comp cg cf) x
    in r == cg (cf x)

testComposition2 :: Int -> [Int] -> Bool
testComposition2 x ys =
    let c = apply (Comp cg cf) x
        cn = foldl (\cc a-> reapply cc a) c ys
        arg = head (reverse ys ++ [x])
    in result cn == cg (cf arg)




tsortApply :: [Integer] -> [Integer]
-- tsortApply xs | trace ("tsortApply " ++ show xs) False = undefined
tsortApply xs = 
    let mer = if length xs > 10 then bmerge else merge
    in flatten $ result $ apply (Fold2 mer) (elems xs)

flatten [a] = a
flatten [] = []
flatten _ = undefined

-- some arbitrary case is refused. testcase data generation ensures element 1 has right neigbours,
-- with which it can be later merged
bmerge (_, [1]) = -- trace ("bmerge refused " ++ show xs ++ " " ++ show [1])
                      Nothing
bmerge x = merge x


merge :: (Num a, Ord a, Show a) => ([a], [a]) -> Maybe [a]
--merge (xs, ys) | trace ("merge " ++ show xs ++ " " ++ show ys) False = undefined
merge (xs, ys) = Just (rec xs ys)
    where rec [] ys' = ys'
          rec xs' [] = xs'
          rec (x:xs') (y:ys') = case compare x y of
                                  LT -> x : rec xs' (y:ys')
                                  EQ -> x : y : rec xs' ys'
                                  GT -> y : rec (x:xs') ys'


-- tests: replace some elements, insert elements, delete elements
--        combinations of the above

data Edit a
    = Insert Int [a]
    | Delete Int Int
      deriving (Show, Eq)
pos (Insert p _) = p
pos (Delete p _) = p
lenmod (Insert _ ns) = length ns
lenmod (Delete _ n) = (-n)
applyEdit xs (Insert x _) | x < 0 = undefined
applyEdit xs (Insert p ns) = let (as,zs) = splitAt p xs
                                        in as ++ ns ++ zs
applyEdit xs (Delete p n) = let (as,zs) = splitAt p xs
                            in as ++ (drop n zs)

positiveArbitrary = arbitrary `suchThat` (>0)

instance Arbitrary a => Arbitrary (Edit a) where
    arbitrary = frequency [(1, liftM2 Insert positiveArbitrary arbitrary)
                          ,(1, liftM2 Delete positiveArbitrary positiveArbitrary)]
    shrink (Insert p ns) = [ Insert p ns' | ns' <- shrink ns]
    shrink (Delete p n) = [ Delete p n' | n' <- shrink n]


data TsortReapply a = TsortReapply [a] [Edit a]
                    deriving (Show, Eq)
numEdits (TsortReapply _ eds) = length eds

instance (Arbitrary a, Show a) => Arbitrary (TsortReapply a) where
    arbitrary = do Positive len <- liftM (`mod` 300) arbitrary
                   str <- mapM (const arbitrary) [1::Int .. len]
                   Positive nEds <- liftM (`mod` 10) arbitrary
                   eds <- mapM (const arbitrary) [1::Int .. nEds] -- `suchThat` validEds len
                   return $ TsortReapply str eds
    shrink (TsortReapply xs eds) = [ TsortReapply xs eds' | eds' <- shrink eds ]
                                   ++ [ TsortReapply xs' eds | xs' <- shrink xs ]

validEds len (NonEmpty eds) | trace ("validEds " ++ show len ++ " " ++ show eds)  False = undefined
validEds len (NonEmpty eds) = snd $ foldl ck (len, True) eds
    where ck (l, ok) ed | p <- pos ed = (l + lenmod ed, ok && p >= 0 && p <= l)

-- tmerge i@(_,_) | trace (" x " ++ show i) False = undefined
tmerge x = merge x

tsortReapply :: TsortReapply Int -> Property
-- tsortReapply t | trace ("\ntsortReapply " ++ show t ++ "\n") False = undefined
tsortReapply t@(TsortReapply as eds) =
    let as' = take 300 $ foldl applyEdit as eds
    in collect (order as, order as', order eds) $
    let bs = apply (Fold2 tmerge) (elems as)
    in -- trace ("\n==================================================\nAPPLY " ++ show eds ++ " " ++ show as) $
       -- trace (show (result bs)) $
       let 
           cs = reapply bs (elems as')
       in -- trace "\n REAPPLY" $
          -- trace (show (result cs)) $
                  -- trace ("   " ++ show as ++ " <> " ++ show as') $
                  flatten (result cs) == sort as'

order :: [a] -> Integer
order = round . (log::Double->Double) . fromInteger . toInteger . length

{-

-}
