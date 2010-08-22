{-# LANGUAGE FlexibleInstances #-}
module Memo1.Tests where

-- import Data.List(sort)
import Debug.Trace
import Memo1
import System.Environment(getArgs)
import Test.QuickCheck

testsMain = do
  args <- getArgs
  let n = case args of 
            x:_ -> read x
            _ -> 100
  let s = (stdArgs { maxSuccess = n })
  mapM_ ($s) tests

qc t args = quickCheckWith args t

tests =
    [ qc testComposition1,
      qc testComposition2,
      -- qc $ eq1 tsortApply sort,
      qc $ eq12 apply1 reapply2
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



{-
tsortApply :: [Integer] -> [Integer]
tsortApply xs = head $ fst $ apply (Fold2 merge) (elems xs)

merge :: (Ord a, Show a) => [a] -> [a] -> Maybe [a]
merge (xs, ys) | trace ("merge " ++ show xs ++ " " ++ show ys) False = undefined
merge (xs, ys) = Just (rec xs ys)
    where rec (x:xs) (y:ys) = case compare x y of
                                LT -> x : rec xs (y:ys)
                                EQ -> x : y : rec xs ys
                                GT -> y : rec (x:xs) ys
-}
