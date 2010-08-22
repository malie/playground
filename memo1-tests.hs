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
  -- mapM_ (runTest (stdArgs { maxSuccess = n })) tests1
  mapM_ (runTest (stdArgs { maxSuccess = n })) tests2
runTest args = quickCheckWith args

tests1 =
    [ 
      -- eq1 tsortApply sort
    ]

tests2 =
    [
      eq12 apply1 reapply2
    ]

eq1 f g x = f x == g x
eq2 f g x y = f x y == g x y
eq12 f g x y = f y == g x y

elems a = map (\x->[x]) a

apply1 :: String -> [String]
apply1 x = fst $ apply (Unfold halves) x

reapply2 :: String -> String -> [String]
reapply2 x y = let (r, c) = apply (Unfold halves) x
                   (rr, cc) = reapply c y
               in rr


halves :: (Show a) => [a] -> [[a]]
-- halves x | trace ("halves " ++ show x) False = undefined
halves [] = []
halves [c] = [[c]]
halves str = let (a,b) = splitAt (length str `div` 2) str
                 in [a,b]



mul3 :: Int -> Int
mul3 x = trace ("*3 " ++ show x) (x*3)
mul7mod10 x = trace ("*7m10 " ++ show x) $ x*7 `mod` 10

t1 = 
    let (r1, c) = apply (Comp mul3 mul7mod10) 7
        (r2, _) = reapply c 8
        (r3, _) = reapply c 9
        (r4, _) = reapply c 7
        (r5, _) = reapply c 17
    in print [r1, r2, r3, r4, r5]


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
