{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
import Data.Generics.Uniplate.Data
import Data.List
import qualified Data.Map as M
import Data.Maybe ( mapMaybe )
import Debug.Trace

import Pretty
import UTree

type Rulename = String
data Parser
     = Def Rulename Parser
     | Seq [Parser]
     | Seq2 Parser Parser
     | Alt [Parser]
     | Ch Char
     | Range Char Char
     | Rep Parser
     | Opt Parser
     | Ref Rulename
     | Not Parser -- ?
     | Wordboundary -- ?
     | Empty -- ?
     deriving (Eq, Ord, Typeable, Data)

instance Pretty Parser where
  pretty (Def n p)    = sep [text "Def", text n, pretty p]
  pretty (Seq xs)     = sep [text "Seq", pretty xs]
  pretty (Seq2 a b)   = sep [text "Seq2", parens $ pretty a, parens $ pretty b]
  pretty (Alt xs)     = sep [text "Alt", pretty xs]
  pretty (Ch ch)      = sep [text "Ch", pretty ch]
  pretty (Range a b)  = sep [text "Range", pretty a, pretty b]
  pretty (Rep p)      = sep [text "Rep", pretty p]
  pretty (Opt p)      = sep [text "Opt", pretty p]
  pretty (Ref n)      = text n
         

gram1 =
  [ Def "upper"      (Range 'A' 'Z')
  , Def "lower"      (Range 'a' 'z')
  , Def "digit"      (Range '0' '9')
  , Def "integer"    (Rep (Ref "digit"))
  , Def "wordchar"   (Alt [Ref "digit", Ref "lower", Ref "upper"])
  , Def "space"      (Ch ' ')
  , Def "optspaces"  (Opt (Rep (Ref "space")))
  , Def "spaces"     (Seq [Ref "space", Ref "optspaces"])
  , Def "var"        (Seq [Ref "upper", Opt (Rep (Ref "wordchar"))])
  , Def "symbol"     (Seq [Ref "lower", Opt (Rep (Ref "wordchar"))])
  , Def "element"    (Alt [Ref "integer", Ref "var", Ref "symbol"])
  , Def "list"       (Rep (Seq [Ref "element", Ref "spaces"]))
  -- , Def "seqtest"    (Seq [Ch 'a', Ch 'b', Ch 'c', Ch 'd'])
  , Def "entry"      (Ref "list")
  ]


data RewriteRule
  = R1 Parser Parser
  | R2 Parser Parser Parser
  deriving (Eq, Ord)

instance Pretty RewriteRule where
  pretty (R1 a b) = sep [pretty a, nest 2 $ sep [ text "==>", pretty b]]
  pretty (R2 a b c) = sep [sep [pretty a, nest 2 $ pretty b],
                           nest 2 $ sep [text "==>", pretty c]]


simplify = transformBi f
  where f (Seq [x]) = x
        f x         = x

-- alt [ opt ... ] unimplemented ...

preprocessSeqOpts :: [Parser] -> [Parser]
preprocessSeqOpts = simplify . ppSeqOpts . inlineOpts

-- inline all productions that are optional
inlineOpts :: [Parser] -> [Parser]
inlineOpts gram =
  let m = M.fromList [ (nm, p) | Def nm p@(Opt _) <- gram ]
      f r@(Ref nm) = M.lookup nm m
      f _ = Nothing
      optdef (Def nm _) = M.member nm m
   in rewriteBi f (filter (not . optdef) gram)

ppSeqOpts :: [Parser] -> [Parser]
ppSeqOpts = transformBi f
  where f (Seq xs) = Alt $ map Seq (vari xs)
        f x        = x

{- produce "variations".
vari [Ch 'a', Opt (Ch 'b'), Ch 'c', Opt (Ch 'd')]
=> [[Ch 'a', Ch 'c'],
    [Ch 'a', Ch 'c', Ch 'd'],
    [Ch 'a', Ch 'b', Ch 'c'],
    [Ch 'a', Ch 'b', Ch 'c', Ch 'd']]
-}
vari :: [Parser] -> [[Parser]]
vari c@(Opt o : xs) = let dxs = vari xs in dxs ++ map (o:) dxs
vari (x:xs) = map (x:) $ vari xs
vari [] = [[]]



altRules fr = concat [ [ R1 x a | x <- xs ] | a@(Alt xs) <- fr ]

seqToSeq2 = rewriteBi f
  where f (Seq [x]) = Just $ Seq [x]
        f (Seq [a,b]) = Just $ Seq2 a b
        f (Seq xs) = Just $ Seq (p xs)
        f _ = Nothing
        p (a:b:xs) = Seq2 a b : p xs
        p [] = []
        p [a] = [a]

seqRules fr = [ R2 a b s | s@(Seq2 a b) <- fr ]

defRules fr = [ R1 b (Ref n) | d@(Def n b) <- fr ]

repRules fr = concat [ [R1 a r, R2 r r r] | r@(Rep a) <- fr ]


{-
type Input = String

type PartialMatch = [Parser]
data Match = Match Input [PartialMatch]

instance Pretty Match where
  pretty (Match input pm) = sep [text "Match", quotes $ text input, pretty pm]

parse input0 rules = rep $ map (\i-> Match [i] [[Ch i]]) input0
    where rep ms0 =
            do let ms1 = map rew ms0
               pprint ms1
          rew (Match i pms) = Match i $
                                fix (\pms -> sort $ nub $ pms ++ concatMap appl pms) pms
          appl ps = map app ps
          app p = mapMaybe (matchRule p) rules

fix f x = let x' = f x
          in if x' /= x
             then fix f x'
             else x'

-}

{-
source text kept in a datastructure that allows
o fragmentation
o pointers to fragments

tree + zipper in tree.hs 
keeps parser derivations as tree annotations
navigation with zipper invalidates and recalculates annotations
-}

matchRule :: Parser -> RewriteRule -> Maybe Parser
-- matchRule p rr | trace (showPretty (p, rr)) False = undefined
matchRule _ (R2 _ _ _) = Nothing
matchRule x (R1 a b) = if x `matches` a
                       then Just b
                       else Nothing

matchRule2 :: Parser -> Parser -> RewriteRule -> Maybe Parser
matchRule2 _ _ (R1 _ _) = Nothing
matchRule2 a b (R2 c d e) = if matches a c && matches b d
                            then Just e
                            else Nothing

matches :: Parser -> Parser -> Bool
matches (Ch ch) (Range a b) = ch >= a && ch <= b
matches a pattern           = a == pattern


parse :: String -> [RewriteRule] -> IO ()
parse input rules =
  do let z = updatingZipperOnList input (fs rules)
     pprint $ tree z

-- ALTERNATIVES

data Match
  = MP Parser
  | MSeq [Match]
  | MSeqr [Match]
  | MAlt [Match]
  | MNo

{-
data Match
  = MP Parser
  | MSeq Match Match
  | MAlt Match Match
  | MNo
any better?
-}

instance Pretty Match where
  pretty (MP p)     = sep [text "MP", pretty p]
  pretty (MSeq ms)  = sep [text "MSeq", nest 2 $ pretty ms]
  pretty (MSeqr ms) = sep [text "MSeqr", nest 2 $ pretty ms]
  pretty (MAlt ms)  = sep [text "MAlt", nest 2 $ pretty ms]
  pretty MNo        = text "MNo"

trace2 nm f a b =
  let res = trace (show (sep [text nm
                             , nest 2 $ sep $ [pretty a, pretty b]])) f a b
   in trace (show (sep [text nm
                       , nest 2 $ sep $ [pretty a, pretty b, text "=>", pretty res]]))
            res

fs rules = (pmap, pfold)
  where
    pmap :: Char -> Match
    pmap ch | trace ("pmap " ++ show ch) False = undefined
    pmap ch = MAlt $ fix $ Ch ch
    fix :: Parser -> [Match]
    fix x | trace ("fix " ++ showPretty x) False = undefined
    fix p = [MP p]
            ++ (concatMap fix $ mapMaybe (matchRule p) rules)

    pfold :: Match -> Match -> Match
    pfold = trace2 "pfold" pfold'

    pfold' a b = mkalt [ prec a b , mkseq2 a b]

    prec a b = case p a b of
                 MNo -> MNo
                 res -> trace (show (sep [text "prec", sep [pretty a, pretty b, text "=>", pretty res]])) res

    p :: Match -> Match -> Match
    p (MP x) (MP y) = mkalt $ mapMaybe (\r-> fmap MP $ matchRule2 x y r) rules

    p (MAlt xs) (MAlt ys) = mkalt $ [ prec x y | x <- xs, y <- ys ]
    p x (MAlt ys) = mkalt $ [ prec x y | y <- ys ]
    p (MAlt xs) y = mkalt $ [ prec x y | x <- xs ]

    p (MSeq xs) y = prec (MSeqr $ reverse xs) y
    p x (MSeqr ys) = prec x (MSeq $ reverse ys)
    p (MSeqr (x:xs)) (MSeq (y:ys)) =
                        case prec x y of
                          MNo -> MNo
                          m@(MP _) -> pSeqrSeq xs m ys
                          (MAlt as) -> mkalt (map f as)
                                       where f a = pSeqrSeq xs a ys
                          x -> error ("p_ss " ++ showPretty x)
                          
    p x (MSeq (y:ys)) = case prec x y of
                          MNo -> MNo
                          m@(MP _) -> pSeqrSeq [] m ys
                          MAlt as -> mkalt $ map (\a -> MSeq (a:ys)) as -- tedious
                                              -- ++ [mkseq2 x (MSeq (y:ys))]
                          -- MSeq rs -> mkseq2 (rs ys
                          x -> error ("p_s " ++ showPretty x)

    p (MSeqr (x:xs)) y = case prec x y of
                           MNo -> MNo
                           m@(MP _) -> pSeqrSeq xs m []
                           MAlt as -> mkalt $ map (\a -> MSeqr (a:xs)) as -- tedious
                                              -- ++ [mkseq2 x (MSeq (y:ys))]
                           -- MSeq rs -> mkseq2 (rs ys
                           x -> error ("p_sr " ++ showPretty x)

    pSeqrSeq ls x rs = mkseq $ reverse ls ++ x : rs


    mkalt :: [Match] -> Match
    mkalt = mk . flatten . filt
      where  mk [] = MNo
             mk [x] = x
             mk xs = MAlt xs
             flatten [] = []
             flatten (MAlt xs : ys) = flatten xs ++ flatten ys
             flatten (x:xs) = x : flatten xs

    mkseq2 (MSeqr xs) (MSeq ys) = MSeq (reverse xs ++ ys)
    mkseq2 (MSeqr xs) y = MSeq (reverse xs ++ [y])
    mkseq2 (MSeq xs) (MSeq ys) = MSeq (xs ++ ys)
    mkseq2 x (MSeq ys) = MSeq (x : ys)
    mkseq2 (MSeq xs) y = MSeq (xs ++ [y])
    mkseq2 a b = MSeq [a, b]

    
    mkseq = mk . flatten
      where  mk [] = MSeq []
             mk [x] = x
             mk xs = MSeq xs
             flatten [] = []
             flatten (MSeq xs : ys) = flatten xs ++ flatten ys
             flatten (x:xs) = x : flatten xs

    filt (MNo : xs) = filt xs
    filt (x : xs) = x : filt xs
    filt [] = []

   {-
    pfold' (MAlt xs) (MAlt ys) = MAlt $ concat [ f2 x y | x <- xs, y <- ys]
                                        ++ [MSeq [MAlt xs, MAlt ys]]
      where f2 :: Match -> Match -> [Match]
            f2 (MP x) (MP y) = mapMaybe gg rules
                   where gg r = MP `fmap` matchRule2 x y r
            f2 x y | trace ("f2 " ++ showPretty (x,y)) False = undefined
            f2 x@(MP _) (MSeq ys) = fseq [x] ys
            f2 (MSeq xs) y@(MP _) = fseq (reverse xs) [y]
            f2 (MSeq xs) (MSeq ys) = fseq (reverse xs) ys
            fseq :: [Match] -> [Match] -> [Match]
            fseq x y | trace ("fseq " ++ showPretty (x,y)) False = undefined
            fseq [MP a] (MAlt bs : ys) = concat [fu [] ys (f2 (MP a) b) | b <- bs]
            fseq (MAlt as : xs) (MAlt bs : ys) =
               concat [fu xs ys (f2 a b) | a <- as, b <- bs]
            fu :: [Match] -> [Match] -> [Match] -> [Match]
            fu xr xl ms = map (\m -> MSeq (xr ++ [m] ++ xl)) ms
-}

-- SEQUENCE
-- type Part = [Parser]




main =
  do let gra1 = preprocessSeqOpts gram1
         gra2 = seqToSeq2 gra1
     pprint $ ("opts inlined", gra1)
     pprint $ ("seq's replaced by seq2s", gra2)
     let fr = sort $ nub $ concat $ map universe gra2
     {-pprint fr
     pprint $ altRules fr
     pprint $ seqRules fr
     pprint $ defRules fr-}
     let rules = altRules fr ++ seqRules fr ++ defRules fr ++ repRules fr
     pprint $ sort $ rules
     -- parse "123 abc  DefabcdxabcdghIJk  " rules
     -- parse "123 abc  Def  " rules
     parse "    " rules
