module Pretty
       ( Pretty
       , pretty
       , pprint
       , showPretty
       , module Text.PrettyPrint
       ) where

import Text.PrettyPrint
import Data.List (intersperse)

class Pretty a where
    pretty :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . showPretty

showPretty :: Pretty a => a -> String
showPretty = show . pretty

instance Pretty a => Pretty [a] where
   pretty l = brackets $ nest 2 $ sep $ mapn1 (<> comma) $ map pretty l

mapn1 f []     = []
mapn1 f [x]    = [x]
mapn1 f (x:xs) = f x : mapn1 f xs

instance (Pretty a, Pretty b) => Pretty (a, b) where
   pretty (a,b) = parens $ sep [ nest 2 (pretty a <> comma)
                               , nest 2 $ pretty b]

instance (Pretty a, Pretty b, Pretty c) => Pretty (a, b, c) where
   pretty (a,b,c) = parens $ sep [ nest 2 (pretty a <> comma)
                                 , nest 2 (pretty b <> comma)
                                 , nest 2 $ pretty c]

instance Pretty Char where
   pretty ch = text ('\'' : ch :"'")

instance Pretty Int where
  pretty = text . show
