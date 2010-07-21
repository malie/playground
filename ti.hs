{-# OPTIONS_GHC -fglasgow-exts #-}
import Data.Generics
import Data.Generics.Uniplate.Data
import Control.Monad.State.Lazy
import Text.PrettyPrint

class Pretty a where
    pretty :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . show . pretty

type Name = String

data Expr t
    = SLiteral Value
    | SVar Name
    | SLambda Name t
    | SApply t t
    | SLet { sletNamedExprs :: NamedExprs t
           , sletBody :: t }
    | SPrim Primitive
      deriving (Data, Typeable)

instance Pretty t => Pretty (Expr t) where
    pretty (SLiteral l) = parens $ sep [text "SLiteral", pretty l]
    pretty (SVar nm) = parens $ sep [text "SVar", quotes $ text nm]
    pretty (SLambda nm b) = parens $ sep [text "SLambda", text nm, nest 2 $ pretty b]
    pretty (SApply f a) = parens $ sep [text "SApply", pretty f, nest 2 $ pretty a]

type Primitive = String

type NamedExprs t = [(Name, t)]

data Type
    = TInteger
    | TBoolean
    | TFun Type Type
    | TVar Integer
      deriving (Data, Typeable)

instance Pretty Type where
    pretty TInteger = text "TInteger"
    pretty TBoolean = text "TBoolean"
    pretty (TFun f a) = parens $ sep [pretty f, text "`TFun`", pretty a]
    pretty (TVar n) = text $ "TVar " ++ show n


data Value
    = VInteger Integer
    | VBoolean Bool
      deriving (Data, Typeable)

instance Pretty Value where
    pretty (VInteger i) = text $ "VInteger " ++ show i
    pretty (VBoolean b) = text $ "VBoolean " ++ show b


newtype SE
    = SE (Expr SE)
    deriving (Data, Typeable)

instance Pretty SE where
    pretty (SE e) = parens $ sep [text "SE", pretty e]


data TIExpr
    = TIType (Expr TIExpr) Type 
    | TITypeUnknown (Expr TIExpr)
      deriving (Data, Typeable)

instance Pretty TIExpr where
    pretty (TIType e t) = parens $ sep [pretty e, text "`TIType`", pretty t]
    pretty (TITypeUnknown e) = parens $ sep [text "TITypeUnknown", pretty e]


enterType :: SE -> TIExpr
enterType (SE (SLambda nm a)) = TITypeUnknown (SLambda nm (enterType a))
enterType (SE (SApply a b))   = TITypeUnknown (SApply (enterType a) (enterType b))
enterType (SE (SLet vs b))    = TITypeUnknown (SLet (map (\(nm, x) -> (nm, enterType x)) vs) (enterType b))
enterType (SE (SLiteral x))   = TITypeUnknown (SLiteral x)
enterType (SE (SVar x))       = TITypeUnknown (SVar x)
enterType (SE (SPrim x))      = TITypeUnknown (SPrim x)

numerate :: TIExpr -> TIExpr
numerate e = fst $ runState (num [] e) 0
    where num env (TITypeUnknown (SLambda nm b)) =
              do n <- gen
                 let nmt = TVar n
                 b'@(TIType _ bt) <- num ((nm, nmt):env) b
                 return (TIType (SLambda nm b') (TFun nmt bt))
          num env (TITypeUnknown x@(SVar nm)) =
              case lookup nm env of
                Just t -> return $ TIType x t
                Nothing -> error $ "free var: " ++ nm
          num env (TITypeUnknown x) =
              do x' <- descendBiM (num env) x
                 n <- gen
                 return $ TIType x' (TVar n)
          gen = do n <- get
                   put $ n+1
                   return n

p1, p2, p3 :: SE
(p1, p2, p3) =
    let i = SE . SLiteral . VInteger
        a f x = SE (SApply f x)
        a2 f x y = a (a f x) y
        v = SE . SVar
        l nm b = SE (SLambda nm b)
    in ( i 234
       , a2 (v "sum") (i 3) (i 4)
       , l "f" (l "x" (a2 (v "f") (v "x") (v "x")))
       )

-- f g a b = g a b
-- f g a b = g a (g b)
-- map
-- fold

main = do
  let et = enterType p3
  pprint et
  let o = numerate et
  pprint o


