{-# OPTIONS_GHC -fglasgow-exts #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveFunctor #-}
import Data.Generics
import qualified Data.Generics.Uniplate.Data as Uniplate
import "mtl" Control.Monad.State.Lazy
import Text.PrettyPrint
import Data.List
import Debug.Trace(trace)

class Pretty a where
    pretty :: a -> Doc

pprint :: Pretty a => a -> IO ()
pprint = putStrLn . showPretty

showPretty :: Pretty a => a -> String
showPretty = show . pretty

instance Pretty a => Pretty [a] where
   pretty l = brackets $ nest 2 $ sep $ map (<> comma) $ map pretty l


type Name = String

data Expr t
    = SLiteral Value
    | SVar Name
    | SLambda Name t
    | SApply t t
    | SLet { sletNamedExprs :: NamedExprs t
           , sletBody :: t }
    | SPrim Primitive
      deriving (Data, Typeable, Functor)

instance Pretty t => Pretty (Expr t) where
    pretty (SLiteral l) = parens $ sep [text "SLiteral", pretty l]
    pretty (SVar nm) = parens $ sep [text "SVar", quotes $ text nm]
    pretty (SLambda nm b) = parens $ sep [text "SLambda" <+> text nm, nest 3 $ pretty b]
    pretty (SApply f a) = parens $ sep [text "SApply", pretty f, nest 3 $ pretty a]
    pretty (SLet vs b) = undefined
    pretty (SPrim nm) = parens $ text "SPrim" <+> text nm

type Primitive = String

type NamedExprs t = [(Name, t)]

data Type
    = TInteger
    | TBoolean
    | TFun Type Type
    | TVar Integer
    | TPrim String
      deriving (Eq, Data, Typeable, Show)

instance Pretty Type where
    pretty TInteger = text "TInteger"
    pretty TBoolean = text "TBoolean"
    pretty (TFun f a) = parens $ sep [pretty f, text "->", pretty a]
    pretty (TVar n) = text $ "TVar " ++ show n
    pretty (TPrim nm) = text $ "TPrim " ++ nm


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
    = TIType Type (Expr TIExpr)
    | TITypeUnknown (Expr TIExpr)
      deriving (Data, Typeable)

instance Pretty TIExpr where
    pretty (TIType t e) = sep [pretty t, pretty e]
    pretty (TITypeUnknown e) = parens $ sep [text "TITypeUnknown", pretty e]


primitives :: [(Primitive, Type)]
primitives =
    let int = TPrim "int"
    in
      [
       ("intAdd", TFun int (TFun int int))
      ]

enterType :: SE -> TIExpr
enterType (SE x) = TITypeUnknown (fmap enterType x)


numerate :: TIExpr -> TIExpr
numerate e = fst $ runState (num [] e) 0
    where num env (TITypeUnknown (SLambda nm b)) =
              do n <- gen
                 b'@(TIType bt _) <- num ((nm, TVar n):env) b
                 return (TIType (TFun (TVar n) bt) (SLambda nm b'))
          num env (TITypeUnknown x@(SVar nm)) =
              case lookup nm env of
                Just t -> return $ TIType t x
                Nothing -> error $ "unknown free var: " ++ nm
          num env (TITypeUnknown p@(SPrim nm)) =
              case lookup nm primitives of
                Just t -> return $ TIType t p
          num env (TITypeUnknown x) =
              do x' <- Uniplate.descendBiM (num env) x
                 n <- gen
                 return $ TIType (TVar n) x'
          gen = do n <- get
                   put $ n+1
                   return n


-- Equality constraint
type Constraint = (Type, Type)
instance Pretty Constraint where
    pretty (a,b) = sep [pretty a, text "=" <+> pretty b]

constraints :: TIExpr -> [Constraint]
constraints = Uniplate.para f
    where c a b = a : concat b
          cs a b = concat (a:b)
          f :: TIExpr -> [[Constraint]] -> [Constraint]
          f (TIType t (SLiteral (VInteger _))) = c (t, TInteger)
          f (TIType t (SLiteral (VBoolean _))) = c (t, TBoolean)
          f (TIType t (SApply (TIType tf _) (TIType ta _))) = c (tf, TFun ta t)
          f _ = concat


-- plai p.280
solve :: [Constraint] -> [Constraint]
solve cs = (xrec cs [])
    where xrec :: [Constraint] -> [Constraint] -> [Constraint]
          xrec [] subst = subst
          xrec (t@(_,_):_) subst | trace ("\n" ++ showPretty t) False = undefined
          xrec ((a, b):cs) subst | a == b = xrec cs subst
          xrec ((a, b):cs) subst =
              case (varLeft a b, a, b) of
                ((u@(TVar _), r), _, _)
                    -> let su = substitute u r
                           subst' = (u,r) : su subst
                       in trace ("new subst:\n " ++ showPretty subst') $
                                xrec (su cs) $ subst'
                (_, TFun rf ra, TFun sf sa)
                    -> xrec ((rf,sf):(ra,sa):cs) subst
          varLeft a b@(TVar _) = (b,a)
          varLeft a b          = (a,b)

class Substitute a b where
    substitute :: a -> a -> b -> b

instance Substitute Type Type where
    substitute u r = Uniplate.transform $ substitute' u r

instance Substitute Type Constraint where
    substitute u r = Uniplate.transformBi $ substitute' u r

instance Substitute Type TIExpr where
    substitute u r = Uniplate.transformBi $ substitute' u r

substitute' :: Type -> Type -> Type -> Type
substitute' (TVar u) r (TVar v) | u == v = r
substitute' _ _ x                        = x

instance Substitute a b => Substitute a [b] where
    substitute u r = map (substitute u r)




p1, p2, p3, p4, p5 :: SE
(p1, p2, p3, p4, p5) =
    let i = SE . SLiteral . VInteger
        a f x = SE (SApply f x)
        a2 f x y = a (a f x) y
        v = SE . SVar
        fn nm b = SE (SLambda nm b)
        p = SE . SPrim
    in ( i 234
       , fn "f" (fn "x" (a2 (v "f") (v "x") (v "x")))
       , fn "f" (fn "a" (a2 (p "intAdd") (v "a") (v "a")))
       , fn "g" (fn "a" (fn "b"
           (a2 (v "g")
               (v "a")
               (a2 (v "g") (v "a") (v "b")))))
       , fn "f" (fn "g" (fn "x" (a (v "f") (a (v "g") (v "x")))))
       )

-- f g a b = g a (g a b)
-- map
-- fold

main = do
  let x = p5
  pprint x
  let et = enterType x
  pprint et
  let o = numerate et
  pprint o
  let c = constraints o
  pprint c
  let r = solve c
  pprint r
  let res = foldr (uncurry substitute) o r
  pprint res
