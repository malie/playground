{-# OPTIONS_GHC -fglasgow-exts #-}
import Data.Generics
import qualified Data.Generics.Uniplate.Data as Uniplate
import Control.Monad.State.Lazy
import Text.PrettyPrint
import Data.List

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
      deriving (Eq, Data, Typeable)

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
    pretty (TIType t e) = sep [pretty e, nest 1 $ sep [text "::", pretty t]]
    pretty (TITypeUnknown e) = parens $ sep [text "TITypeUnknown", pretty e]


primitives :: [(Primitive, Type)]
primitives =
    let int = TPrim "int"
    in
      [
       ("intAdd", TFun int (TFun int int))
      ]

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
                 b'@(TIType bt _) <- num ((nm, nmt):env) b
                 return (TIType (TFun nmt bt) (SLambda nm b'))
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


type Constraint = (Type, Type)
instance Pretty Constraint where
    pretty (a,b) = sep [pretty a, text "=" <+> pretty b]

instance Pretty [Constraint] where  -- why?
    pretty l = brackets $ nest 2 $ sep $ map (<> comma) $ map pretty l

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
solve cs = rec cs []
    where rec :: [Constraint] -> [Constraint] -> [Constraint]
          rec [] subst = subst
          rec ((a, b):cs) subst =
              case (a==b, varLeft a b, a, b) of
                (True, _, _, _)
                    -> rec cs subst
                (_, (u@(TVar _), r), _, _)
                    -> let substAll = Uniplate.transformBi $ substitute u r
                       in rec (substAll cs) $ (u,r) : substAll subst
                (_, _, TFun rf ra, TFun sf sa)
                    -> rec ((rf,sf):(ra,sa):cs) subst
          varLeft a b@(TVar _) = (b,a)
          varLeft a b          = (a,b)
          substitute :: Type -> Type -> Type -> Type
          substitute (TVar u) r (TVar v) | u == v = r
          substitute _ _ x                        = x




p1, p2, p3, p4 :: S
(p1, p2, p3, p4) =
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
       )

-- f g a b = g a (g a b)
-- map
-- fold

main = do
  let et = enterType p4
  let o = numerate et
  pprint o
  let c = constraints o
  pprint c
  let r = solve c
  pprint r


