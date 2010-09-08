{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternGuards #-}
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

instance (Pretty a, Pretty b) => Pretty (a, b) where
   pretty (a,b) = parens $ sep [ nest 2 (pretty a <> comma)
                               , nest 2 $ pretty b]

type TypeName = String     -- upper case
type TypeVarName = String  -- lower case polymorphic type variable

data TopLevel t
    = TLDef Name t
    | TLData TypeName TypeContext [DataConstr]

instance Pretty t => Pretty (TopLevel t) where
    pretty (TLDef nm e) = sep [ text nm <+> text "=" , nest 2 $ pretty e]

data TypeContext = TCTypes [TypeName]

type DataConstrName = String
data DataConstr = DataConstr DataConstrName TypeArgs

type TypeArgs = [TypeArg]
data TypeArg
    = TAType TypeVarName
    | TATypeConstr TypeName TypeArgs



type Name = String

data Expr t
    = SLiteral Value
    | SVar Name
    | SLambda Name t
    | SApply t t
    | SLet { sletNamedExprs :: NamedExprs t
           , sletBody :: t }
    | SPrim Primitive
    | SCons Int DataConstrName -- arity and constructor name
    | SCase t (CaseBranches t)
      deriving (Data, Typeable, Functor)

instance Pretty t => Pretty (Expr t) where
    pretty (SLiteral l) = parens $ sep [text "SLiteral", pretty l]
    pretty (SVar nm) = parens $ sep [text "SVar", quotes $ text nm]
    pretty (SLambda nm b) = parens $ sep [text "SLambda" <+> text nm, nest 3 $ pretty b]
    pretty (SApply f a) = parens $ sep [text "SApply", pretty f, nest 3 $ pretty a]
    pretty (SLet vs b) = undefined
    pretty (SPrim nm) = parens $ text "SPrim" <+> text nm
    pretty (SCons ary nm) = text $ nm ++ "/" ++ show ary
    pretty (SCase e bs) = parens $ sep [ text "case" <+> pretty e <+> text "of"
                                       , nest 2 $ sep $ map branch bs ]
        where branch (p, pe) = sep [ pretty p <+> text "->", nest 2 $ pretty pe ]


type Primitive = String

type NamedExprs t = [(Name, t)]

type CaseBranches t = [(Pattern, t)]

data Pattern
    = PVar Name
    | PCons DataConstrName [Name] -- data constructor name and variable names
      deriving (Data, Typeable)

instance Pretty Pattern where
    pretty (PVar nm) = text nm
    pretty (PCons nm ns) = parens $ sep $ map text $ nm:ns

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

data TITopLevel
    = TITopLevel Type (TopLevel TIExpr)
    | TITopLevelUnknown (TopLevel TIExpr)
instance Pretty TITopLevel where
    pretty (TITopLevelUnknown e) = pretty e
    pretty (TITopLevel t e@(TLDef nm _)) = sep [ text nm <+> text "::" <+> pretty t
                                               , pretty e]

enterType :: [TopLevel SE] -> [TITopLevel]
enterType ts = map tl ts
    where tl (TLDef nm x) = TITopLevelUnknown $ TLDef nm $ expr x
          expr (SE x) = TITypeUnknown $ fmap expr x

numerate :: [TITopLevel] -> [TITopLevel]
numerate ts = fst $ runState (mapM numtl ts) 0
    where numtl (TITopLevelUnknown (TLDef nm x)) = do tn <- gen
                                                      liftM (TITopLevel (TVar tn) . TLDef nm)
                                                            (num [] x)
          num env (TITypeUnknown (SLambda nm b)) =
              do n <- gen
                 b'@(TIType bt _) <- num ((nm, TVar n):env) b
                 return (TIType (TFun (TVar n) bt) (SLambda nm b'))
          num env (TITypeUnknown x@(SVar nm)) =
              case lookup nm env of
                Just t -> return $ TIType t x
                Nothing -> gen >>= (\n -> return $ TIType (TVar n) x)
          num env (TITypeUnknown p@(SPrim nm)) =
              case lookup nm primitives of
                Just t -> return $ TIType t p
                Nothing -> error $ "unknown SPrim type: " ++ nm
          num env (TITypeUnknown x) =
              do x' <- Uniplate.descendBiM (num env) x
                 n <- gen
                 return $ TIType (TVar n) x'
          gen = do n <- get
                   put $ n+1
                   return n

-- Equality constraint
data Constraint
    = Equal Type Type
    | InstanceOf Type Type
      deriving (Data, Typeable)

instance Pretty Constraint where
    pretty (Equal a b) = sep [pretty a, text "===" <+> pretty b]
    pretty (InstanceOf a b) = sep [pretty a, nest 2 $ sep [text "`instanceOf`", pretty b]]

constraints :: [TITopLevel] -> [Constraint]
constraints ts = concatMap pt ts
    where globals = concatMap gl ts
          gl (TITopLevel t (TLDef nm _)) = [(nm, t)]
          pt (TITopLevel t1 (TLDef _ e@(TIType t2 _))) = Equal t1 t2 : Uniplate.para f e
          f :: TIExpr -> [[Constraint]] -> [Constraint]
          f (TIType t (SLiteral (VInteger _))) = c (Equal t TInteger)
          f (TIType t (SLiteral (VBoolean _))) = c (Equal t TBoolean)
          f (TIType t (SApply (TIType tf _) (TIType ta _))) = c (Equal tf $ TFun ta t)
          f (TIType t (SVar nm)) | Just gt <- lookup nm globals = c (InstanceOf t gt)
          f _ = concat
          c a b = a : concat b

-- plai p.280
solve :: [Constraint] -> [Constraint]
solve cs = (xrec cs ([],[]))
    where xrec :: [Constraint] -> ([Constraint], [Constraint]) -> [Constraint]
          xrec [] (subst, del) = subst ++ del
          xrec (co:_) sd | trace ("\n" ++ showPretty sd) False = undefined
          xrec (i@(InstanceOf _ _):cs) (subst, del) = xrec cs (subst, i:del)
          xrec (Equal a b:cs) sd | a == b = xrec cs sd
          xrec (Equal a b:cs) (subst, del) =
              case (varLeft a b, a, b) of
                ((u@(TVar _), r), _, _)
                    -> let su = substitute u r
                           subst' = Equal u r : su subst
                       in trace ("new subst:\n " ++ showPretty subst') $
                                xrec (su cs) $ (subst', su del)
                (_, TFun rf ra, TFun sf sa)
                    -> xrec (Equal rf sf : Equal ra sa : cs) (subst, del)
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


prelude =
    [ TLData "List" (TCTypes ["a"])
             [ DataConstr "Nil" []
             , DataConstr "Cons" [ TAType "a", TATypeConstr "List" [TAType "a"]]]]

progs :: [[TopLevel SE]]
progs =
    let i = SE . SLiteral . VInteger
        a f x = SE (SApply f x)
        a2 f x y = a (a f x) y
        v = SE . SVar
        fn nm b = SE (SLambda nm b)
        p = SE . SPrim
        cons ary nm = SE $ SCons ary nm
        cas e bs = SE $ SCase e bs
    in [ [TLDef "compose" $ fn "f" $ fn "g" $ fn "x" $ a (v "f") (a (v "g") (v "x"))]
       , [TLDef "fff" $ fn "f" $ fn "x" $ a2 (v "fff") (v "f") (a (v "f") (v "x"))]
       , [TLDef "main" $ fn "g" $ fn "a" $ fn "b"
                    (a2 (v "g")
                        (v "a")
                        (a2 (v "g") (v "a") (v "b")))]
       , prelude ++
         [TLDef "map" $ fn "f" $ fn "l" $
                cas (v "l") [(PCons "cons" ["hd", "tl"]
                             , a2 (cons 2 "cons")
                                  (a (v "f") (v "hd"))
                                  (v "tl"))]]
       ]

-- f g a b = g a (g a b)
-- map
-- fold

main = do
  let x = progs !! 1
  pprint x
  let et = enterType x
  pprint et
  let o = numerate et
  pprint o
  let c = constraints o
  pprint c
  let r = solve c
  pprint r
  putStrLn "\n"
  pprint r
{-  let res = foldr (uncurry substitute) o r
  pprint res
-}