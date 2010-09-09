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
      deriving (Data, Typeable)

data TypeContext = TCTypes [TypeName]
                   deriving (Data, Typeable)

type DataConstrName = String
data DataConstr = DataConstr DataConstrName TypeArgs
                  deriving (Data, Typeable)

type TypeArgs = [TypeArg]
data TypeArg
    = TAType TypeVarName
    | TATypeConstr TypeName TypeArgs
      deriving (Data, Typeable)


instance Pretty t => Pretty (TopLevel t) where
    pretty (TLDef nm e) = sep [ text nm <+> text "=" , nest 2 $ pretty e]
    pretty (TLData tn tc cs) = sep [ text "data" <+> text tn <+> pretty tc,
                                     nest 2 $ sep $ zipWith con cs ("=" : repeat "|")]
        where con c prefix = text prefix <+> pretty c

instance Pretty TypeContext where
    pretty (TCTypes ns) = sep $ map text ns

instance Pretty DataConstr where
    pretty (DataConstr dcnm ta) = sep (text dcnm : map (parens.pretty) ta)

instance Pretty TypeArg where
    pretty (TAType tvn) = text tvn
    pretty (TATypeConstr tn tas) = sep (text tn : map pretty tas)

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
    | TApp Type Type
    | TVar Integer
    | TName TypeName
    | TArg TypeName
    | TPrim String
      deriving (Eq, Data, Typeable, Show)

instance Pretty Type where
    pretty TInteger = text "TInteger"
    pretty TBoolean = text "TBoolean"
    pretty a@(TFun _ _) = parens $ sep $ link $ map pretty $ arrow a
        where arrow (TFun f a) = f : arrow a
              arrow a          = [a]
              link (a:as) = a : map (nest 2 . (text "->" <+>) . parens) as
    pretty (TApp f a) = sep [ text $ "TApp "
                            , nest 2 $ parens $ pretty f
                            , nest 2 $ parens $ pretty a]
    pretty (TVar n) = text $ "TVar " ++ show n
    pretty (TName n) = text $ "TName " ++ show n
    pretty (TArg n) = text $ "TArg " ++ show n
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
    | TIDataConstructor DataConstrName Type
    | TITopLevelUnknown (TopLevel TIExpr)
      deriving (Data, Typeable)
instance Pretty TITopLevel where
    pretty (TITopLevelUnknown e) = pretty e
    pretty (TITopLevel t e@(TLDef nm _)) = sep [ text nm <+> text "::" <+> pretty t
                                               , pretty e]
    pretty (TIDataConstructor nm t) = sep [text nm <+> text "::", nest 2 $ pretty t]

enterType :: [TopLevel SE] -> [TITopLevel]
enterType ts = concatMap tl ts
    where tl (TLDef nm x) = [TITopLevelUnknown $ TLDef nm $ expr x]
          tl (TLData tn (TCTypes tvars) dcs) = map dataCons dcs
              where dataCons (DataConstr dcn tas) = TIDataConstructor dcn $ typeFuns tas dataType
                    typeArg (TAType tvn) = TArg tvn
                    typeArg (TATypeConstr tn tas) = typeApps (TName tn) tas
                    -- foldr (flip . TFun) dataType (map typeArg tas)
                    typeFuns [] dt = dt
                    typeFuns (a:as) dt = TFun (typeArg a) (typeFuns as dt)
                    typeApps dt [] = dt
                    typeApps dt (a:as) = TApp (typeApps dt as) (typeArg a)
                    dataType = foldl TApp (TName tn) (map TArg tvars)
          expr (SE x) = TITypeUnknown $ fmap expr x

numerate :: [TITopLevel] -> [TITopLevel]
numerate ts = fst $ runState (mapM numtl ts) 0
    where numtl (TITopLevelUnknown (TLDef nm x)) = do tn <- gen
                                                      liftM (TITopLevel (TVar tn) . TLDef nm)
                                                            (num [] x)
          numtl x = return x
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
    where globals = trace ("globals:\n " ++ show globals') globals'
          globals' = concatMap gl ts
          gl (TITopLevel t (TLDef nm _)) = [(nm, t)]
          gl (TIDataConstructor nm t) = [(nm, t)]
          pt (TITopLevel t1 (TLDef _ e@(TIType t2 _))) = Equal t1 t2 : Uniplate.para f e
          pt (TIDataConstructor _ _) = []
          f :: TIExpr -> [[Constraint]] -> [Constraint]
          f (TIType t (SLiteral (VInteger _))) = c (Equal t TInteger)
          f (TIType t (SLiteral (VBoolean _))) = c (Equal t TBoolean)
          f (TIType t (SApply (TIType tf _) (TIType ta _))) = c (Equal tf $ TFun ta t)
          f (TIType t (SVar nm)) | Just gt <- lookup nm globals = c (InstanceOf t gt)
          f (TIType t (SCons _ nm)) | Just gt <- lookup nm globals = c (InstanceOf t gt)
          f _ = concat
          c a b = a : concat b

data SubstDel
    = SubstDel { sdSubstitutions :: [Constraint]
               , sdDelayed :: [Constraint]}

instance Pretty SubstDel where
    pretty (SubstDel s d) = parens $ sep [text "SubstDel", nest 2 $ sep [pretty s, pretty d]]

insertSubstitution u r (SubstDel subst del) = let su = substitute u r
                                              in SubstDel (Equal u r : su subst) (su del)
insertDelayed d (SubstDel subst del) = SubstDel subst (d:del)

-- plai p.280
solve :: [Constraint] -> [Constraint]
solve cs = (xrec cs (SubstDel [] []))
    where xrec :: [Constraint] -> SubstDel -> [Constraint]
          xrec [] (SubstDel subst del) = subst ++ del
          xrec (co:_) sd | trace ("\n" ++ showPretty sd) False = undefined
          xrec (io@(InstanceOf _ _):cs) sd = xrec cs $ insertDelayed io sd
          xrec (Equal a b:cs) sd | a == b = xrec cs sd
          xrec (Equal a b:cs) sd | (u@(TVar _), r) <- varLeft a b
                                 = xrec (substitute u r cs) $ insertSubstitution u r sd
          xrec (Equal (TFun rf ra) (TFun sf sa) : cs) sd
                                 = xrec (Equal rf sf : Equal ra sa : cs) sd
          varLeft a b@(TVar _) = (b,a)
          varLeft a b          = (a,b)

substitute :: (Data a) => Type -> Type -> a -> a
substitute u r = Uniplate.transformBi $ f u r
    where f :: Type -> Type -> Type -> Type
          f (TVar u) r (TVar v) | u == v = r
          f _ _ x                        = x

substituteEquals :: Constraint -> [TITopLevel] -> [TITopLevel]
substituteEquals (Equal a b) tl = map (substitute a b) tl
substituteEquals (InstanceOf _ _) tl = tl

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
                cas (v "l") [(PCons "Cons" ["hd", "tl"]
                             , a2 (cons 2 "Cons")
                                  (a (v "f") (v "hd"))
                                  (v "tl"))]]
       ]

-- f g a b = g a (g a b)
-- map
-- fold

main = do
  let x = progs !! 3
  pprint x
  let et = enterType x
  pprint et
  let o = numerate et
  pprint o
  let c = constraints o
  pprint c
  let r = solve c
  pprint r
  let res = foldr substituteEquals o r
  pprint res
