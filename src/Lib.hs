{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
module Lib
    ( someFunc
    ) where

import Prelude hiding (lex)
import Data.Functor
import System.FilePath
import Data.List
import Data.Map (Map)
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Monoid
import System.Environment
import Control.Monad

import Control.Monad.Error.Class (MonadError)
import Language.PureScript.Errors (MultipleErrors)

import Language.PureScript.AST.Declarations
import Language.PureScript.AST.Binders
import Language.PureScript.Types
import Language.PureScript.AST.SourcePos
import Language.PureScript.Names
import Language.PureScript.Parser.Declarations
import Language.PureScript.Parser.Common
import Language.PureScript.Parser.Lexer
import Language.PureScript.Options
import Language.PureScript.Make
import Language.PureScript.Sugar.Names.Imports

someFunc :: IO ()
someFunc = main

main = do
  [importsFile, codeFile] <- getArgs
  Right importsDecls <- readDecls importsFile
  (Right imports, _) <- runMake defaultOptions (toImports importsDecls)

  Right codeDecls <- readDecls codeFile
  codeDeclsImports <- runMake defaultOptions (toImports importsDecls)

  let pickedImports = filter (not . isEmptyImport)
                    $ importsForReferences (usedReferences imports codeDecls)
                                           imports
  putStrLn $ unlines $ map ppImport pickedImports

parseDecls :: String -> [Declaration]
parseDecls s = either (const []) id $ do
  ts <- lex "" s
  Module _ _ _ decls _ <- runTokenParser "" parseModule ts
  pure decls

readDecls :: FilePath -> IO (Either _ [Declaration])
readDecls filename = do
  file <- readFile filename
  pure $ do
    ts <- lex filename file
    Module _ _ _ decls _ <- runTokenParser filename parseModule ts
    pure decls

data Import = Import { sourceSpan      :: SourceSpan
                     , moduleName      :: ModuleName
                     , declarationType :: ImportDeclarationType
                     , qualifier       :: Maybe ModuleName
                     }
            deriving (Show, Eq)

isEmptyImport :: Import -> Bool
isEmptyImport (Import _ _ (Explicit []) _) = True
isEmptyImport _ = False

ppImport :: Import -> String 
ppImport Import{..} =
  "import " <> runModuleName moduleName
            <> ppImportDeclarationType declarationType
            <> ppQualifier qualifier
  where
    ppImportDeclarationType :: ImportDeclarationType -> String
    ppImportDeclarationType Implicit = " "
    ppImportDeclarationType (Hiding as) =
      "hiding" <> " (" <> intercalate ", " (ppRef <$> as) <> ") "
    ppImportDeclarationType (Explicit as) =
      " (" <> intercalate ", " (ppRef <$> as) <> ") "
    
    ppQualifier (Just a) = "as " <> runModuleName a
    ppQualifier Nothing  = ""

    ppRef (ValueRef (Ident i)) = i
    ppRef (ValueRef (Op o)) = "(" <> o <> ")"
    ppRef (TypeRef type_ Nothing) = runProperName type_ <> "()"
    ppRef (TypeRef type_ (Just ctors)) = runProperName type_ <> "(" <> intercalate ", " (runProperName <$> ctors) <> ")"
    ppRef (TypeClassRef a) = "class" <> " " <> runProperName a
    ppRef (PositionedDeclarationRef _ _ a) = ppRef a
    ppRef a = show a

importDeclaration :: Import -> Declaration
importDeclaration Import{..} =
  ImportDeclaration moduleName declarationType qualifier False

toImports :: [Declaration] -> Make [Import]
toImports = fmap (Map.foldMapWithKey f) . findImports
  where
    f name = map (\(Just ss, declType, q) -> Import ss name declType q)

importsForReferences :: [Qualified DeclarationRef] -> [Import] -> [Import]
importsForReferences refs = map (importForReferences refs)

importForReferences :: [Qualified DeclarationRef] -> Import -> Import
importForReferences refs (Import ss name Implicit (Just q))
  | not (any (qualifiedWith q) refs) = Import ss name (Explicit []) (Just q)
  | otherwise = Import ss name Implicit (Just q)
importForReferences refs (Import ss name (Explicit iRefs) q)
  = Import ss name (Explicit (pickImportReferences refs (Qualified q iRefs))) q
importForReferences _ i = i

pickImportReferences :: [Qualified DeclarationRef] -> Qualified [DeclarationRef]
                     -> [DeclarationRef]
pickImportReferences refs (Qualified q iRefs)
  = mapMaybe f iRefs
  where
    f (TypeRef type_ (Just _))
      = guard isTypeUsed $> TypeRef type_ (Just usedConstructors)
      where
        (Any isTypeUsed, (Set.toList -> usedConstructors)) = foldMap g refs

        g (Qualified q' (TypeRef type_' (Just constrs')))
            | type_ == type_' && q == q'
            = (Any True, Set.fromList constrs')
        g (Qualified q' (PositionedDeclarationRef _ _ a)) = g (Qualified q' a)
        g _ = (Any False, mempty)
    f (PositionedDeclarationRef a b c) = PositionedDeclarationRef a b <$> f c
    f a = guard (Qualified q a `elem` refs) $> a

overPositioned :: (DeclarationRef -> a) -> (DeclarationRef -> a)
overPositioned f (PositionedDeclarationRef _ _ c) = (f c)
overPositioned f a = f a

qualifiedWith :: ModuleName -> Qualified a -> Bool
qualifiedWith _ (Qualified Nothing _) = False
qualifiedWith name (Qualified (Just name') _) = name == name'
      
usedReferences :: [Import] -> [Declaration] -> [Qualified DeclarationRef]
usedReferences imports = foldMap inDeclaration
  where
    inExpr NumericLiteral{} = []
    inExpr StringLiteral{} = []
    inExpr CharLiteral{} = []
    inExpr BooleanLiteral{} = []
    inExpr (UnaryMinus a) = inExpr a
    inExpr (BinaryNoParens a b c) = inExpr a <> inExpr b <> inExpr c
    inExpr (Parens a) = inExpr a
    inExpr (OperatorSection a ebc) = inExpr a <> either inExpr inExpr ebc
    inExpr (ArrayLiteral as) = foldMap inExpr as
    inExpr (ObjectLiteral (snd . unzip -> as)) = foldMap inExpr as
    inExpr (ObjectConstructor (snd . unzip -> as)) = foldMap (maybe [] inExpr) as
    inExpr (ObjectGetter{}) = []
    inExpr (Accessor _ a) = inExpr a
    inExpr (ObjectUpdate a (snd . unzip -> as)) = inExpr a <> foldMap inExpr as
    inExpr (ObjectUpdater a (snd . unzip -> as)) = maybe [] inExpr a
                                                <> foldMap (maybe [] inExpr) as
    inExpr (Abs eab c) = either (const []) inBinder eab <> inExpr c
    inExpr (App a b) = inExpr a <> inExpr b
    inExpr (Var qIdent) = [ValueRef <$> qIdent]
    inExpr (IfThenElse a b c) = inExpr a <> inExpr b <> inExpr c
    inExpr (Constructor a) = maybe [] (\t -> [TypeRef t . Just . (:[]) <$> a])
                                      (findConstructorType imports a)
    inExpr (Case expr alts) =
         foldMap inExpr expr
      <> foldMap (\(CaseAlternative{..}) ->
                      foldMap inBinder caseAlternativeBinders
                   <> either (foldMap (\(a,b) -> inExpr a <> inExpr b))
                             inExpr caseAlternativeResult)
                 alts
    inExpr (TypedValue _ a t) = inExpr a <> inType t
    inExpr (Let ds e) = foldMap inDeclaration ds <> inExpr e
    inExpr (Do els) = foldMap inDoNotationElement els
    inExpr TypeClassDictionaryConstructorApp{} = []
    inExpr TypeClassDictionary{} = []
    inExpr TypeClassDictionaryAccessor{} = []
    inExpr SuperClassDictionary{} = []
    inExpr (PositionedValue _ _ e) = inExpr e

    inDoNotationElement (DoNotationValue expr) = inExpr expr
    inDoNotationElement (DoNotationBind binder expr) =
       Qualified Nothing (ValueRef (Ident "bind")) : inExpr expr <> inBinder binder
    inDoNotationElement (DoNotationLet decls) = foldMap inDeclaration decls
    inDoNotationElement (PositionedDoNotationElement _ _ a) = inDoNotationElement a

    inDeclaration (DataDeclaration _ _ _ constrs) = foldMap (foldMap inType . snd) constrs
    inDeclaration (DataBindingGroupDeclaration decls) = foldMap inDeclaration decls
    inDeclaration (TypeSynonymDeclaration _ _ type_) = inType type_
    inDeclaration (TypeDeclaration _ type_) = inType type_
    inDeclaration (ValueDeclaration _ _ binders guardedExprs) =
         foldMap inBinder binders
      <> either (foldMap (\(a,b) -> inExpr a <> inExpr b)) inExpr guardedExprs
    inDeclaration (BindingGroupDeclaration decls) = foldMap (\(_, _, e) -> inExpr e) decls
    inDeclaration (ExternDeclaration _ a) = inType a
    inDeclaration (ExternDataDeclaration{}) = []
    inDeclaration (FixityDeclaration{}) = []
    inDeclaration (ImportDeclaration{}) = []
    inDeclaration (TypeClassDeclaration _ _ constrs decls) =
      foldMap inConstraint constrs <> foldMap inDeclaration decls
    inDeclaration (TypeInstanceDeclaration _ constraints className args body) =
         foldMap inConstraint constraints
      <> [TypeClassRef <$> className]
      <> foldMap inType args
      <> case body of
           DerivedInstance -> []
           ExplicitInstance decls -> foldMap inDeclaration decls
    inDeclaration (PositionedDeclaration _ _ a) = inDeclaration a

    inBinder NullBinder = []
    inBinder BooleanBinder{} = []
    inBinder StringBinder{} = []
    inBinder CharBinder{} = []
    inBinder NumberBinder{} = []
    inBinder VarBinder{} = []
    inBinder (ConstructorBinder name binders) =
         maybe [] (\t -> [(\n -> TypeRef t (Just [n])) <$> name])
                  (findConstructorType imports name)
      <> foldMap inBinder binders
    inBinder (ObjectBinder ((snd . unzip) -> binders)) = foldMap inBinder binders
    inBinder (ArrayBinder binders) = foldMap inBinder binders
    inBinder (NamedBinder _ a) = inBinder a
    inBinder (PositionedBinder _ _ a) = inBinder a
    inBinder (TypedBinder t b) = inType t <> inBinder b

    inConstraint :: Constraint -> [Qualified DeclarationRef]
    inConstraint (qClassName, args) = (TypeClassRef <$> qClassName) : foldMap inType args

    inType (TUnknown _)   = []
    inType (TypeVar _)    = []
    inType (TypeWildcard) = []
    inType (TypeConstructor a) = [flip TypeRef (Just []) <$> a]
    inType (TypeApp a b)  = inType a <> inType b
    inType (ForAll _ a _) = inType a
    inType (ConstrainedType constraints a)
                            = inType a <> foldMap inConstraint constraints
    inType (Skolem _ _ _ _) = []
    inType (REmpty)         = []
    inType (RCons _ a b)    = inType a <> inType b
    inType (KindedType a _) = inType a
    inType (PrettyPrintFunction a b) = inType a <> inType b
    inType (PrettyPrintObject a)     = inType a
    inType (PrettyPrintForAll _ a)   = inType a

findConstructorType :: [Import] -> Qualified (ProperName 'ConstructorName)
                    -> Maybe (ProperName 'TypeName)
findConstructorType imports = (`lookup` (foldMap f imports))
  where
    f (Import _ _ (Explicit iRefs) q) =
        foldMap (overPositioned
                  (\case TypeRef typeName (Just ctors) 
                           -> (map ((,typeName) . (Qualified q)) ctors)
                         _ -> [])) iRefs
    f _ = []
