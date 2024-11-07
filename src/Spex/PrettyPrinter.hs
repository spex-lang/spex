{-# LANGUAGE OverloadedStrings #-}

module Spex.PrettyPrinter where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String
import Prettyprinter
import Prettyprinter.Util

import Spex.Syntax

------------------------------------------------------------------------

prettySpec :: Spec -> Doc x
prettySpec spec = prettyComponent spec.component

prettyComponent :: Component -> Doc x
prettyComponent comp =
  "component"
    <+> prettyBS comp.id
    <+> "where"
    <> line
    <> line
    <> vsep (map prettyTypeDecl comp.typeDecls)
    <> vsep (map (prettyOpDecl . item) comp.opDecls)

prettyTypeDecl :: TypeDecl -> Doc x
prettyTypeDecl tyDecl =
  "type" <+> prettyBS tyDecl.typeId <+> "=" <+> prettyType tyDecl.rhs

prettyOpDecl :: OpDecl -> Doc x
prettyOpDecl opDecl =
  hsep $
    [ prettyBS opDecl.id
    , ":"
    , prettyMethod opDecl.method
    , prettyPath opDecl.path
    ]
      ++ ( case opDecl.body of
            Nothing -> []
            Just ty -> [prettyType ty]
         )
      ++ [ prettyResponseType opDecl.responseType
         ]

prettyMethod :: Method -> Doc x
prettyMethod Get = "GET"
prettyMethod Post = "POST"
prettyMethod Put = "PUT"
prettyMethod Delete = "DELETE"

prettyPath :: [PathSegment Type] -> Doc x
prettyPath = align . hcat . zipWith (<>) (repeat "/") . map aux
  where
    aux (Path bs) = prettyBS bs
    aux (Hole p ty) = "{" <> prettyBS p <+> ":" <+> prettyType ty <> "}"

prettyType :: Type -> Doc x
prettyType UnitT = "Unit"
prettyType BoolT = "Bool"
prettyType StringT = "String"
prettyType IntT = "Int"
prettyType (ArrayT tys) = error "prettyType: not implemented yet!"
prettyType (RecordT r) = prettyRecord prettyType r
prettyType (UserT tid) = prettyBS tid.item
prettyType (AbstractT ty) = "@" <> prettyType ty
prettyType (UniqueT ty) = "!" <> prettyType ty

prettyResponseType :: Type -> Doc x
prettyResponseType UnitT = mempty
prettyResponseType ty = "->" <+> prettyType ty

prettyRecord :: (a -> Doc x) -> Record a -> Doc x
prettyRecord p =
  group
    . encloseSep
      (flatAlt "{ " "{")
      (flatAlt " }" "}")
      ", "
    . prettyFields p

prettyFields :: (a -> Doc x) -> Map Field a -> [Doc x]
prettyFields p = map (\(f, x) -> prettyBS f <+> ":" <+> p x) . Map.toList

prettyBS :: (Coercible a ByteString) => a -> Doc x
prettyBS = fromString . BS8.unpack . coerce

putSpec :: Spec -> IO ()
putSpec = putDocW 72 . prettySpec
