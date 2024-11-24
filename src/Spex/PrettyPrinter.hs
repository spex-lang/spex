{-# LANGUAGE OverloadedStrings #-}

module Spex.PrettyPrinter where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BS8
import Data.Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.String
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Prettyprinter
import Prettyprinter.Render.Text
import System.IO

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

displayDeployment :: Deployment -> Text
displayDeployment d =
  Text.decodeUtf8Lenient d.hostPort.host
    <> ":"
    <> Text.pack (show d.hostPort.port)

displayTypeId :: TypeId -> Text
displayTypeId (TypeId bs) = Text.decodeUtf8Lenient bs

prettyOpF :: (a -> Doc x) -> (a -> Doc x) -> OpF a -> Doc x
prettyOpF pp pb op =
  hsep $
    [ prettyBS op.id
    , ":"
    , prettyMethod op.method
    , prettyPathF pp op.path
    ]
      ++ ( case op.body of
            Nothing -> []
            Just x -> [pb x]
         )
      ++ [ prettyResponseType op.responseType
         ]

displayOp :: Op -> Text
displayOp = renderStrict . layoutPretty defaultLayoutOptions . prettyOp

displayOps :: [Op] -> Text
displayOps = renderStrict . layoutPretty defaultLayoutOptions . prettyOps

prettyOps :: [Op] -> Doc x
prettyOps = vcat . map prettyOp

prettyOp :: Op -> Doc x
prettyOp = prettyOpF (\val -> "=" <+> prettyValue val) prettyValue

prettyOpDecl :: OpDecl -> Doc x
prettyOpDecl = prettyOpF (\ty -> ":" <+> prettyType ty) prettyType

prettyMethod :: Method -> Doc x
prettyMethod Get = "GET"
prettyMethod Post = "POST"
prettyMethod Put = "PUT"
prettyMethod Delete = "DELETE"

prettyPathF :: (a -> Doc x) -> [PathSegment a] -> Doc x
prettyPathF pp = align . hcat . zipWith (<>) (repeat "/") . map aux
  where
    aux (Path bs) = prettyBS bs
    aux (Hole p x) = "{" <> prettyBS p <+> pp x <> "}"

prettyType :: Type -> Doc x
prettyType UnitT = "Unit"
prettyType BoolT = "Bool"
prettyType StringT = "String"
prettyType IntT = "Int"
prettyType (ArrayT tys) = error "prettyType: not implemented yet!"
prettyType (RecordT r) = prettyRecord (\ty -> ":" <+> prettyType ty) r
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
prettyFields p = map (\(f, x) -> prettyBS f <+> p x) . Map.toList

prettyBS :: (Coercible a ByteString) => a -> Doc x
prettyBS = fromString . BS8.unpack . coerce

------------------------------------------------------------------------

prettyValue :: Value -> Doc x
prettyValue UnitV = "()"
prettyValue (BoolV b) = viaShow b
prettyValue (IntV i) = viaShow i
prettyValue (StringV t) = pretty t
prettyValue (ArrayV vs) = undefined
prettyValue (RecordV fvs) = prettyRecord (\val -> "=" <+> prettyValue val) fvs

displayValue :: Value -> Text
displayValue =
  renderStrict
    . layoutPretty defaultLayoutOptions
    . prettyValue

------------------------------------------------------------------------

putSpec :: Spec -> IO ()
putSpec = hPutSpec stdout

hPutSpec :: Handle -> Spec -> IO ()
hPutSpec h = hPutDoc h . prettySpec
