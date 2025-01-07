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
import Data.Vector (Vector)
import Data.Vector qualified as Vector
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
    <+> prettyId comp.id
    <+> "where"
    <> line
    <> line
    <> vsep (map prettyTypeDecl comp.typeDecls)
    <> vsep (map (prettyOpDecl . item) comp.opDecls)

prettyTypeDecl :: TypeDecl -> Doc x
prettyTypeDecl tyDecl =
  "type" <+> prettyId tyDecl.typeId <+> "=" <+> prettyType tyDecl.rhs

displayDeployment :: Deployment -> Text
displayDeployment d =
  d.hostPort.host
    <> ":"
    <> Text.pack (show d.hostPort.port)

displayTypeId :: TypeId -> Text
displayTypeId (TypeId tid) = tid

prettyOpId :: OpId -> Doc x
prettyOpId (OpId oid) = pretty oid

prettyOpF :: (parameter -> Doc x) -> OpF parameter a -> Doc x
prettyOpF pp op =
  hsep $
    [ prettyOpId op.id
    , ":"
    , pp op.parameter
    , prettyResponseType op.responseType
    ]

prettyHttpParameter ::
  (a -> Doc x) -> (a -> Doc x) -> HttpParameter a -> Doc x
prettyHttpParameter pp pb httpParameter =
  hsep $
    [ prettyMethod httpParameter.method
    , prettyPathF pp httpParameter.path
    ]
      ++ ( case httpParameter.body of
            Nothing -> []
            Just x -> [pb x]
         )

displayOp :: Op -> Text
displayOp = renderStrict . layoutPretty defaultLayoutOptions . prettyOp

displayOps :: [Op] -> Text
displayOps = renderStrict . layoutPretty defaultLayoutOptions . prettyOps

prettyOps :: [Op] -> Doc x
prettyOps = vcat . map prettyOp

prettyOp :: Op -> Doc x
prettyOp =
  prettyOpF
    (prettyHttpParameter (\val -> "=" <+> prettyValue val) prettyValue)

prettyOpDecl :: OpDecl -> Doc x
prettyOpDecl =
  prettyOpF
    (prettyHttpParameter (\ty -> ":" <+> prettyType ty) prettyType)

prettyMethod :: Method -> Doc x
prettyMethod Get = "GET"
prettyMethod Post = "POST"
prettyMethod Put = "PUT"
prettyMethod Delete = "DELETE"

prettyPathF :: (a -> Doc x) -> [PathSegment a] -> Doc x
prettyPathF pp = align . hcat . zipWith (<>) (repeat "/") . map aux
  where
    aux (Path t) = pretty t
    aux (Hole p x) = "{" <> pretty p <+> pp x <> "}"

prettyType :: Type -> Doc x
prettyType UnitT = "Unit"
prettyType BoolT = "Bool"
prettyType StringT = "String"
prettyType IntT = "Int"
prettyType (ArrayT tys) = error "prettyType: not implemented yet!"
prettyType (RecordT r) = prettyRecord (\ty -> ":" <+> prettyType ty) r
prettyType (UserT tid) = prettyId tid.item
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
prettyFields p = map (\(Field f, x) -> pretty f <+> p x) . Map.toList

prettyArray :: (a -> Doc x) -> Vector a -> Doc x
prettyArray p = list . map p . Vector.toList

prettyBS :: (Coercible a ByteString) => a -> Doc x
prettyBS = fromString . BS8.unpack . coerce

prettyId :: (Coercible a Text) => a -> Doc x
prettyId = fromString . Text.unpack . coerce

------------------------------------------------------------------------

prettyValue :: Value -> Doc x
prettyValue UnitV = "()"
prettyValue (BoolV b) = viaShow b
prettyValue (IntV i) = viaShow i
prettyValue (StringV t) = pretty t
prettyValue (ArrayV vs) = prettyArray prettyValue vs
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
