{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Spex.Parser where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Char (ord)
import Data.Coerce
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import FlatParse.Basic hiding (Parser, char, cut, runParser, string)

import Spex.Lexer
import Spex.Syntax

------------------------------------------------------------------------

type Name = BS.ByteString

{- | Parse an identifier. This parser uses `isKeyword` to check that an
   identifier is not a keyword.
-}
ident :: Parser Name
ident =
  token $
    byteStringOf $
      withSpan
        (identStartLowerChar *> skipMany identChar)
        (\_ span0 -> fails (isKeyword span0))

-- | Parse an identifier, throw a precise error on failure.
ident' :: Parser Name
ident' = ident `cut'` Msg "identifier"

bident :: Parser Name
bident =
  token $
    byteStringOf $
      withSpan
        (identStartUpperChar *> skipMany identChar)
        (\_ span0 -> fails (isKeyword span0))

bident' :: Parser Name
bident' = bident `cut'` Msg "Identifier"

digit :: Parser Int
digit = (\c -> ord c - ord '0') <$> satisfyAscii isDigit

int :: Parser Int
int = token $ do
  (place, n) <-
    chainr
      (\n (!place, !acc) -> (place * 10, acc + place * n))
      digit
      (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

int' :: Parser Int
int' = int `cut` [Lit "int"]

------------------------------------------------------------------------

specP :: Parser Spec
specP = do
  comp <- componentP'
  eof `cut` [Msg "end of input (lexical error)"]
  return (Spec comp)

componentP :: Parser Component
componentP = do
  $(keyword "component")
  x <- ComponentId <$> bident'
  $(keyword' "where")
  decls <- many (fmap Left typeDeclP <|> fmap Right (annP opDeclP))
  let (tys, ops) = partitionEithers decls
  return (Component x tys ops)

componentP' :: Parser Component
componentP' = componentP `cut` [Lit "component"]

typeDeclP :: Parser TypeDecl
typeDeclP = do
  $(keyword "type")
  x <- TypeId <$> bident'
  $(symbol' "=")
  ty <- typeP'
  return (TypeDecl x ty)

opDeclP :: Parser OpDecl
opDeclP = do
  x <- OpId <$> ident
  $(symbol' ":")
  m <- methodP'
  p <- pathSegmentsP'
  ws
  b <- bodyDeclP
  respTy <- responseTypeP
  return (Op x m p b respTy)

modalTypeP :: Parser Type
modalTypeP = abstractTypeP <|> uniqueTypeP <|> typeP
  where
    abstractTypeP = AbstractT <$ $(symbol "@") <*> typeP'
    uniqueTypeP = UniqueT <$ $(symbol "!") <*> typeP'

modalTypeP' :: Parser Type
modalTypeP' = modalTypeP `cut` ["type"]

typeP :: Parser Type
typeP = baseTypeP <|> recordDeclP <|> userTypeP
  where
    baseTypeP =
      token
        $( switch
            [|
              case _ of
                "Unit" -> pure UnitT
                "Bool" -> pure BoolT
                "Int" -> pure IntT
                "String" -> pure StringT
              |]
         )
    userTypeP = UserT <$> annP (TypeId <$> bident)

annP :: Parser a -> Parser (Ann a)
annP p = Ann <$> fmap coerce getPos <*> p

typeP' :: Parser Type
typeP' = typeP `cut` ["type"]

recordDeclP :: Parser Type
recordDeclP = do
  fs <- insideBraces (fieldDeclP' `sepEndBy` $(symbol ","))
  return (RecordT (Map.fromList fs))

fieldDeclP :: Parser (Field, Type)
fieldDeclP = (,) <$> fmap Field ident <* $(symbol' ":") <*> typeP'

fieldDeclP' :: Parser (Field, Type)
fieldDeclP' = fieldDeclP `cut` [Lit "record field declaration"]

bodyDeclP :: Parser (Maybe Type)
bodyDeclP = withOption bodyP (return . Just) (return Nothing)
  where
    bodyP :: Parser Type
    bodyP = modalTypeP

pathSegmentsP :: Parser [PathSegment Type]
pathSegmentsP = do
  $(symbol "/")
  pathSegmentP `sepBy` $(symbol "/")

pathSegmentsP' :: Parser [PathSegment Type]
pathSegmentsP' = pathSegmentsP `cut` ["path segments"]

pathSegmentP :: Parser (PathSegment Type)
pathSegmentP = pathP <|> holeP
  where
    pathP = Path <$> ident

    holeP = insideBraces $ do
      x <- ident'
      $(symbol' ":")
      ty <- modalTypeP'
      return (Hole x ty)

responseTypeP :: Parser Type
responseTypeP = nonUnitResponseP <|> unitResponseP
  where
    nonUnitResponseP = do
      $(symbol "->")
      typeP'

    unitResponseP = do
      ws
      return UnitT

methodP :: Parser Method
methodP =
  token
    $( switch
        [|
          case _ of
            "GET" -> pure Get
            "POST" -> pure Post
          |]
     )

methodP' :: Parser Method
methodP' = methodP `cut` ["method"]

------------------------------------------------------------------------

deploymentP :: Parser Deployment
deploymentP = do
  $(keyword "deployment")
  $(symbol' "{")
  $(symbol' "url:")
  host <- hostP
  -- let port = 8080
  -- let health = HealthCheckPath "/health"
  -- let reset = ResetPath "/reset"
  -- \$(symbol' ":")
  -- port <- portP'
  $(symbol' ",")
  $(symbol' "health:")
  healthPath <- HealthCheckPath <$> urlP
  $(symbol' ",")
  $(symbol' "reset:")
  resetPath <- ResetPath <$> urlP
  $(symbol' "}")
  return (Deployment (HostPort host 80) healthPath resetPath)

hostP :: Parser ByteString
hostP = urlP

urlP :: Parser ByteString
urlP = do
  $(symbol "\"")
  url <- some $ satisfyAscii $ \c ->
    isLatinLetter c
      || isDigit c
      || c `elem` (unreserved ++ reserved)
  $(symbol' "\"")
  return (BS8.pack url)
  where
    -- https://www.rfc-editor.org/rfc/rfc3986#section-2
    unreserved :: String
    unreserved = "-._~"

    reserved :: String
    reserved = genDelims ++ subDelims

    genDelims = ":/?#[]@"
    subDelims = "!$&'()*+,;="

portP' :: Parser Int
portP' = int `cut` [Lit "port"]

------------------------------------------------------------------------

-- * Utils

sepBy :: Parser a -> Parser sep -> Parser [a]
{-# INLINEABLE sepBy #-}
sepBy p sep = sepBy1 p sep <|> return []

sepBy1 :: Parser a -> Parser sep -> Parser [a]
{-# INLINEABLE sepBy1 #-}
sepBy1 p sep = do
  x <- p
  xs <- many (sep >> p)
  return (x : xs)

sepEndBy1 :: Parser a -> Parser sep -> Parser [a]
{-# INLINEABLE sepEndBy1 #-}
sepEndBy1 p sep = do
  x <- p
  ( do
      _ <- sep
      xs <- sepEndBy p sep
      return (x : xs)
      <|> return [x]
    )

sepEndBy :: Parser a -> Parser sep -> Parser [a]
{-# INLINEABLE sepEndBy #-}
sepEndBy p sep = sepEndBy1 p sep <|> return []

insideBraces :: Parser a -> Parser a
insideBraces p = do
  $(symbol "{")
  x <- p
  $(symbol' "}")
  return x

--------------------------------------------------------------------------------

-- * Examples

t :: IO ()
t = testParser specP p1

p1 :: String
p1 =
  unlines
    [ "component Foo where"
    , ""
    , "addPet : POST /pet !Pet"
    , "getPet : GET /pet/{petId : @Int} -> Pet"
    , ""
    , "type Pet = { petId : Int, petName : String }"
    ]

-- "deployment {",
-- "  url:    \"http://localhost:8080\",",
-- "  health: \"/health\",",
-- "  reset:  \"/reset\"",
-- "}"
