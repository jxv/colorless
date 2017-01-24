{-# LANGUAGE ConstraintKinds #-}
module Colorless.Parser.Syntax
  ( runSyntaxM
  , satisfy
  , fnName
  , fnDef
  , primTy
  ) where

import Pregame
import Control.Applicative ((*>), (<*))
import Data.Either.Combinators (rightToMaybe)
import Colorless.Parser.Types
import qualified Text.Megaparsec as P
import Text.Megaparsec.Prim (MonadParsec)
import qualified Data.Set as Set

newtype SyntaxM a = SyntaxM (P.ParsecT ParserError [LexToken] IO a)
  deriving (Functor, Applicative, Monad, MonadIO, Alternative, MonadPlus, MonadParsec ParserError [LexToken])

runSyntaxM :: SyntaxM a -> [LexToken] -> IO (Either (P.ParseError (P.Token [LexToken]) ParserError) a)
runSyntaxM (SyntaxM m) input = P.runParserT m "" input

type SyntaxParser = MonadParsec ParserError [LexToken]

satisfy :: SyntaxParser m => (LexToken -> Bool) -> m LexToken
satisfy f = P.token test (Just (LexToken NoLoc LexEOI))
  where
    test x =
      if f x
        then Right x
        else Left (Set.singleton (P.Tokens (x :| [])), Set.empty, Set.empty)

lowerCamelCase :: SyntaxParser m => m Text
lowerCamelCase = do
  (LexToken _ (LexLowerCamelCase t)) <- satisfy $ \(LexToken _ l) -> case l of
    LexLowerCamelCase _ -> True
    _ -> False
  return t

fnName :: SyntaxParser m => m FnName
fnName = FnName <$> lowerCamelCase

colon :: SyntaxParser m => m ()
colon = do
  void . satisfy $ \(LexToken _ l) -> l == LexColon
  return ()

fnDef :: SyntaxParser m => m FnDef
fnDef = FnDef
  <$> P.manyTill arg colon
  <*> fmap MonoTyRefPrimTy primTy
  <*> pure mempty

arg :: SyntaxParser m => m (ArgName, MonoTyRef)
arg = do
  p <- primTy
  return (primTyToArgName p, MonoTyRefPrimTy p)

primTyToArgName :: PrimTy -> ArgName
primTyToArgName = \case
  PrimTyUnit -> "unit"
  PrimTyU8 -> "u8"
  PrimTyStr -> "str"
  _ -> "a"

primTy :: SyntaxParser m => m PrimTy
primTy = P.choice $ map (uncurryN lowerCamelCaseToken)
  [ ("unit", PrimTyUnit)
  , ("u8", PrimTyU8)
  , ("str", PrimTyStr)
  ]

lowerCamelCaseToken :: SyntaxParser m => Text -> a -> m a
lowerCamelCaseToken u a = P.token test Nothing
  where
    err x = Left (Set.singleton (P.Tokens (x:|[])), Set.empty, Set.empty)
    test x@LexToken{ _lex = LexLowerCamelCase t } = if t == u then Right a else err x
    test x = err x
