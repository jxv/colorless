{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser.Declaration
  ( Declarations(..)
  , declaration
  , tagDeclarationToken'
  , moduleOverrideDeclaration'
  ) where

import Colorless.Parser.Combinator
import Colorless.Parser.Token
import Colorless.Parser.Types
import Pregame
import Control.Applicative ((*>))

class Monad m => Declarations m where
  tagDeclarationToken :: m TagDeclaration
  moduleOverrideDeclarationToken :: m ModuleOverrideDeclaration

declaration :: (Combinator m, Declarations m) => m Declaration
declaration = choice
  [ DeclarationTag <$> tagDeclarationToken
  , DeclarationModuleOverride <$> moduleOverrideDeclarationToken
  ]

tagDeclarationToken' :: Token m => m TagDeclaration
tagDeclarationToken' = do
  tag <- tagToken
  return TagDeclaration { _tag = tag, _tags = [] }

moduleOverrideDeclaration' :: Token m => m ModuleOverrideDeclaration
moduleOverrideDeclaration' = do
  initiateModuleOverride
  ModuleOverrideDeclaration <$> moduleReferenceToken <*> (moduleVersionSeparator *> moduleVersionToken)
