{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser.Declaration
  ( Declarations(..)
  , declaration
  , moduleOverrideDeclaration'
  ) where

import Colorless.Parser.Combinator
import Colorless.Parser.Token
import Colorless.Parser.Types
import Pregame
import Control.Applicative ((*>))

class Monad m => Declarations m where
  moduleOverrideDeclarationToken :: m ModuleOverrideDeclaration

declaration :: (Combinator m, Declarations m) => m Declaration
declaration = choice
  [ DeclarationModuleOverride <$> moduleOverrideDeclarationToken
  ]

moduleOverrideDeclaration' :: Token m => m ModuleOverrideDeclaration
moduleOverrideDeclaration' = do
  initiateModuleOverride
  ModuleOverrideDeclaration <$> moduleReferenceToken <*> (moduleVersionSeparator *> moduleVersionToken)
