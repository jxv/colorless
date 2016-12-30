{-# LANGUAGE OverloadedLists #-}
module Colorless.Parser.Declaration
  ( Declarations(..)
  , declaration
  , moduleOverrideDeclaration'
  ) where

import Colorless.Parser.Atomic
import Colorless.Parser.Types
import Colorless.Parser.Token
import Pregame

class Monad m => Declarations m where
  moduleOverrideDeclaration :: m ModuleOverrideDeclaration

declaration :: (Atomic m, Declarations m) => m Declaration
declaration = match
  [ DeclarationModuleOverride <$> moduleOverrideDeclaration
  ]

moduleOverrideDeclaration' :: Token m => m ModuleOverrideDeclaration
moduleOverrideDeclaration' = do
  initiateModuleOverride
  ModuleOverrideDeclaration <$> moduleReference <*> moduleVersion
