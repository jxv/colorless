module Colorless.Parser.Token
  ( Token(..)
  , initiateModuleOverride'
  , moduleReference'
  , moduleVersion'
  ) where

import Colorless.Parser.Atomic
import Colorless.Parser.Types
import Pregame

class Monad m => Token m where
  initiateModuleOverride :: m ()
  moduleReference :: m ModuleReference
  moduleVersion :: m ModuleVersion

initiateModuleOverride' :: Atomic m => m ()
initiateModuleOverride' = void $ literal "-"

moduleReference' :: Atomic m => m ModuleReference
moduleReference' = undefined

moduleVersion' :: Monad m => m ModuleVersion
moduleVersion' = undefined
