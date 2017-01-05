module Colorless.Parser.Declaration
  ( Declarations(..)
  , declaration
  , tagDeclarationToken'
  , functionDeclarationToken'
  , moduleOverrideDeclaration'
  ) where

import Colorless.Parser.Combinator
import Colorless.Parser.Token
import Colorless.Parser.Prototypes
import Pregame
import Control.Applicative ((*>))

class Monad m => Declarations m where
  tagDeclarationToken :: m TagDeclaration
  functionDeclarationToken :: m FunctionDeclaration
  moduleOverrideDeclarationToken :: m ModuleOverrideDeclaration

declaration :: (Combinator m, Declarations m) => m Declaration
declaration = choice
  [ DeclarationTag <$> tagDeclarationToken
  , DeclarationFunction <$> functionDeclarationToken
  , DeclarationModuleOverride <$> moduleOverrideDeclarationToken
  ]

tagDeclarationToken' :: Token m => m TagDeclaration
tagDeclarationToken' = do
  tag <- tagToken
  tags <- pure []
  return TagDeclaration
    { _tag = tag
    , _tags = tags
    }

functionDeclarationToken' :: Token m => m FunctionDeclaration
functionDeclarationToken' = do
  function <- functionToken
  functionParameters <- pure []
  output <- typeToken
  tags <- pure []
  return FunctionDeclaration
    { _function = function
    , _parameters = functionParameters
    , _output = output
    , _tags = tags
    }

moduleOverrideDeclaration' :: Token m => m ModuleOverrideDeclaration
moduleOverrideDeclaration' = do
  initiateModuleOverride
  ModuleOverrideDeclaration <$> moduleReferenceToken <*> (moduleVersionSeparator *> moduleVersionToken)
