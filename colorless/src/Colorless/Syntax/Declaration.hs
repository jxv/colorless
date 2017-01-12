module Colorless.Syntax.Declaration
  ( Decls(..)
  , declaration
  , tagDeclToken'
  , fnDeclToken'
  , moduleOverrideDecl'
  ) where

import Pregame
import Colorless.Syntax.Combinator
import Colorless.Syntax.Token
import Colorless.Syntax.Types
import Control.Applicative ((*>))

class Monad m => Decls m where
  tagDeclToken :: m TagDecl
  fnDeclToken :: m FnDecl
  moduleOverrideDeclToken :: m ModuleOverrideDecl

declaration :: (Combinator m, Decls m) => m Decl
declaration = choice
  [ DeclTag <$> tagDeclToken
  , DeclFn <$> fnDeclToken
  , DeclModuleOverride <$> moduleOverrideDeclToken
  ]

tagDeclToken' :: Token m => m TagDecl
tagDeclToken' = do
  tag <- tagToken
  tags <- pure []
  return TagDecl
    { _tag = tag
    , _tags = tags
    }

fnDeclToken' :: Token m => m FnDecl
fnDeclToken' = do
  fn <- fnToken
  fnArgs <- pure []
  output <- tyToken
  tags <- pure []
  return FnDecl
    { _fn = fn
    , _args = fnArgs
    , _output = output
    , _tags = tags
    }

moduleOverrideDecl' :: Token m => m ModuleOverrideDecl
moduleOverrideDecl' = do
  initiateModuleOverride
  ModuleOverrideDecl <$> moduleRefToken <*> (moduleVersionSeparator *> moduleVersionToken)
