-- Module
module Phonebook.Major0
  ( phonebook'version
  , phonebook'pull
  , phonebook'handler
  , phonebook'spec
  , Phonebook'Thrower(..)
  , Phonebook'Service(..)
  , PersonId(..)
  , Name(..)
  , Phone(..)
  , Street(..)
  , City(..)
  , Zipcode(..)
  , Address(..)
  , Person(..)
  , LookupPerson(..)
  , LookupPersonByName(..)
  , InsertPerson(..)
  , State(..)
  , State'Other'Members(..)
  ) where

-- Imports
import Prelude as P
import Control.Monad as P
import Control.Monad.Except as M
import Data.IORef as IO
import Data.String as P (IsString)
import Fluid.Imports as R
import Fluid.Server as C

--------------------------------------------------------
-- Configs
--------------------------------------------------------

-- Version
phonebook'version :: C.Version
phonebook'version = C.Version 0 0

phonebook'pull :: C.Pull
phonebook'pull = C.Pull "http" "127.0.0.1" "/" 8000

--------------------------------------------------------
-- Interfaces
--------------------------------------------------------

-- Thrower
class C.ServiceThrower m <= Phonebook'Thrower m where
  phonebook'throw :: () -> m a
  phonebook'throw = C.serviceThrow P.. R.toJSON P.. C.toVal

-- Service
class P.Monad m <= Phonebook'Service meta m where
  phonebook'LookupPerson :: meta -> LookupPerson -> m (P.Maybe Person)
  phonebook'LookupPersonByName :: meta -> LookupPersonByName -> m [Person]
  phonebook'InsertPerson :: meta -> InsertPerson -> m PersonId

instance Phonebook'Service meta m => Phonebook'Service meta (M.ExceptT C.Response m) where
  phonebook'LookupPerson _meta = M.lift P.. phonebook'LookupPerson _meta
  phonebook'LookupPersonByName _meta = M.lift P.. phonebook'LookupPersonByName _meta
  phonebook'InsertPerson _meta = M.lift P.. phonebook'InsertPerson _meta

--------------------------------------------------------
-- Types
--------------------------------------------------------

-- Wrap: PersonId
newtype PersonId = PersonId R.Text

-- Wrap: Name
newtype Name = Name R.Text

-- Wrap: Phone
newtype Phone = Phone R.Text

-- Wrap: Street
newtype Street = Street R.Text

-- Wrap: City
newtype City = City R.Text

-- Wrap: Zipcode
newtype Zipcode = Zipcode R.Text

-- Struct: Address
data Address = Address
  { street :: Street
  , city :: City
  , zipcode :: Zipcode
  , state :: State
  }

-- Struct: Person
data Person = Person
  { name :: Name
  , phone :: Phone
  , address :: (P.Maybe Address)
  , friends :: [PersonId]
  }

-- Struct: LookupPerson
data LookupPerson = LookupPerson
  { id :: PersonId
  }

-- Struct: LookupPersonByName
data LookupPersonByName = LookupPersonByName
  { name :: Name
  }

-- Struct: InsertPerson
data InsertPerson = InsertPerson
  { person :: Person
  }

-- Enumeration: State
data State
  = State'CA
  | State'NY
  | State'TX
  | State'Other State'Other'Members

data State'Other'Members = State'Other'Members
  { state'OtherName :: R.Text
  }

--------------------------------------------------------
-- Add-ons
--------------------------------------------------------

--------------------------------------------------------
-- Request handling
--------------------------------------------------------

-- Handler
phonebook'handler
  :: (Phonebook'Service meta m, R.MonadIO m, R.MonadCatch m)
  => (xtra -> C.Hooks m () meta)
  -> xtra
  -> C.Request
  -> m (P.Either C.Response C.Response)
phonebook'handler _hooksBuilder xtra C.Request{C.meta=meta, C.query=query} = R.catch
  (M.runExceptT P.$ do
    meta' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableMeta) P.return (C.fromValFromJson meta)
    let _hooks = _hooksBuilder xtra
    xformMeta <- M.lift P.$ C.metaMiddleware _hooks meta'
    envRef <- R.liftIO C.emptyEnv
    variableBaseCount <- R.liftIO (R.size P.<$> IO.readIORef envRef)
    _limits <- M.lift P.$ C.sandboxLimits _hooks xformMeta
    let _limits' = _limits
          { C.variables = P.fmap (P.+ variableBaseCount) (C.variables _limits)
          }
    _serviceCallCountRef <- R.liftIO (IO.newIORef 0)
    _lambdaCountRef <- R.liftIO (IO.newIORef 0)
    _exprCountRef <- R.liftIO (IO.newIORef 0)
    let evalConfig = C.EvalConfig
          { C.limits = _limits'
          , C.langServiceCallCount = _serviceCallCountRef
          , C.langLambdaCount = _lambdaCountRef
          , C.langExprCount = _exprCountRef
          , C.apiCall = phonebook'ApiCall xformMeta
          }
    query' <- P.maybe (C.runtimeThrow C.RuntimeError'UnparsableQuery) P.return (C.jsonToExpr query)
    vals <- C.runEval (C.forceVal P.=<< C.eval query' envRef) evalConfig
    P.return P.$ C.Response'Success (R.toJSON vals) _limits)
  (\(C.ThrownValue _err) -> P.return P.. P.Left P.$ C.Response'Error (C.ResponseError'Service _err))

-- API
phonebook'ApiCall :: (Phonebook'Service meta m, C.ServiceThrower m, C.RuntimeThrower m) => meta -> C.ApiCall -> m C.Val
phonebook'ApiCall meta' apiCall' = case C.parseApiCall phonebook'ApiParser apiCall' of
  P.Nothing -> C.runtimeThrow (C.RuntimeError'UnrecognizedCall P.$ C.apiCallName apiCall')
  P.Just x' -> case x' of
    Phonebook'Api'LookupPerson a' -> C.toVal P.<$> phonebook'LookupPerson meta' a'
    Phonebook'Api'LookupPersonByName a' -> C.toVal P.<$> phonebook'LookupPersonByName meta' a'
    Phonebook'Api'InsertPerson a' -> C.toVal P.<$> phonebook'InsertPerson meta' a'

-- API Parser
phonebook'ApiParser :: C.ApiParser Phonebook'Api
phonebook'ApiParser = C.ApiParser
  { C.hollow = R.empty
  , C.struct = R.fromList
     [ ("LookupPerson", v Phonebook'Api'LookupPerson)
     , ("LookupPersonByName", v Phonebook'Api'LookupPersonByName)
     , ("InsertPerson", v Phonebook'Api'InsertPerson)
     ]
  , C.enumeration = R.empty
  , C.wrap = R.empty
  }
  where
    v x y = x P.<$> C.fromVal y

-- Api
data Phonebook'Api
  = Phonebook'Api'LookupPerson LookupPerson
  | Phonebook'Api'LookupPersonByName LookupPersonByName
  | Phonebook'Api'InsertPerson InsertPerson

--------------------------------------------------------
-- Type Instances
--------------------------------------------------------

instance C.ToVal PersonId where
  toVal (PersonId _w) = C.toVal _w

instance C.FromVal PersonId where
  fromVal _v = PersonId P.<$> C.fromVal _v

instance R.ToJSON PersonId where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON PersonId where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Name where
  toVal (Name _w) = C.toVal _w

instance C.FromVal Name where
  fromVal _v = Name P.<$> C.fromVal _v

instance R.ToJSON Name where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Name where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Phone where
  toVal (Phone _w) = C.toVal _w

instance C.FromVal Phone where
  fromVal _v = Phone P.<$> C.fromVal _v

instance R.ToJSON Phone where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Phone where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Street where
  toVal (Street _w) = C.toVal _w

instance C.FromVal Street where
  fromVal _v = Street P.<$> C.fromVal _v

instance R.ToJSON Street where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Street where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal City where
  toVal (City _w) = C.toVal _w

instance C.FromVal City where
  fromVal _v = City P.<$> C.fromVal _v

instance R.ToJSON City where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON City where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Zipcode where
  toVal (Zipcode _w) = C.toVal _w

instance C.FromVal Zipcode where
  fromVal _v = Zipcode P.<$> C.fromVal _v

instance R.ToJSON Zipcode where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Zipcode where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Address where
  toVal Address
    { street
    , city
    , zipcode
    , state
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("street", C.toVal street)
    , ("city", C.toVal city)
    , ("zipcode", C.toVal zipcode)
    , ("state", C.toVal state)
    ]

instance C.FromVal Address where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Address
      P.<$> C.getMember _m "street"
      P.<*> C.getMember _m "city"
      P.<*> C.getMember _m "zipcode"
      P.<*> C.getMember _m "state"
    _ -> P.Nothing

instance R.ToJSON Address where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Address where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal Person where
  toVal Person
    { name
    , phone
    , address
    , friends
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal name)
    , ("phone", C.toVal phone)
    , ("address", C.toVal address)
    , ("friends", C.toVal friends)
    ]

instance C.FromVal Person where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> Person
      P.<$> C.getMember _m "name"
      P.<*> C.getMember _m "phone"
      P.<*> C.getMember _m "address"
      P.<*> C.getMember _m "friends"
    _ -> P.Nothing

instance R.ToJSON Person where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON Person where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal LookupPerson where
  toVal LookupPerson
    { id
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("id", C.toVal id)
    ]

instance C.FromVal LookupPerson where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> LookupPerson
      P.<$> C.getMember _m "id"
    _ -> P.Nothing

instance R.ToJSON LookupPerson where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPerson where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal LookupPersonByName where
  toVal LookupPersonByName
    { name
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("name", C.toVal name)
    ]

instance C.FromVal LookupPersonByName where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> LookupPersonByName
      P.<$> C.getMember _m "name"
    _ -> P.Nothing

instance R.ToJSON LookupPersonByName where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON LookupPersonByName where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal InsertPerson where
  toVal InsertPerson
    { person
    } = C.Val'ApiVal P.$ C.ApiVal'Struct P.$ C.Struct P.$ R.fromList
    [ ("person", C.toVal person)
    ]

instance C.FromVal InsertPerson where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Struct (C.Struct _m)) -> InsertPerson
      P.<$> C.getMember _m "person"
    _ -> P.Nothing

instance R.ToJSON InsertPerson where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON InsertPerson where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

instance C.ToVal State where
  toVal = \case
    State'CA -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "CA" P.Nothing
    State'NY -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "NY" P.Nothing
    State'TX -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "TX" P.Nothing
    State'Other State'Other'Members
      { state'OtherName
      } -> C.Val'ApiVal P.$ C.ApiVal'Enumeral P.$ C.Enumeral "Other" P.$ P.Just P.$ R.fromList
      [ ("name", C.toVal state'OtherName)
      ]

instance C.FromVal State where
  fromVal = \case
    C.Val'ApiVal (C.ApiVal'Enumeral (C.Enumeral _tag _m)) -> case (_tag,_m) of
      ("CA", P.Nothing) -> P.Just State'CA
      ("NY", P.Nothing) -> P.Just State'NY
      ("TX", P.Nothing) -> P.Just State'TX
      ("Other", P.Just _m') -> State'Other P.<$> (State'Other'Members
          P.<$> C.getMember _m' "name"
        )
      _ -> P.Nothing
    _ -> P.Nothing

instance R.ToJSON State where
  toJSON = R.toJSON P.. C.toVal

instance R.FromJSON State where
  parseJSON _v = do
    _x <- R.parseJSON _v
    case C.fromVal _x of
      P.Nothing -> P.mzero
      P.Just _y -> P.return _y

--------------------------------------------------------
-- Spec
--------------------------------------------------------

phonebook'spec :: R.Value
phonebook'spec = v
  where P.Just v = R.decode "{\"fluid\":{\"major\":0,\"minor\":0},\"schema\":{\"PersonId\":\"String\",\"Name\":\"String\",\"Phone\":\"String\",\"Street\":\"String\",\"City\":\"String\",\"State\":[\"CA\",\"NY\",\"TX\",{\"tag\":\"Other\",\"m\":[{\"name\":\"String\"}]}],\"Zipcode\":\"String\",\"Address\":{\"m\":[{\"street\":\"Street\"},{\"city\":\"City\"},{\"zipcode\":\"Zipcode\"},{\"state\":\"State\"}]},\"Person\":{\"m\":[{\"name\":\"Name\"},{\"phone\":\"Phone\"},{\"address\":{\"n\":\"Option\",\"p\":\"Address\"}},{\"friends\":{\"n\":\"List\",\"p\":\"PersonId\"}}]},\"LookupPerson\":{\"m\":[{\"id\":\"PersonId\"}],\"o\":{\"n\":\"Option\",\"p\":\"Person\"}},\"LookupPersonByName\":{\"m\":[{\"name\":\"Name\"}],\"o\":{\"n\":\"List\",\"p\":\"Person\"}},\"InsertPerson\":{\"m\":[{\"person\":\"Person\"}],\"o\":\"PersonId\"}},\"pull\":{\"protocol\":\"http\",\"name\":\"Phonebook\",\"host\":\"127.0.0.1\",\"path\":\"/\",\"port\":8000,\"error\":\"Unit\",\"meta\":\"Unit\"},\"version\":{\"major\":0,\"minor\":0}}"
