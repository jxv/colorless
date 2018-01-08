module Fluid.Cli.Main where

import Prelude (Unit, bind, pure, unit, (&&), (==), discard)
import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Args
import Fluid.Cli.Clojure (generateServer) as Clojure
import Fluid.Cli.Cpp (generateServer) as Cpp
import Fluid.Cli.Java (generateServer) as Java
import Fluid.Cli.JavaScript (generateClient) as JavaScript
import Fluid.Cli.Haskell (generateServer, generateClient) as Haskell
import Fluid.Cli.Node (generateServer) as Node
import Fluid.Cli.PureScript (generateServer, generateClient) as PureScript
import Fluid.Cli.Python (generateServer) as Python
import Fluid.Cli.Ruby (generateServer) as Ruby
import Fluid.Cli.Rust (generateServer) as Rust
import Fluid.Cli.Scala (generateServer) as Scala
import Fluid.Cli.Swift (generateServer) as Swift
import Fluid.Cli.Generator (generate)

import Fluid.Gen.Clojure.Conversion (conversion) as Clojure
import Fluid.Gen.Cpp.Conversion (conversion) as Cpp
import Fluid.Gen.Java.Conversion (conversion) as Java
import Fluid.Gen.JavaScript.Conversion (conversion) as JavaScript
import Fluid.Gen.Haskell.Conversion (conversion) as Haskell
import Fluid.Gen.Node.Conversion (conversion) as Node
import Fluid.Gen.PureScript.Conversion (conversion) as PureScript
import Fluid.Gen.Python.Conversion (conversion) as Python
import Fluid.Gen.Ruby.Conversion (conversion) as Ruby
import Fluid.Gen.Rust.Conversion (conversion) as Rust
import Fluid.Gen.Scala.Conversion (conversion) as Scala
import Fluid.Gen.Swift.Conversion (conversion) as Swift

import Fluid.Gen.Swift.Dependency (depFilter) as Swift

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  args <- liftEff' getArgs

  if args.lang == "clojure" && args.side == "server"
    then generate Clojure.conversion args Set.empty Clojure.generateServer else pure unit
  if args.lang == "cpp" && args.side == "server"
    then generate Cpp.conversion args Set.empty Cpp.generateServer else pure unit
  if args.lang == "java" && args.side == "server"
    then generate Java.conversion args Set.empty Java.generateServer else pure unit
  if args.lang == "javascript" && args.side == "client"
    then generate JavaScript.conversion args Set.empty JavaScript.generateClient else pure unit
  if args.lang == "haskell" && args.side == "server"
    then generate Haskell.conversion args Set.empty Haskell.generateServer else pure unit
  if args.lang == "haskell" && args.side == "client"
    then generate Haskell.conversion args Set.empty Haskell.generateClient else pure unit
  if args.lang == "node" && args.side == "server"
    then generate Node.conversion args Set.empty Node.generateServer else pure unit
  if args.lang == "python" && args.side == "server"
    then generate Python.conversion args Set.empty Python.generateServer else pure unit
  if args.lang == "ruby" && args.side == "server"
    then generate Ruby.conversion args Set.empty Ruby.generateServer else pure unit
  if args.lang == "rust" && args.side == "server"
    then generate Rust.conversion args Set.empty Rust.generateServer else pure unit
  if args.lang == "purescript" && args.side == "server"
    then generate PureScript.conversion args Set.empty PureScript.generateServer else pure unit
  if args.lang == "purescript" && args.side == "client"
    then generate PureScript.conversion args Set.empty PureScript.generateClient else pure unit
  if args.lang == "scala" && args.side == "server"
    then generate Scala.conversion args Set.empty Scala.generateServer else pure unit
  if args.lang == "swift" && args.side == "server"
    then generate Swift.conversion args Swift.depFilter Swift.generateServer else pure unit
