module Fluid.Cli.Main where

import Prelude (Unit, bind, (&&), (==), discard)
import Control.Monad (when)
import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)
import Data.Set as Set

import Fluid.Cli.Args
import Fluid.Cli.Clojure (generateServer) as Clojure
import Fluid.Cli.Cpp (generateServer) as Cpp
import Fluid.Cli.Elm (generateServer) as Elm
import Fluid.Cli.Go (generateServer) as Go
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
import Fluid.Gen.Elm.Conversion (conversion) as Elm
import Fluid.Gen.Go.Conversion (conversion) as Go
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

  when (args.lang == "clojure" && args.side == "server") (generate Clojure.conversion args Set.empty Clojure.generateServer)
  when (args.lang == "cpp" && args.side == "server") (generate Cpp.conversion args Set.empty Cpp.generateServer)
  when (args.lang == "elm" && args.side == "server") (generate Elm.conversion args Set.empty Elm.generateServer)
  when (args.lang == "go" && args.side == "server") (generate Go.conversion args Set.empty Go.generateServer)
  when (args.lang == "java" && args.side == "server") (generate Java.conversion args Set.empty Java.generateServer)
  when (args.lang == "javascript" && args.side == "client") (generate JavaScript.conversion args Set.empty JavaScript.generateClient)
  when (args.lang == "haskell" && args.side == "server") (generate Haskell.conversion args Set.empty Haskell.generateServer)
  when (args.lang == "haskell" && args.side == "client") (generate Haskell.conversion args Set.empty Haskell.generateClient)
  when (args.lang == "node" && args.side == "server") (generate Node.conversion args Set.empty Node.generateServer)
  when (args.lang == "python" && args.side == "server") (generate Python.conversion args Set.empty Python.generateServer)
  when (args.lang == "ruby" && args.side == "server") (generate Ruby.conversion args Set.empty Ruby.generateServer)
  when (args.lang == "rust" && args.side == "server") (generate Rust.conversion args Set.empty Rust.generateServer)
  when (args.lang == "purescript" && args.side == "server") (generate PureScript.conversion args Set.empty PureScript.generateServer)
  when (args.lang == "purescript" && args.side == "client") (generate PureScript.conversion args Set.empty PureScript.generateClient)
  when (args.lang == "scala" && args.side == "server") (generate Scala.conversion args Set.empty Scala.generateServer)
  when (args.lang == "swift" && args.side == "server") (generate Swift.conversion args Swift.depFilter Swift.generateServer)
