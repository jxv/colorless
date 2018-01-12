module Fluid.Cli.Main where

import Prelude (Unit, bind, (&&), (==), discard)
import Control.Monad (when)
import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)

import Fluid.Cli.Args (getArgs)
import Fluid.Cli.Clojure (generateServer) as Clojure
import Fluid.Cli.Cpp (generateServer) as Cpp
import Fluid.Cli.Crystal (generateServer) as Crystal
import Fluid.Cli.CSharp (generateServer) as CSharp
import Fluid.Cli.D (generateServer) as D
import Fluid.Cli.Elm (generateServer) as Elm
import Fluid.Cli.FSharp (generateServer) as FSharp
import Fluid.Cli.Go (generateServer) as Go
import Fluid.Cli.Java (generateServer) as Java
import Fluid.Cli.JavaScript (generateClient) as JavaScript
import Fluid.Cli.Kotlin (generateServer) as Kotlin
import Fluid.Cli.OCaml (generateServer) as OCaml
import Fluid.Cli.Haskell (generateServer, generateClient) as Haskell
import Fluid.Cli.Idris (generateServer) as Idris
import Fluid.Cli.Node (generateServer) as Node
import Fluid.Cli.PureScript (generateServer, generateClient) as PureScript
import Fluid.Cli.Python (generateServer) as Python
import Fluid.Cli.Ruby (generateServer) as Ruby
import Fluid.Cli.Rust (generateServer) as Rust
import Fluid.Cli.Scala (generateServer) as Scala
import Fluid.Cli.Swift (generateServer) as Swift

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  args <- liftEff' getArgs

  when (args.lang == "clojure" && args.side == "server") (Clojure.generateServer args)
  when (args.lang == "cpp" && args.side == "server") (Cpp.generateServer args)
  when (args.lang == "crystal" && args.side == "server") (Crystal.generateServer args)
  when (args.lang == "csharp" && args.side == "server") (CSharp.generateServer args)
  when (args.lang == "d" && args.side == "server") (D.generateServer args)
  when (args.lang == "elm" && args.side == "server") (Elm.generateServer args)
  when (args.lang == "fsharp" && args.side == "server") (FSharp.generateServer args)
  when (args.lang == "go" && args.side == "server") (Go.generateServer args)
  when (args.lang == "haskell" && args.side == "server") (Haskell.generateServer args)
  when (args.lang == "haskell" && args.side == "client") (Haskell.generateClient args)
  when (args.lang == "idris" && args.side == "server") (Idris.generateServer args)
  when (args.lang == "java" && args.side == "server") (Java.generateServer args)
  when (args.lang == "javascript" && args.side == "client") (JavaScript.generateClient args)
  when (args.lang == "kotlin" && args.side == "server") (Kotlin.generateServer args)
  when (args.lang == "ocaml" && args.side == "server") (OCaml.generateServer args)
  when (args.lang == "node" && args.side == "server") (Node.generateServer args)
  when (args.lang == "python" && args.side == "server") (Python.generateServer args)
  when (args.lang == "ruby" && args.side == "server") (Ruby.generateServer args)
  when (args.lang == "rust" && args.side == "server") (Rust.generateServer args)
  when (args.lang == "purescript" && args.side == "server") (PureScript.generateServer args)
  when (args.lang == "purescript" && args.side == "client") (PureScript.generateClient args)
  when (args.lang == "scala" && args.side == "server") (Scala.generateServer args)
  when (args.lang == "swift" && args.side == "server") (Swift.generateServer args)
