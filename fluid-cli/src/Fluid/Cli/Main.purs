module Fluid.Cli.Main where

import Prelude (Unit, bind, pure, unit, (&&), (==), discard)
import Control.Monad.Aff (Fiber, launchAff, liftEff')
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff (Eff)
import Node.FS.Aff (FS)

import Fluid.Cli.Args
import Fluid.Cli.Clojure (generateClojureServer)
import Fluid.Cli.Java (generateJavaServer)
import Fluid.Cli.JavaScript (generateJavaScriptClient)
import Fluid.Cli.Haskell (generateHaskellServer, generateHaskellClient)
import Fluid.Cli.Node (generateNodeServer)
import Fluid.Cli.PureScript (generatePureScriptServer, generatePureScriptClient)
import Fluid.Cli.Python (generatePythonServer)
import Fluid.Cli.Ruby (generateRubyServer)
import Fluid.Cli.Rust (generateRustServer)
import Fluid.Cli.Scala (generateScalaServer)
import Fluid.Cli.Swift (generateSwiftServer)
import Fluid.Cli.Generator (generate)

main :: forall eff. Eff (fs :: FS, console :: CONSOLE | eff) (Fiber (fs :: FS, console :: CONSOLE | eff) Unit)
main = launchAff do
  args <- liftEff' getArgs

  if args.lang == "clojure" && args.side == "server"
    then generate args generateClojureServer else pure unit
  if args.lang == "java" && args.side == "server"
    then generate args generateJavaServer else pure unit
  if args.lang == "javascript" && args.side == "client"
    then generate args generateJavaScriptClient else pure unit
  if args.lang == "haskell" && args.side == "server"
    then generate args generateHaskellServer else pure unit
  if args.lang == "haskell" && args.side == "client"
    then generate args generateHaskellClient else pure unit
  if args.lang == "node" && args.side == "server"
    then generate args generateNodeServer else pure unit
  if args.lang == "python" && args.side == "server"
    then generate args generatePythonServer else pure unit
  if args.lang == "ruby" && args.side == "server"
    then generate args generateRubyServer else pure unit
  if args.lang == "rust" && args.side == "server"
    then generate args generateRustServer else pure unit
  if args.lang == "purescript" && args.side == "server"
    then generate args generatePureScriptServer else pure unit
  if args.lang == "purescript" && args.side == "client"
    then generate args generatePureScriptClient else pure unit
  if args.lang == "scala" && args.side == "server"
    then generate args generateScalaServer else pure unit
  if args.lang == "swift" && args.side == "server"
    then generate args generateSwiftServer else pure unit
