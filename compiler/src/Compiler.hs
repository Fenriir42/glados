-- | Main compiler module.
module Compiler
  ( Options (..),
    options,
    entrypoint,
    prologue,

    -- * Re-exports
    module Compiler.Bytecode,
    module Compiler.Codegen,
  )
where

import Compiler.Bytecode
import Compiler.Codegen
import Options.Applicative

data Options = Options
  { optFile :: Maybe FilePath,
    optDump :: Bool,
    optOutput :: Maybe FilePath,
    optLoad :: Maybe FilePath,
    optStdlib :: Maybe FilePath
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> optional (argument str (metavar "FILE" <> help "Source file to compile and run"))
    <*> switch (long "dump" <> short 'd' <> help "Print disassembly instead of executing")
    <*> optional (strOption (long "output" <> short 'o' <> metavar "FILE" <> help "Write compiled bytecode to FILE"))
    <*> optional (strOption (long "load" <> short 'l' <> metavar "FILE" <> help "Load and run a pre-compiled .qbc FILE"))
    <*> optional (strOption (long "stdlib" <> metavar "DIR" <> help "Path to the Quant standard library (default: auto-detected)"))

prologue :: String
prologue = "Compile and run a Quant source file"

-- | Stub: full pipeline is in the cli package.
entrypoint :: Options -> IO ()
entrypoint _ = putStrLn "Use the 'cli' binary to compile and run Quant files."
