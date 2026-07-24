-- | Public interface to the Quant stack-based virtual machine.
module VM
  ( VMError (..),
    VMState (..),
    Frame (..),
    runProgram,
    runFunction,
    runFunctionCov,
    runFunctionLineCov,
    runDebugProgram,
  )
where

import VM.Interpreter (Frame (..), VMError (..), VMState (..), runDebugProgram, runFunction, runFunctionCov, runFunctionLineCov, runProgram)
