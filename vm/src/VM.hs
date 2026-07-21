-- | Public interface to the Quant stack-based virtual machine.
module VM
  ( VMError (..),
    runProgram,
    runFunction,
    runFunctionCov,
  )
where

import VM.Interpreter (VMError (..), runFunction, runFunctionCov, runProgram)
