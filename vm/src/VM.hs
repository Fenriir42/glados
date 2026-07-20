-- | Public interface to the Quant stack-based virtual machine.
module VM
  ( VMError (..),
    runProgram,
    runFunction,
  )
where

import VM.Interpreter (VMError (..), runFunction, runProgram)
