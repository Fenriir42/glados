-- | Public interface to the Quant stack-based virtual machine.
module VM
  ( VMError (..),
    runProgram,
  )
where

import VM.Interpreter (VMError (..), runProgram)
