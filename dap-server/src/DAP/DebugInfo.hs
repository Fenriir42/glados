module DAP.DebugInfo
  ( DebugInfo,
    buildDebugInfo,
    instrLine,
    lineInstrs,
  )
where

import AST.Types.Common (FuncName)
import Compiler.Bytecode (Bytecode (..), Instruction (..))
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)

-- | Maps between bytecode positions and source lines.
data DebugInfo = DebugInfo
  { -- func -> instrIdx -> lineNo (nearest preceding ICovMark)
    diInstrToLine :: Map FuncName (IntMap Int),
    -- lineNo -> [(func, instrIdx)] for the ICovMark at that line
    diLineToInstrs :: IntMap [(FuncName, Int)]
  }

buildDebugInfo :: [Bytecode] -> DebugInfo
buildDebugInfo bcs = DebugInfo instrToLine lineToInstrs
  where
    infos = map buildFuncInfo bcs
    instrToLine =
      Map.fromList
        [(bytecodeFunction bc, i2l) | (bc, (i2l, _)) <- zip bcs infos]
    lineToInstrs =
      foldr (IntMap.unionWith (++)) IntMap.empty [l2i | (_, l2i) <- infos]

    buildFuncInfo :: Bytecode -> (IntMap Int, IntMap [(FuncName, Int)])
    buildFuncInfo bc =
      let fname = bytecodeFunction bc
          marks =
            [ (idx, lineNo)
              | (idx, ICovMark lineNo) <- zip [0 ..] (bytecodeInstructions bc)
            ]
          i2l = IntMap.fromList marks
          l2i =
            IntMap.fromListWith
              (++)
              [(lineNo, [(fname, idx)]) | (idx, lineNo) <- marks]
       in (i2l, l2i)

-- | Source line for a (func, ip) pair: the nearest ICovMark at or before @ip@.
instrLine :: DebugInfo -> FuncName -> Int -> Maybe Int
instrLine di func ip =
  case Map.lookup func (diInstrToLine di) of
    Nothing -> Nothing
    Just m -> fmap snd (IntMap.lookupLE ip m)

-- | All (func, instrIdx) pairs whose ICovMark is at @lineNo@.
lineInstrs :: DebugInfo -> Int -> [(FuncName, Int)]
lineInstrs di lineNo =
  fromMaybe [] (IntMap.lookup lineNo (diLineToInstrs di))
