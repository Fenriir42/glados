module Main (main) where

import AST.Types.AST (Program (..))
import Compiler.Bytecode (Bytecode (..), Instruction (..))
import Compiler.Codegen (compileProgram)
import Control.Monad (forM_)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Lib (lexFile)
import Parser.Decl (parseDecl)
import Test.Hspec
import Text.Megaparsec (errorBundlePretty, many, runParser)

-- | Attempt to parse and compile a file, returning an error string or the
-- compiled bytecodes.
compileFile :: FilePath -> IO (Either String [Bytecode])
compileFile filePath = do
  lexResult <- lexFile filePath
  case lexResult of
    Left lexErr -> return $ Left $ "Lexing error: " ++ lexErr
    Right tokens ->
      case runParser (many parseDecl) filePath tokens of
        Left parseErr ->
          return $ Left $ "Parse error: " ++ errorBundlePretty parseErr
        Right decls ->
          case compileProgram Map.empty Set.empty Map.empty (Program decls) of
            Left compileErr -> return $ Left $ "Compile error: " ++ show compileErr
            Right bytecodes -> return $ Right bytecodes

displayBytecode :: Bytecode -> String
displayBytecode bc =
  unlines $
    [ "Function: " ++ show (bytecodeFunction bc),
      "Instructions (" ++ show (length (bytecodeInstructions bc)) ++ "):"
    ]
      ++ zipWith (\i instr -> "  " ++ pad i ++ ": " ++ show instr) [0 ..] (bytecodeInstructions bc)
      ++ ( if null (bytecodeStrings bc)
             then []
             else
               "String pool:"
                 : zipWith (\i s -> "  [" ++ show i ++ "] " ++ show s) [0 ..] (bytecodeStrings bc)
         )
  where
    pad i = let s = show (i :: Int) in replicate (4 - length s) ' ' ++ s

fileSpec :: FilePath -> Int -> Spec
fileSpec path expectedFunctions = do
  it ("Should compile " ++ path) $ do
    result <- compileFile path
    case result of
      Left err -> expectationFailure err
      Right bytecodes -> do
        length bytecodes `shouldBe` expectedFunctions
        forM_ bytecodes $ \bc ->
          putStrLn $ "\n" ++ displayBytecode bc

main :: IO ()
main = hspec $ do
  describe "File compilation" $ do
    fileSpec "/root/glados/tests/nested_elif.qa" 1
    fileSpec "/root/glados/tests/nth_prime.qa" 2
    fileSpec "/root/glados/tests/array.qa" 1
