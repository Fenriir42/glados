module Parser.Stmt (parseBlock) where

import AST.Types.AST (Block (..))
import AST.Types.Common (Located (..))
import Parser.Utils (TokenParser)

parseBlock :: TokenParser (Located (Block ann))
