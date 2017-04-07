module Dama.Core.Gen (genCore) where

import qualified Dama.Parser.AST as AST
import Dama.Error
import qualified Dama.Core.Core as Core

genCore :: AST.Program -> Either Error Core.Program
genCore = error "makeCore"
