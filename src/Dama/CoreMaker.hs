module Dama.CoreMaker (makeCore) where

import qualified Dama.AST as AST
import Dama.Error
import qualified Dama.Core as Core

makeCore :: AST.Program -> Either Error Core.Program
makeCore = error "makeCore"
