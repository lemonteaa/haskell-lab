-- Implementing the evaluator example in the classic "Applicative programming with effects"
-- At http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

import Data.Map (Map)
import qualified Data.Map as Map

data Exp variable value = Var variable | Val value | Add (Exp variable value) (Exp variable value)
data Env variable value = Envmap (Map variable value) value deriving Show

fetch :: (Ord vari) => vari -> Env vari val -> val
fetch x (Envmap e nil) = Map.findWithDefault nil x e
