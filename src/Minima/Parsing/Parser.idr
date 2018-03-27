module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Minima.AST

%access public export

variable : Parser Expression
variable = do letters <- some letter
              pure $ Variable $ pack letters

expression : Parser Expression
expression = variable
