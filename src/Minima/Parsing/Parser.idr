module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Minima.AST

%access public export

mutual
  variable : Parser Expression
  variable = Variable <$> (pack <$> some letter)

  number : Parser Expression
  number = NumberLiteral <$> integer

  string : Parser Expression
  string = do oneOf "'"
              content <- some $ noneOf "'"
              oneOf "'"
              pure $ StringLiteral $ pack content

  definition : Parser Expression
  definition = do name <- pack <$> some letter
                  string " is "
                  value <- expression
                  pure $ Definition name value

  expression : Parser Expression
  expression = definition
           <|> number
           <|> string
           <|> variable
