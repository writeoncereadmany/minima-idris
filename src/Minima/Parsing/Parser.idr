module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Minima.AST

%access public export

mutual
  identifier : Parser String
  identifier = pack <$> some letter

  variable : Parser Expression
  variable = Variable <$> identifier

  number : Parser Expression
  number = NumberLiteral <$> integer

  string : Parser Expression
  string = do oneOf "'"
              content <- some $ noneOf "'"
              oneOf "'"
              pure $ StringLiteral $ pack content

  definition : Parser Expression
  definition = do name <- identifier
                  some space
                  string "is"
                  some space
                  value <- expression
                  pure $ Definition name value

  list : Parser start -> Parser a -> Parser end -> Parser (List a)
  list start elem end = do start
                           elems <- sepBy elem (spaces *> char ',' <* spaces)
                           end
                           pure elems


  function : Parser Expression
  function = do args <- list (char '[') identifier (char ']')
                spaces
                string "=>"
                spaces
                body <- expression
                pure $ Function args body

  call : Parser Expression
  call = do callable <- expression
            args <- list (char '[') expression (char ']')
            pure $ Call callable args

  expression : Parser Expression
  expression = definition
           <|> call
           <|> number
           <|> string
           <|> variable
           <|> function
