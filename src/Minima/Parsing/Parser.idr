module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Minima.AST

%access public export

identifier : Parser String
identifier = pack <$> lexeme (some letter)

variable : Parser Expression
variable = Variable <$> identifier

number : Parser Expression
number = do val <- lexeme integer
            pure $ NumberLiteral val

string : Parser Expression
string = StringLiteral <$> quoted '''

list : Parser a -> Parser (List a)
list elem = sepBy elem (token ",")


mutual
  definition : Parser Expression
  definition = do name <- lexeme identifier
                  token "is"
                  value <- expression
                  pure $ Definition name value

  function : Parser Expression
  function = do args <- lexeme $ between (char '[') (char ']') (list identifier)
                token "=>"
                body <- expression
                pure $ Function args body

  argList : Parser (List Expression)
  argList = do char '['
               args <- commitTo $ list expression
               char ']'
               pure args

  call : Parser (Expression -> Expression)
  call = do args <- argList
            pure $ flip Call args

  group : Parser Expression
  group = do char '('
             expressions <- commitTo $ list expression
             char ')'
             pure $ Group expressions

  fragment : Parser Expression
  fragment = number
         <|> definition
         <|> string
         <|> group
         <|> variable
         <|> function

  expression : Parser Expression
  expression = do exp <- fragment
                  modifiedBy <- (call <|> pure id)
                  pure $ modifiedBy exp
