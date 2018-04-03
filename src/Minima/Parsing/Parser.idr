module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Lightyear.Position
import Minima.AST

%access public export

Exp : Type
Exp = Expression Position String

identifier : Parser String
identifier = pack <$> lexeme (some letter)

variable : Parser Exp
variable = do pos <- getPosition
              name <- identifier
              pure $ Variable pos name

number : Parser Exp
number = do pos <- getPosition
            val <- lexeme integer
            pure $ NumberLiteral pos val

string : Parser Exp
string = do pos <- getPosition
            content <- quoted '''
            pure $ StringLiteral pos content

list : Parser a -> Parser (List a)
list elem = sepBy elem (token ",")

mutual
  definition : Parser Exp
  definition = do pos <- getPosition
                  name <- lexeme identifier
                  token "is"
                  value <- expression
                  pure $ Definition pos name value

  function : Parser Exp
  function = do pos <- getPosition
                args <- lexeme $ between (char '[') (char ']') (list identifier)
                token "=>"
                body <- expression
                pure $ Function pos args body

  argList : Parser (List Exp)
  argList = do char '['
               args <- commitTo $ list expression
               char ']'
               pure args

  call : Parser (Exp -> Exp)
  call = do pos <- getPosition
            args <- argList
            pure $ flip (Call pos) args

  group : Parser Exp
  group = do pos <- getPosition
             char '('
             expressions <- commitTo $ list expression
             char ')'
             pure $ Group pos expressions

  fragment : Parser Exp
  fragment = number
         <|> definition
         <|> string
         <|> group
         <|> variable
         <|> function

  expression : Parser Exp
  expression = do exp <- fragment
                  operations <- many call
                  pure $ foldl (\ex => \f => f ex) exp operations

program : Parser (List Exp)
program = do spaces
             expressions <- list expression
             spaces
             eof
             pure expressions
