module Parser

import Lightyear
import Lightyear.Strings
import LightYear.Char
import Minima.AST

%access public export

Exp : Type
Exp = Expression ()

identifier : Parser String
identifier = pack <$> lexeme (some letter)

variable : Parser Exp
variable = Variable () <$> identifier

number : Parser Exp
number = do val <- lexeme integer
            pure $ (NumberLiteral () val)

string : Parser Exp
string = StringLiteral () <$> quoted '''

list : Parser a -> Parser (List a)
list elem = sepBy elem (token ",")

mutual
  definition : Parser Exp
  definition = do name <- lexeme identifier
                  token "is"
                  value <- expression
                  pure $ Definition () name value

  function : Parser Exp
  function = do args <- lexeme $ between (char '[') (char ']') (list identifier)
                token "=>"
                body <- expression
                pure $ Function () args body

  argList : Parser (List Exp)
  argList = do char '['
               args <- commitTo $ list expression
               char ']'
               pure args

  call : Parser (Exp -> Exp)
  call = do args <- argList
            pure $ flip (Call ()) args

  group : Parser Exp
  group = do char '('
             expressions <- commitTo $ list expression
             char ')'
             pure $ Group () expressions

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
             eof
             pure expressions
