module Parser

import Lightyear
import Lightyear.Strings
import Lightyear.Char
import Lightyear.Position
import Minima.AST
import Minima.Record

%access public export

Exp : Type
Exp = Expression (Record [('Position, Position)]) String

notePosition : Monad m => ParserT str m (Record [('Position, Position)])
notePosition = do pos <- getPosition
                  pure $ with Record ['Position := pos]

identifier : Parser String
identifier = pack <$> lexeme (some letter)

variable : Parser Exp
variable = do pos <- notePosition
              name <- identifier
              pure $ Variable pos name

number : Parser Exp
number = do pos <- notePosition
            val <- lexeme integer
            pure $ NumberLiteral pos val

string : Parser Exp
string = do pos <- notePosition
            content <- quoted '''
            pure $ StringLiteral pos content

list : Parser a -> Parser (List a)
list elem = sepBy elem (token ",")

mutual
  definition : Parser Exp
  definition = do pos <- notePosition
                  name <- lexeme identifier
                  token "is"
                  value <- expression
                  pure $ Definition pos name value

  function : Parser Exp
  function = do pos <- notePosition
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
  call = do pos <- notePosition
            args <- argList
            pure $ flip (Call pos) args

  group : Parser Exp
  group = do pos <- notePosition
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

program : Parser Exp
program = do pos <- notePosition
             spaces
             expressions <- commitTo $ list expression
             spaces
             eof
             pure $ Group pos expressions
