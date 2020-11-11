{- A C compiler implementation in Haskell -}
module Main where

import Data.Char
import Control.Applicative

data Id = Id String 
  deriving (Show)

data Params = Params [String]
            | Empty
  deriving (Show)

data Statement = Return
               | Statement Statement Expression Semicolon
  deriving (Show)

data Semicolon = Semicolon
  deriving (Show)

-- NOTE: Only accepts an integer for now. TODO: <= INT_MAX
data Expression = Expression Integer
  deriving (Show)

data Body = Body [Statement]
  deriving (Show)

data Function = Function ReturnType Identifier Params Body
  deriving (Show)

data Identifier = Identifier String
  deriving (Show)

data ReturnType = ReturnType String
  deriving (Show)

data Program = Program Function
  deriving (Show)

{-
  Open brace {
  Close brace }
  Open parenthesis \(
  Close parenthesis \)
  Semicolon ;
  Int keyword int
  Return keyword return
  Identifier [a-zA-Z]\w*
  Integer literal [0-9]+
-}

-- NOTE: To get a proper error reporting we could use Either (Int, Int, String) (String, a) 
-- Here we return what the parser has consumed, and the rest of the input to pass it to the next parser
newtype Parser a = Parser 
  { runParser :: String -> Maybe (String, a) 
  }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do 
      (input', x) <- p input
      Just (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Just (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) = 
    Parser $ \input -> p1 input <|> p2 input


-- NOTE: Using where notation instead of lambda notation
charP :: Char -> Parser Char
charP x = Parser f
  where 
    f (y:ys)
      -- Condition guards
        | y == x    = Just (ys, x)
        | otherwise = Nothing
    f [] = Nothing

-- NOTE: To use charP over a string we could simply use map, but we would obtain a list of Parser Char
-- But we actually want a Parser of lists
-- [Parser Char] -> Parser [Char]

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
  let (token, rest) = span f input
    in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
       then Nothing
    else Just (input', xs)

-- NOTE: discard white space 
-- Returns True for any Unicode space character, and the control characters \t, \n, \r, \f, \v.
ws :: Parser String
ws = spanP isSpace

-- NOTE: many applies a function on its input until it fails:
-- Running runParser jsonNull "nullnullnull" would produce Just("nullnull", JsonNull), adding many:
--         runParser (many jsonNull "nullnullnull" will produce (Just "", JsonNull, JsonNull, JsonNull) 
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) 
                 <|> pure []

-- NOTE: Strings can be wrapped either into single or double quotes. No escaping yet
doubleQuotes = charP '"' *> spanP (/='"') <* charP '"'
singleQuotes = charP '\'' *> spanP (/='\'') <* charP '\''

stringLiteral :: Parser String
stringLiteral = singleQuotes <|> doubleQuotes

semiColon :: Parser Semicolon
semiColon = (\_ -> Semicolon) <$> stringP ";"

-- NOTE: Only parses an int for now
expression :: Parser Expression
expression = f <$> notNull (ws *> spanP isDigit <* ws)
  where f ds = Expression $ read ds

returnStatement :: Parser Statement
returnStatement = (\_ -> Return) <$> stringP "return"

-- NOTE: Can be a lot of things
statement :: Parser Statement
statement = Statement <$> returnStatement  <*> expression <*> semiColon
      -- <|>

returnType :: Parser ReturnType
returnType = f <$> (ws *> stringP "int" <* ws <|> ws *> stringP "void" <* ws)
  where f "int"  = ReturnType "int"
        f "void" = ReturnType "void"
        f_        = undefined

-- NOTE: [a-zA-Z]\w* for now. First char must be an alpha but others can be digits
identifier :: Parser Identifier
identifier = Identifier <$> spanP isAlpha

-- NOTE: Function parameters
params :: Parser Params
params = Params <$> (ws *> charP '(' *> ws *>
                           elements 
                           <* ws <* charP ')' <* ws)
  where
    elements = sepBy (ws *> charP ',' <* ws) stringLiteral

body :: Parser Body
body = Body <$> (ws *> charP '{' *> ws *>
                           statements 
                           <* ws <* charP '}')
  where
    statements = sepBy (ws) statement

function :: Parser Function
function = Function <$> returnType <*> identifier <*> params <*> body

-- NOTE: Program must return a list of functions
program :: Parser Program
program = Program <$> function

parseFile :: FilePath -> Parser a -> IO (Maybe a)
parseFile filename parser = do
  input <- readFile filename 
  return (snd <$> runParser parser input)

main = undefined



