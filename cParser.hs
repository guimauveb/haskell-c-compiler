{-# LANGUAGE FlexibleInstances #-}

{- λ C compiler implementation in Haskell -}

module Parser where

import Data.Either
import Prelude
import Data.Char
import Control.Applicative

data Input = Input 
  { location :: Int
  , inputStr :: String
  } deriving (Show, Eq)

data Declaration = Declaration VariableType Identifier 
  deriving (Show)

-- Function parameters
data Params = Params [Declaration]
  deriving (Show)

-- We only care about return statements for now 
data Statement = Return
               | Statement Statement Expression -- Mandatory semicolon
  deriving (Show)

-- NOTE: Only accepts an integer for now. 
data Expression = Expression Integer
  deriving (Show)

-- A function body will be reduced to a list of statements for the moment
data Body = Body [Statement]
  deriving (Show)

data Identifier = Identifier String
  deriving (Show)

data VariableType = VariableType String
  deriving (Show)

data ReturnType = ReturnType String
  deriving (Show)

data Function = Function ReturnType Identifier Params Body
  deriving (Show)

data Program = Program Function
  deriving (Show)

data ParserError = ParserError Int String 
  deriving (Show) 

instance Alternative (Either ParserError) where
    empty        = Left $ ParserError 1 "Empty."
    Left _ <|> n = n
    m      <|> _ = m


-- NOTE: To get a proper error reporting we could use Either (Int, Int, String) (String, a) 
-- Here we return what the parser has consumed, and the rest of the input to pass it to the next parser
-- For now we only return Either "an error message" or the actual value
newtype Parser a = Parser 
  { runParser :: String -> Either ParserError (String, a) }

instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do 
      (input', x) <- p input
      Right (input', f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Right (input, x)
  (Parser p1) <*> (Parser p2) = 
    Parser $ \input -> do
    (input', f) <- p1 input
    (input'', a) <- p2 input'
    Right (input'', f a)

instance Alternative Parser where
  empty = Parser $ \_ -> Left $ ParserError 0 "empty"
  (Parser p1) <|> (Parser p2) = 
    Parser $ \input -> p1 input <|> p2 input


-- NOTE: Using where notation instead of lambda notation
charP :: Char -> Parser Char
charP x = Parser f
  where 
    f (y:ys)
        | y == x    = Right (ys, x)
        | otherwise = Left $ 
                      ParserError 
                      0 $ "Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'"
        where str = y:ys
    f [] = Left $ ParserError 0 "Empty string."

-- NOTE: To use charP over a string we could simply use map, but we would obtain a list of Parser Char
-- But we actually want a Parser of lists
-- [Parser Char] -> Parser [Char]
stringP :: String -> Parser String
stringP str = 
  Parser $ \input -> 
    case runParser (traverse charP str) input of 
      Left _ ->
        Left $ 
        ParserError
          0 $ "Expected \"" ++ str ++ "\", but found \"" ++ input ++ "\""
      result -> result

spanP :: (Char -> Bool) -> Parser String
spanP f = Parser $ \input -> 
  let (token, rest) = span f input
    in Right (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
       then Left $ ParserError 0 "Value is null."
    else Right (input', xs)

-- NOTE: discard whitespace 
-- Returns True for any Unicode space character, and the control characters \t, \n, \r, \f, \v.
ws :: Parser String
ws = spanP isSpace

-- NOTE: Mandatory whitespace
mandWs :: Parser String
mandWs = spanP (not . isSpace)

semicolon :: Parser String
semicolon = ws <* charP ';'

-- NOTE: many applies a function on its input until it fails:
-- Running runParser jsonNull "nullnullnull" would produce Right("nullnull", JsonNull), adding many:
--         runParser (many jsonNull "nullnullnull" will produce (Right "", JsonNull, JsonNull, JsonNull) 
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) 
                 <|> pure []

-- NOTE: Strings can be wrapped either into single or double quotes. No escaping yet
doubleQuotes = charP '"'  *> spanP (/='"')  <* charP '"'
singleQuotes = charP '\'' *> spanP (/='\'') <* charP '\''

stringLiteral :: Parser String
stringLiteral = singleQuotes <|> doubleQuotes

int_max = 2147483647

isIntMax :: Parser String -> Parser String
isIntMax (Parser p) = 
  Parser $ \input -> do
    (input', xs) <- p input
    if read xs > int_max 
       then Left $ ParserError 0 "Integer must be <= INT_MAX."
    else Right (input', xs)

-- NOTE: Only parses an int for now
expression :: Parser Expression
expression = f <$> (isIntMax . notNull) (ws *> spanP isDigit <* ws) 
  where f ds  = Expression $ read ds

returnStatement :: Parser Statement
returnStatement = (\_ -> Return) <$> stringP "return" <* mandWs -- There must be a white space after return

-- NOTE: Can be a lot of things (but always ends with a semicolon)
statement :: Parser Statement
statement = Statement <$> returnStatement  <*> expression <* semicolon
      -- <|>

-- TODO - NOTE: Use a data structure to represent data types (int, char etc) instead of having the same code
-- for both returnType and variableType
returnType :: Parser ReturnType
returnType = f <$> 
  (ws *> stringP "int" <* ws <|> ws *> stringP "void" <* ws <|> ws *> stringP "char" <* ws)
  where f "int"  = ReturnType "int"
        f "void" = ReturnType "void"
        f "char" = ReturnType "char"
        f_       = undefined -- Proper error message

variableType :: Parser VariableType
variableType = f <$> 
  (ws *> stringP "int" <* ws <|> ws *> stringP "void" <* ws <|> ws *> stringP "char" <* ws)
  where f "int"  = VariableType "int"
        f "void" = VariableType "void"
        f "char" = VariableType "char"
        f_       = undefined -- Proper error message

-- NOTE: [a-zA-Z]\w* for now. 
-- TODO - First char must be an alpha but others can be digits
identifier :: Parser Identifier
identifier = Identifier <$> spanP isAlpha

declaration :: Parser Declaration
declaration = Declaration <$> variableType <*> identifier

-- NOTE: Function parameters
params :: Parser Params
params = Params <$> (ws *> charP '(' *> ws *>
                           elements 
                           <* ws <* charP ')' <* ws)
  where
    elements = sepBy (ws *> charP ',' <* ws) declaration 

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

parseFile :: FilePath -> Parser a -> IO (Either ParserError a)
parseFile filename parser = do
  input <- readFile filename 
  return (snd <$> runParser parser input)

-- TODO: Traverse the AST and generate the assembly code
generateAssembly :: Maybe Program -> String
generateAssembly = undefined







