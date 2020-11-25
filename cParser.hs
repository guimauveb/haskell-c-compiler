{-# LANGUAGE FlexibleInstances #-}

{- C compiler implementation in Haskell 

\\\ \\\ 
 \\\ \\\ 
  \\\ \\\  \\\\\\\\ 
  /// //\\   Ɔ.Ɔ.Ɔ.Ɔ.Ɔ.Ɔ.Ɔ
 /// ///\\\ \\\\\\\\ 
/// ///  \\\ 
       mov
       eax
        7

TODO - Error handling using Either works but doesn't always return the proper error message. For instance when
an error occurs in parseString (let's say when "return" isn't matched) the error displayed doesn't come from 
parseString but from parseChar trying to parse the character that follows "return", even though the error occured
in parseString! 

-}

module Main where

import Data.Char
import Control.Applicative
import System.Environment

data Input = Input 
  { location :: Int
  , inputStr :: String
  } deriving (Show, Eq)

data Declaration = Declaration VariableType Identifier 
  deriving (Show)

-- We only care about return statements for now 
data Statement = Return
               | Statement Statement Expression -- Mandatory semicolon
  deriving (Show)

-- NOTE: Only accepts an integer for now. 
data Expression = Expression Integer
  deriving (Show)

data VariableType = VariableType String
  deriving (Show)

data ReturnType = ReturnType String
  deriving (Show)

data Identifier = Identifier String
  deriving (Show)

-- Function parameters
data Params = Params [Declaration]
  deriving (Show)

-- A function body will be reduced to a list of statements for the moment
data Body = Body [Statement]
  deriving (Show)

data Function = Function ReturnType Identifier Params Body
  deriving (Show)

data Program = Program Function
  deriving (Show)

data ParseError = ParseError Int String 
--  deriving (Show) 

-- Pun intended
haskii = ["",
         "\\\\\\ \\\\\\ ", 
         " \\\\\\ \\\\\\ ",
         "  \\\\\\ \\\\\\  \\\\\\\\\\\\\\\\ ",
         "  /// //\\\\   Ɔ.Ɔ.Ɔ.Ɔ.Ɔ.Ɔ.Ɔ",
         " /// ///\\\\\\ \\\\\\\\\\\\\\\\ ",
         "/// ///  \\\\\\ ",
         "       mov",
         "       eax",
         "        7",
         "",
         "A Haskell C compiler by guimauve..."
        ]


instance Alternative (Either ParseError) where
    empty        = Left $ ParseError 1 "Empty."
    Left _ <|> n = n
    m      <|> _ = m

newtype Parser a = Parser 
  { runParser :: String -> Either ParseError (String, a) }

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
  empty = Parser $ \_ -> Left $ ParseError 0 "empty"
  (Parser p1) <|> (Parser p2) = 
    Parser $ \input -> p1 input <|> p2 input


-- DOING: Error checking
showError :: ParseError -> String
showError (ParseError loc var) = show loc ++ var

instance Show ParseError where show = showError

-- trapError action = catchError action (return . show)

-- NOTE: Using where notation instead of lambda notation
parseChar :: Char -> Parser Char
parseChar x = Parser f
  where 
    f (y:ys)
        | y == x    = Right (ys, x)
        | otherwise = Left $ 
                      ParseError 
                      0 $ " Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'"
    f [] = Left $ ParseError 0 " Empty string."

-- NOTE: To use parseChar over a string we could simply use map, but we would obtain a list of Parser Char
-- But we actually want a Parser of lists
-- [Parser Char] -> Parser [Char]
parseString :: String -> Parser String
parseString str = 
  Parser $ \input -> 
    case runParser (traverse parseChar str) input of 
      Left _ ->
        Left $ 
        ParseError
          0 $ " Expected \"" ++ str ++ "\", but found \"" ++ input ++ "\""
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
       then Left $ ParseError 0 " Value is null."
    else Right (input', xs)

int_max = 2147483647

isIntMax :: Parser String -> Parser String
isIntMax (Parser p) = 
  Parser $ \input -> do
    (input', xs) <- p input
    if read xs > int_max 
       then Left $ ParseError 0 " Integer must be <= INT_MAX."
    else Right (input', xs)

-- NOTE: discard whitespace 
-- Returns True for any Unicode space character, and the control characters \t, \n, \r, \f, \v.
ws :: Parser String
ws = spanP isSpace

-- NOTE: Mandatory whitespace
mandWs :: Parser String
mandWs = spanP (not . isSpace)

semicolon :: Parser String
semicolon = ws <* parseChar ';'

-- NOTE: many applies a function on its input until it fails:
-- Running runParser jsonNull "nullnullnull" would produce Right("nullnull", JsonNull), adding many:
--         runParser (many jsonNull "nullnullnull" will produce (Right "", JsonNull, JsonNull, JsonNull) 
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) 
                 <|> pure []

-- NOTE: Strings can be wrapped either into single or double quotes. No escaping yet
doubleQuotes = parseChar '"'  *> spanP (/='"')  <* parseChar '"'
singleQuotes = parseChar '\'' *> spanP (/='\'') <* parseChar '\''

stringLiteral :: Parser String
stringLiteral = singleQuotes <|> doubleQuotes

-- NOTE: Only parses an int for now
expression :: Parser Expression
expression = f <$> (isIntMax . notNull) (ws *> spanP isDigit <* ws) 
  where f ds  = Expression $ read ds

returnStatement :: Parser Statement
returnStatement = (\_ -> Return) <$> parseString "return" <* mandWs -- There must be a white space after return

-- NOTE: Can be a lot of things (but always ends with a semicolon)
statement :: Parser Statement
statement = Statement <$> returnStatement  <*> expression <* semicolon
      -- <|>

-- TODO - NOTE: Use a data structure to represent data types (int, char etc) instead of having the same code
-- for both returnType and variableType
returnType :: Parser ReturnType
returnType = f <$> 
  (ws *> parseString "int" <* ws <|> ws *> parseString "void" <* ws <|> ws *> parseString "char" <* ws)
  where f "int"  = ReturnType "int"
        f "void" = ReturnType "void"
        f "char" = ReturnType "char"
        f_       = undefined -- Proper error message

variableType :: Parser VariableType
variableType = f <$> 
  (ws *> parseString "int" <* ws <|> ws *> parseString "void" <* ws <|> ws *> parseString "char" <* ws)
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
params = Params <$> (ws *> parseChar '(' *> ws *>
                           elements 
                           <* ws <* parseChar ')' <* ws)
  where
    elements = sepBy (ws *> parseChar ',' <* ws) declaration 

body :: Parser Body
body = Body <$> (ws *> parseChar '{' *> ws *>
                           statements 
                           <* ws <* parseChar '}')
  where
    statements = sepBy (ws) statement

function :: Parser Function
function = Function <$> returnType <*> identifier <*> params <*> body

-- NOTE: Program must return a list of functions
program :: Parser Program
program = Program <$> function

-- TODO: Traverse the AST and generate the assembly code
generateAssembly :: Maybe Program -> String
generateAssembly = undefined


main :: IO ()
main = getArgs >>= \ args ->
       readFile (args !! 0) >>= \ source ->
       putStrLn (unlines haskii) >>
       putStrLn ("[INFO] Parsing source file '" ++ args !! 0 ++ "'") >>
       case runParser program source of
        Right (source, ast) -> 
          putStrLn ("[INFO] Parsed as:\n" ++ show ast)
        Left e -> 
          putStrLn ("[ERROR] Error while parsing:\n" ++ show e)



