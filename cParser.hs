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

import Control.Monad
import Control.Applicative

import Data.Char
import Data.List
import Data.Maybe

import System.Console.GetOpt
import System.Environment
import System.Exit
import System.IO

import Text.Printf

data Flag = Blanks
          | InstructionSet String
          | AssemblyFile String
          | Help
          deriving (Eq,Show)

data Input = Input 
  { location :: Int
  , inputStr :: String
  } deriving (Show, Eq)

data Declaration = Declaration VariableType Identifier 
  deriving (Show, Eq)

-- We only care about return statements for now 
data Statement = Return
               | Statement Statement Expression -- Mandatory semicolon
  deriving (Show, Eq)

-- NOTE: Only accepts an integer for now. 
data Expression = Expression Integer
  deriving (Show, Eq)

data VariableType = VariableType String
  deriving (Show, Eq)

data ReturnType = ReturnType String
  deriving (Show, Eq)

data Identifier = Identifier String
  deriving (Show, Eq)

-- Function parameters
data Params = Params [Declaration]
  deriving (Show, Eq)

-- A function body will be reduced to a list of statements for the moment
data Body = Body [Statement]
  deriving (Show, Eq)

data Function = Function ReturnType Identifier Params Body
  deriving (Show, Eq)

data Program = Program Function
  deriving (Show, Eq)

data ParseError = ParseError Int String 
--  deriving (Show, Eq) 

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
-- TODO - Implement ParseError using catchError from Control.Monad
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

-- The following block of code is dedicated to assembly generation. I should put it in a separate module.
generateExpression :: Expression -> String
generateExpression (Expression ex) = show ex

generateStatement :: [Statement] -> String
generateStatement ([Statement s ex]) 
  | (s==Return) = "movl     $" ++ 
                  generateExpression ex ++
                  ", %eax" ++ 
                  "\n" ++
                  "ret"
  | otherwise   = ""
-- Statement Statement Expression -- Mandatory semicolon

generateBody :: Body -> String
generateBody (Body s) = generateStatement s

generateParams :: Params -> String
generateParams (Params [])= undefined
generateParams (Params (x:xs)) = undefined

generateFunctionHead :: Identifier -> String
generateFunctionHead (Identifier i) = ".globl _" ++ i ++ 
                                      "\n_" ++ i ++
                                      ":\n"

generateFunction :: Function -> String
generateFunction (Function ret id params body) = generateFunctionHead id ++
                                                 generateBody body

-- TODO: Traverse the AST and generate the assembly code
generateAssembly :: Program -> String
generateAssembly (Program f) = generateFunction f 

-- Arguments handling
flags :: [OptDescr Flag]
flags = [Option ['i'] [] (ReqArg InstructionSet "") ""
        ,Option ['o'] [] (ReqArg AssemblyFile "") ""
        ,Option []    ["help"] (NoArg Help) "Print this help message"
        ]

parse argv = case getOpt Permute flags argv of
              (args, fs, []) -> do
                let files = if null fs then ["-"] else fs
                if Help `elem` args 
                  then do hPutStrLn stderr (usageInfo header flags)
                          exitWith ExitSuccess
                  else return (nub (concatMap set args), files)

              (_,_,errs)    -> do
                hPutStrLn stderr (concat errs ++ usageInfo header flags)
                exitWith (ExitFailure 1)
              where header = "Usage: cParser <source.c> [-o] <assembly.s> [-i] <instruction_set>"
                    set f = [f]

-- These two functions do basically the same thing. They take a list of flags and match a datatype then return its
-- string value. See how I could abstract them so that I can pass any datatype and abstract away its string value.
-- Like so: filterFlag Flag <anyStr> -> anyStr
-- Or at least return the consumed input and the rest of the list, so we don't move the list around with already
-- parsed values.
filterInstructionSet :: [Flag] -> String
filterInstructionSet list = 
  case head [x | x@(InstructionSet _) <- list] of 
    InstructionSet i -> i 
    empty -> "Could not match flag in the list of arguments." -- Not matched!

filterAssemblyOutput :: [Flag] -> String
filterAssemblyOutput list = 
  case head [x | x@(AssemblyFile _) <- list] of 
    AssemblyFile a -> a
    empty -> "Could not match flag in the list of arguments." -- Not matched!


main :: IO ()
main = do
  putStrLn (unlines haskii) 
  (as, fs) <- getArgs >>= parse
  let ins = filterInstructionSet as 
      ass = filterAssemblyOutput as
      file = fs !! 0
  putStrLn $ "Instruction set: "           ++ ins -- NOTE: Only x86 for now
  putStrLn ("[INFO] Parsing source file '" ++ show (fs !! 0) ++ "'") >>
    readFile file >>= \ source ->
       case runParser program source of
        Right (source, ast) -> 
          putStrLn ("[INFO] Parsed as the following AST: (NOTE: Pretty print it)\n" ++ show ast ++ "\n") >>
          putStrLn ("[INFO] Assembly:") >>
          putStrLn asm >>
          writeFile ass asm >>
          putStrLn ("Assembly was written to " ++ ass)
            where 
              asm = generateAssembly ast

        Left e -> 
          putStrLn ("[ERROR] Error while parsing:\n" ++ show e)


