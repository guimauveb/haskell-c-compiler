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
                 deriving Eq

instance Show Declaration where
  show (Declaration a b) = "Declaration " ++ show a ++ " " ++ show b 
                         ++ "           "          

-- We only care about return statements for now 
data Statement = Return
               | Statement Statement Expression -- Mandatory semicolon
               deriving (Show, Eq)

data UnaryOperator = UnOperator Char
                   deriving (Show, Eq)

data BinaryOperator = BinOperator Char
                    deriving (Show, Eq)

-- TODO - Is our program able to handle repetition ? That is, in EBNF notation: <term> { ("+" | "-") <term> }
-- TODO - Split binary operators into two categories ? (+,- and *,/)
data Expression = AddOrSubtractBinaryOperation Term BinaryOperator Term
                deriving (Show, Eq)

-- A factor is an expression a unary operator can be applied to
data Factor = WrappedExpression Expression -- WrappedExpression is an expression wrapped in parentheses
            | UnaryOperation UnaryOperator Factor
            | Constant Integer
            deriving(Show, Eq)

-- TODO - Split binary operators into two categories ? (+,- and *,/)
data Term = MultiplyOrDivideOperation Factor BinaryOperator Factor
          | FactorTerm Factor
          deriving(Show, Eq)

data VariableType = VariableType String
                  deriving (Show, Eq)

data ReturnType = ReturnType String
                deriving Eq

instance Show ReturnType where
  show (ReturnType a) = "ReturnType " ++ show a ++ " "

data Identifier = Identifier String
                deriving Eq

instance Show Identifier where
  show (Identifier a) = show a ++ "\n"

-- Function parameters
data Params = Params [Declaration]
            deriving Eq

instance Show Params where
  show (Params a) = "    Params " ++ show a ++ "\n"

-- A function body will be reduced to a list of statements for the moment
data Body = Body [Statement]
          deriving Eq

instance Show Body where
  show (Body xs) = "    Body " ++ show xs

data Function = Function ReturnType Identifier Params Body
              deriving Eq
 
instance Show Function where
  show (Function a b c d) = "  Function " ++ show a ++ show b ++ show c ++ show d

data Program = Program Function
             deriving Eq
 
instance Show Program where
  show (Program a) = "Program\n" ++ show a 

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

-- TODO - constant is now a Factor
constant :: Parser Factor
constant = f <$> (isIntMax . notNull) (ws *> spanP isDigit <* ws) 
  where f ds = Constant $ read ds

unaryOperator :: Parser UnaryOperator
unaryOperator = f <$>
  (ws *> parseString "-" <* ws <|> ws *> parseString "~" <* ws <|> ws *> parseString "!" <* ws)
  where f "-" = UnOperator '-'
        f "~" = UnOperator '~'
        f "!" = UnOperator '!'

-- TODO - unaryOperation is now a factor
unaryOperation :: Parser Factor
unaryOperation = UnaryOperation <$> unaryOperator <*> factor

binaryOperator :: Parser BinaryOperator
binaryOperator = f <$>
  (ws *> parseString "-" <* ws <|> ws *> parseString "+" <* ws <|> 
   ws *> parseString "*" <* ws <|> ws *> parseString "/" <* ws)
  where f "-" = BinOperator '-'
        f "+" = BinOperator '+'
        f "*" = BinOperator '*'
        f "/" = BinOperator '/'

binaryOperation :: Parser Expression
binaryOperation = BinaryOperation <$> expression <*> binaryOperator <*> expression

-- TODO -> '(' Expression ')' | UnaryOperation | Constant Integer
factor :: Parser Factor
factor = WrappedExpression <$> (ws *> parseChar '(' *> ws *>
                           expression
                           <* ws <* parseChar ')' <* ws)
expression :: Parser Expression
expression = constant 
          <|> unaryOperation
          <|> binaryOperation

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
generateUnaryOperation :: UnaryOperator -> String
generateUnaryOperation (UnOperator op) 
  | (op=='-') = "neg      %eax" ++ "\n"
  | (op=='!') = "cmpl     $0, %eax" ++ "\n" ++  -- set ZF on if exp == 0, set it off otherwise 
                "movl     $0, %eax" ++ "\n" ++  -- zero out EAX (doesn't change FLAGS)
                "sete     %al"      ++ "\n"     -- set AL register (the lower byte of EAX) to 1 if ZF is on
  | (op=='~') = "not      %eax"     ++ "\n"
  | otherwise = "Unknown unary operator."


generateExpression :: Expression -> String
-- TODO - constant is now a Factor
generateExpression (Constant ex) = "movl     $" 
                                 ++ show ex 
                                 ++ ", %eax"
                                 ++ "\n"
-- TODO - unaryOperation is now a Factor
generateExpression (UnaryOperation unop exp) =  generateExpression exp ++ generateUnaryOperation unop


generateStatement :: [Statement] -> String
generateStatement ([Statement s ex]) 
  | (s==Return) = generateExpression ex 
                  ++ "ret"
  | otherwise   = ""

generateBody :: Body -> String
generateBody (Body s) = generateStatement s

generateParams :: Params -> String
generateParams (Params []) = undefined
generateParams (Params (x:xs)) = undefined

generateFunctionHead :: Identifier -> String
generateFunctionHead (Identifier i) = ".globl _" ++ i ++ 
                                      "\n_" ++ i ++
                                      ":\n"

generateFunction :: Function -> String
generateFunction (Function ret id params body) = generateFunctionHead id ++
                                                 generateBody body

-- DOING: Traverse the AST and generate the assembly code
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
filterInstructionSet [] = "No instruction set was given. Defaults to x86."
filterInstructionSet list = 
  case head [x | x@(InstructionSet _) <- list] of 
    InstructionSet i -> i 
    empty -> "Could not match instruction set in the list of arguments." -- Not matched!

filterAssemblyOutput :: [Flag] -> String
filterAssemblyOutput [] = "No assembly file path given."
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
  putStrLn ("[INFO] Parsing source file " ++ show (fs !! 0)) >>
    readFile file >>= \ source ->
       case runParser program source of
        Right (source, ast) -> 
          putStrLn ("[INFO] Parsed as the following AST:\n" ++ show ast ++ "\n") >>
          putStrLn ("[INFO] Instruction set: " ++ ins) >> -- NOTE: Only x86 for now
          putStrLn ("[INFO] Assembly:\n") >>
          putStrLn asm >>
          writeFile ass asm >>
          putStrLn ("\n[INFO] Assembly code was written to: " ++ ass)
            where 
              asm = generateAssembly ast

        Left e -> 
          putStrLn ("[ERROR] Error while parsing:\n" ++ show e)


