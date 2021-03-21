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

import           Control.Applicative
import           Control.Monad

import           Data.Char
import           Data.List
import           Data.Maybe

import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

import           Text.Printf

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

newtype UnaryOperator = UnOperator Char
                   deriving (Show, Eq)

data BinaryOperator = MultiplicationOperator Char
                    | DivisionOperator Char
                    | AdditionOperator Char
                    | SubtractOperator Char
                    deriving (Show, Eq)

newtype VariableType = VariableType String
                  deriving (Show, Eq)

newtype ReturnType = ReturnType String
                deriving Eq

instance Show ReturnType where
  show (ReturnType a) = "ReturnType " ++ show a ++ " "

newtype Identifier = Identifier String
                deriving Eq

instance Show Identifier where
  show (Identifier a) = show a ++ "\n"

-- Function parameters
newtype Params = Params [Declaration]
            deriving Eq

instance Show Params where
  show (Params a) = "    Params " ++ show a ++ "\n"

-- A function body will be reduced to a list of statements for the moment
newtype Body = Body [Statement]
          deriving Eq

instance Show Body where
  show (Body xs) = "    Body " ++ show xs

data Function = Function ReturnType Identifier Params Body
              deriving Eq

instance Show Function where
  show (Function a b c d) = "  Function " ++
    show a ++ show b ++ show c ++ show d

newtype Program = Program Function
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

-- TODO - Error checking
showError :: ParseError -> String
showError (ParseError loc var) = show loc ++ var

deParser :: Parser a -> String -> Either ParseError (String, a)
deParser (Parser p) = p

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

{-- Explaination:
     >>== (bind) takes a parser in 'a' and a function from 'a' to a parser in 'b', and returns a parser in 'b'.
     The resulting parser is made by wrapping 'q :: String -> Either ParserError (String, b)' in the Parser
     constructor 'Parser'. Then 'q', the combined parser, takes a 'String' called 'input' and applies the function
     'p1 :: String -> Either ParseError (String, a)' that we got from pattern matching against the first parser,
     and pattern matches on the result. In case of error, we return 'ParseError'. In case of success, we get two things:
     the still unparsed string called 'rest' and the result 'a'. We give 'a' to 'f', the second parser combinator, and get
     a 'Parser b' which we need to unwrap with 'deParser' to get a function '(String -> Either ParseError (String, b)' back.
     That function can be applied to 'rest' for the final result of the combined parsers.
--}
instance Monad Parser where
  Parser p1 >>= f = Parser q where
    q input = case p1 input of
              Right (rest, a) -> deParser (f a) rest
              Left _ ->
                Left $ ParseError 1 " There was an error."

  return parsed = Parser $ \rest -> Right (rest,parsed)

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

intMax = 2147483647

isIntMax :: Parser String -> Parser String
isIntMax (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if read xs > intMax
       then Left $ ParseError 0 " Integer must be <= INT_MAX."
    else Right (input', xs)

-- Discard whitespace
-- Returns True for any Unicode space character, and the control characters \t, \n, \r, \f, \v.
ws :: Parser String
ws = spanP isSpace

-- Mandatory whitespace
mandWs :: Parser String
mandWs = spanP (not . isSpace)

semicolon :: Parser String
semicolon = ws <* parseChar ';'

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)
                 <|> pure []

-- Strings can be wrapped either into single or double quotes. No escaping yet
doubleQuotes = parseChar '"'  *> spanP (/='"')  <* parseChar '"'
singleQuotes = parseChar '\'' *> spanP (/='\'') <* parseChar '\''

stringLiteral :: Parser String
stringLiteral = singleQuotes <|> doubleQuotes

unaryOperator :: Parser UnaryOperator
unaryOperator = f <$>
  (ws *> parseString "-" <* ws <|> ws *> parseString "~" <* ws <|> ws *> parseString "!" <* ws)
    where f "-" = UnOperator '-'
          f "~" = UnOperator '~'
          f "!" = UnOperator '!'

unaryOperation :: Parser Expression
unaryOperation = Unary <$> unaryOperator <*> parseFactor

addOperator :: Parser BinaryOperator
addOperator = f <$>
  (ws *> parseString "+" <* ws)
    where f "+" = AdditionOperator '+'

subtractOperator :: Parser BinaryOperator
subtractOperator = f <$>
  (ws *> parseString "-" <* ws)
     where f "-" = SubtractOperator '-'

multiplicationOperator :: Parser BinaryOperator
multiplicationOperator = f <$>
  (ws *> parseString "*" <* ws)
    where f "*" = MultiplicationOperator '*'

divisionOperator :: Parser BinaryOperator
divisionOperator = f <$>
  (ws *> parseString "/" <* ws)
    where f "/" = DivisionOperator '/'

binaryOperator :: Parser BinaryOperator
binaryOperator = addOperator
              <|> subtractOperator
              <|> multiplicationOperator
              <|> divisionOperator

data BinOp = Add | Sub | Multiply | Divide
    deriving (Show, Eq)

data Expression = Binary BinOp Expression Expression
          | Unary UnaryOperator Expression
          | Constant Integer
          | FactorTerm Expression
          | WrappedExpression Expression
          deriving(Show, Eq)


constant :: Parser Expression
constant = f <$> (isIntMax . notNull) (ws *> spanP isDigit <* ws)
  where f ds = Constant $ read ds

parseExpression :: Parser Expression
parseExpression = do
  t1 <- parseTerm
  loop t1
  where termSuffix t1 = do
          op <- binaryOperator
          t2 <- parseTerm
          case op of
            AdditionOperator '+' -> loop (Binary Add t1 t2)
            SubtractOperator '-' -> loop (Binary Sub t1 t2)
        loop t = termSuffix t <|> return t

parseTerm :: Parser Expression
parseTerm = do
  f1 <- parseFactor
  loop f1
    where factorSuffix f1 = do
            op <- binaryOperator
            f2 <- parseFactor
            case op of
              MultiplicationOperator '*' -> loop (Binary Multiply f1 f2)
              DivisionOperator '/' -> loop (Binary Divide f1 f2)
              _ -> empty
          loop t = factorSuffix t <|> return t

-- A factor is an expression a unary operator can be applied to.
parseFactor :: Parser Expression
parseFactor = WrappedExpression <$> (ws *> parseChar '(' *> ws *>
                           parseExpression
                           <* ws <* parseChar ')' <* ws)
      <|> unaryOperation
      <|> constant

returnStatement :: Parser Statement
returnStatement = (Return <$ parseString "return") <* mandWs -- There must be a white space after return

-- NOTE: Can be a lot of things (but always ends with a semicolon)
statement :: Parser Statement
statement = Statement <$> returnStatement  <*> parseExpression <* semicolon

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
    statements = sepBy ws statement

function :: Parser Function
function = Function <$> returnType <*> identifier <*> params <*> body

-- NOTE: Program must return a list of functions
program :: Parser Program
program = Program <$> function

-- TODO - Split unary operators ?
-- The following block of code is dedicated to assembly generation. I should put it in a separate module.
generateUnaryOperation :: UnaryOperator -> String
generateUnaryOperation (UnOperator op)
  | op=='-' = "neg      %eax" ++ "\n"
  | op=='!' = "cmpl     $0, %eax" ++ "\n" ++  -- set ZF on if exp == 0, set it off otherwise
                "movl     $0, %eax" ++ "\n" ++  -- zero out EAX (doesn't change FLAGS)
                "sete     %al"      ++ "\n"     -- set AL register (the lower byte of EAX) to 1 if ZF is on
  | op=='~' = "not      %eax"     ++ "\n"
  | otherwise = "Unknown unary operator."

generateTerm :: Expression -> String
generateTerm = undefined

generateFactor :: Expression -> String
generateFactor = undefined

generateExpression :: Expression -> String
-- TODO - constant is now a Factor
generateExpression = undefined
--generateExpression (Constant ex) = "movl     $"
--                                 ++ show ex
--                                 ++ ", %eax"
--                                 ++ "\n"
-- TODO - unaryOperation is now a Factor
-- generateExpression (UnaryOperation unop exp) =  generateExpression exp ++ generateUnaryOperation unop

generateStatement :: [Statement] -> String
generateStatement [Statement s ex]
  | s==Return = generateExpression ex
                  ++ "ret"
  | otherwise   = ""

generateBody :: Body -> String
generateBody (Body s) = generateStatement s

generateParams :: Params -> String
generateParams (Params [])     = undefined
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
                          exitSuccess
                  else return (nub (concatMap set args), files)

              (_,_,errs)    -> do
                hPutStrLn stderr (concat errs ++ usageInfo header flags)
                exitWith (ExitFailure 1)
              where header = "Usage: cParser <source.c> [-o] <assembly.s> [-i] <instruction_set>"
                    set f = [f]

-- These two functions do basically the same thing. They take a list of flags and match a datatype then return its
-- string value. See how I could abstract them out so that I can pass any datatype and abstract away its string value.
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
    empty          -> "Could not match flag in the list of arguments." -- Not matched!


main :: IO ()
main = do
  putStrLn (unlines haskii)
  (as, fs) <- getArgs >>= parse
  let ins = filterInstructionSet as
      ass = filterAssemblyOutput as
      file = head fs
  putStrLn ("[INFO] Parsing source file " ++ show (head fs)) >>
    readFile file >>= \ source ->
       case runParser program source of
        Right (source, ast) ->
          putStrLn ("[INFO] Parsed as the following AST:\n" ++ show ast ++ "\n") >>
          putStrLn ("[INFO] Instruction set: " ++ ins ++ "[INFO] Assembly:\n") >> -- NOTE: Only x86 for now
          putStrLn asm >>
          writeFile ass asm >>
          putStrLn ("\n[INFO] Assembly code was written to: " ++ ass)
            where
              asm = generateAssembly ast

        Left e ->
          putStrLn ("[ERROR] Error while parsing:\n" ++ show e)



