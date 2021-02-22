{-# LANGUAGE FlexibleInstances #-}

import           Control.Applicative
import           Control.Monad
import           Data.Char

data BinOp = Add | Sub | Multiply
    deriving (Show)

data Expr = Constant Int
          | Binary BinOp Expr Expr
          deriving (Show)

data ParseError = ParseError Int String

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

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

parseChar :: Char -> Parser Char
parseChar x = Parser f
  where
    f (y:ys)
        | y == x    = Right (ys, x)
        | otherwise = Left $
                      ParseError
                      0 $ " Expected '" ++ [x] ++ "', but found '" ++ [y] ++ "'"
    f [] = Left $ ParseError 0 " Empty string."

expression :: Parser Expr
expression = sub
  where
    product = foldl1 (Binary Multiply) <$> factor `sepBy1` optional (parseChar '*')
    sum     = foldl1 (Binary Add)      <$> product `sepBy1` parseChar '+'
    sub     = foldr1 (Binary Sub)      <$> product `sepBy1` parseChar '-'
    factor  = int <|> between (parseChar '(') (parseChar ')') expression
    int     = Constant . read <$> some digit


parseString str =
    case parse expression "" str of
      Left e  -> error $ show e
      Right r -> r

main = undefined
