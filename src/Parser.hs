{-
Grammar
Expr -> Expr + term | Expr - term | Term
Term ->  Factor ^ Term | Term * Factor | Factor / Term | Factor 
Factor -> (Expr) | Func | Constant | X
Func -> sin Factor | cos Factor | ...
Constant -> double
X -> x

Parser for mathematical expressions
-}
module Parser where

import Text.Parsec (try,parse, endBy1)
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, digit, spaces, string)
import Text.Parsec.Combinator (many1, choice, chainl1)
import Control.Applicative ((<|>))

import Functions (Expr(..), Func(..))

--parse digit arguments then fucntion
parseArgs::Parser ([Char],Expr)
parseArgs = do
               m<-digit `endBy1` spaces
               f<-expr
               return (m,f)

symbol::Parser a->Parser a
symbol x = do
               spaces
               y<-x
               spaces
               return y

parseInt::Parser (Expr)
parseInt = do
            n <- many1 digit
            return $ Const (read n)

parseDouble:: Parser (Expr)
parseDouble = do
               n <- many1 digit
               dec <- char '.'
               n2 <- many1 digit
               return $ Const (read $ n ++ "." ++ n2)

constant :: Parser (Expr)
constant = try parseDouble <|> parseInt

variable::Parser (Expr)
variable = do
            char 'x'
            spaces
            return X

function::Parser (Expr)
function = do
            f <- h_function
            Apply f <$> factor

h_function::Parser Func
h_function = (string "log" >> return Log)
          <|> (string "exp" >> return Exp)
          <|> parseInv
          <|> (char 's' >> (string "qrt" >> return Sqrt) <|> parseSin )
          <|> parseCos
          <|> parseTan
               where
                    parseInv = char 'a' >> (string "sin" >> (char 'h' >> return Asinh)
                                                            <|> return Asin)
                                        <|> (string "cos" >> (char 'h' >> return Acosh)
                                                            <|> return Acos)
                                        <|> (string "tan" >> (char 'h' >> return Atanh)
                                                            <|> return Atan)
                    parseSin = string "in" >> (char 'h' >> return Sinh)
                                        <|> return Sin
                    parseCos = string "cos" >> (char 'h' >> return Cosh)
                                        <|> return Cos
                    parseTan = string "tan" >> (char 'h' >> return Tanh)
                                        <|> return Tan

factor::Parser (Expr)
factor = try (do
               symbol $ char '('
               e <- expr
               symbol $ char ')'
               return e
             )
          <|> function <|> constant <|> variable

term::Parser (Expr)
term = try power <|> chainl1 factor op <|> factor
          where
               power = do {f1<-factor; symbol $ char '^'; f2 <- term; return (f1 :^: f2)}
               op =  do {symbol $ char '*'; return (:*:)}
                    <|> do {symbol $ char '/'; return (:/:)}

expr::Parser (Expr)
expr = try (chainl1 term op) <|> term
          where
               op =  do {symbol $ char '+'; return (:+:)}
                    <|> do {symbol $ char '-'; return sub}
                         where sub f g = f :+: (Const (-1) :*: g)