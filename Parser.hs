module Parser where

import Data.Char
import Data.Tuple
import Control.Applicative

lambda :: Char
lambda = '\\'

-- TODO: implement own show
data Statement
    = Eval Expr
    | Define Expr Expr
    deriving (Show, Eq)

data Expr
    = Var String Int    -- x
    | Def Expr Expr     -- λx.x
    | Appl Expr Expr    -- (λx.x y)
    deriving (Eq)

instance Show Expr where
    show :: Expr -> String
    show (Var name _) = name
    show (Def (Var var _) expr) = "\\" ++ var ++ "." ++ show expr
    show (Appl expr1 expr2) = "(" ++ show expr1 ++ " " ++ show expr2 ++ ")"


newtype Parser a = Parser {run :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \str -> do
        (str', x) <- p str
        return (str', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \str -> Just(str, x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser f) <*> (Parser p) = Parser $ \str -> do
        (str', g) <- f str
        (str'', x) <- p str'
        return (str'', g x)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ const Nothing

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 =
        Parser $ \str -> p1 str <|> p2 str

isVarName :: Char -> Bool
isVarName = flip notElem ['(', ')', '=', '.', ' ', '\t', '\n', '\r', lambda]

charP :: Char -> Parser Char
charP c = Parser f
    where
        f (x:xs)
            | x == c = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

-- x, Add, 3 ...
varP :: Parser Expr
varP = Parser $ \str ->
    case swap $ span isVarName str of
        (_, "")     -> Nothing
        (str', var) -> Just(str', Var var (-1))

-- \x.x
defP :: Parser Expr
defP = Def <$> (charP lambda *> ws *> varP <* ws <* charP '.') <*> exprP

-- (f x)
applP :: Parser Expr
applP = Appl <$> (charP '(' *> ws *> exprP <* ws) <*> (exprP <* ws <* charP ')')

ws :: Parser String
ws = Parser (pure . swap . span isSpace)

exprP :: Parser Expr
exprP = ws *> (defP <|> applP <|> varP) <* ws

defineP :: Parser Statement
defineP = Define <$> (ws *> varP <* ws <* charP '=') <*> exprP

stmntP :: Parser Statement
stmntP = ws *> defineP <|> (Eval <$> exprP) <* ws