module Parser where

import Data.Char
import Data.Tuple
import Control.Applicative

lambda :: Char
lambda = '\\'

type VarName = String

data Statement
    = Eval Expr
    | Define Bool VarName Expr
    deriving (Eq, Show)

data Expr
    = Var VarName Int    -- x
    | Def VarName Expr   -- λx.x
    | Appl Expr Expr     -- (λx.x y)
    deriving (Eq)

instance Show Expr where
    show :: Expr -> String
    show (Var name _) = name
    show (Def var expr) = "\\" ++ var ++ "." ++ show expr
    show (Appl expr1 expr2) = show expr1 ++ " (" ++ show expr2 ++ ")"

newtype Parser a = Parser {runParser :: String -> Maybe (String, a)}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \str -> do
        (str', x) <- p str
        return (str', f x)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \str -> Just (str, x)

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
nameP :: Parser VarName
nameP = Parser $ \str ->
    case swap $ span isVarName str of
        (_, "")     -> Nothing
        (str', var) -> Just (str', var)

varP :: Parser Expr
varP = flip Var (-1) <$> nameP

-- \x.x
defP :: Parser Expr
defP = Def <$> (charP lambda *> ws *> nameP <* ws <* charP '.') <*> exprP

atomP :: Parser Expr
atomP = varP <|> (charP '(' *> ws *> exprP <* ws <* charP ')' )

itemsP :: Parser Expr
itemsP = foldl1 Appl <$> some (atomP <* ws)

ws :: Parser String
ws = Parser (pure . swap . span isSpace)

exprP :: Parser Expr
exprP = ws *> (defP <|> itemsP) <* ws

defineEP :: Parser Statement
defineEP = Define False <$> (ws *> nameP <* ws <* charP '=') <*> exprP

defineLP :: Parser Statement
defineLP = Define True <$> (ws *> nameP <* ws <* charP ':' <* charP '=') <*> exprP

defineP :: Parser Statement
defineP = defineLP <|> defineEP

stmntP :: Parser Statement
stmntP = ws *> defineP <|> (Eval <$> exprP) <* ws
