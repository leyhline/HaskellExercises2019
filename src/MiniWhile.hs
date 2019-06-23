module MiniWhile where

import Data.Char
import Data.List
import ParserCon

-- ^ Parsing

data Program = Program [Stmt]
  deriving (Show, Eq)
data Stmt = Asgn Id Exp
            -- Add more things here
  deriving (Show, Eq)
data Exp = Num Integer
         | Var Id
         -- Add more things here
  deriving (Show, Eq)
type Id = String


parseString :: String -> Maybe Program
parseString s = do
  l <- lexer s
  parse parser l

parser :: Parser Token Program
parser = undefined -- Implement this


-- ^ Lexing
-- Use this lexer to tokenize the input before parsing

data TOrd = LEq | GTh | Eq | NEq
  deriving (Show, Eq)

data TOp = Plus | Minus | Mult | Div
  deriving (Show, Eq)

data Token = TSep -- ';'
           | TAsgn -- ': ='
           | TNum Integer
           | TId Id
           | TWhile
           | TDo
           | TDone
           | TIf
           | TThen
           | TElse
           | TFi
           | TNot
           | TCmp TOrd
           | TOp TOp
           | LBrack
           | RBrack
  deriving (Eq, Show)

lexer :: String -> Maybe [Token]
lexer = parse $ many1 (skipSpace *> p_tok) <* skipSpace

skipSpace = many (satisfy isSpace)
p_tok =
  t_keyword
  <|> t_alnum
  <|> t_sep
  <|> t_asgn
  <|> t_num
  <|> t_cmp
  <|> t_brack
  <|> t_op

t_num = TNum . read <$> many1 (satisfy isDigit)
t_sep = TSep <$ lit ';'
t_asgn = TAsgn <$ string ": ="
t_alnum = fmap TId $ (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
t_cmp = TCmp LEq <$ string "<=" <|> TCmp GTh <$ string ">" <|> TCmp Eq <$ string "==" <|> TCmp NEq <$ string "! ="
t_brack = LBrack <$ string "(" <|> RBrack <$ string ")"
t_op = TOp Plus <$ string "+" <|> TOp Minus <$ string "-" <|> TOp Mult <$ string "*" <|> TOp Div <$ string "/"
t_keyword = TWhile <$ string "while" <|> TDone <$ string "done" <|> TDo <$ string "do"
  <|> TIf <$ string "if" <|> TThen <$ string "then" <|> TElse <$ string "else"
  <|> TFi <$ string "fi" <|> TNot <$ string "not"

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs 
many1 p = (:) <$> p <*> many p
