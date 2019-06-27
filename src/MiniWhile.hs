module MiniWhile where

import Data.Char
import Data.List
import ParserCon

-- ^ Parsing

data Program = Program [Stmt]
  deriving (Show, Eq)
data Stmt = Asgn Id Exp
          | While Exp [Stmt]
  deriving (Show, Eq)
data Exp = If Exp Exp Exp
         | LEq AExp AExp
         | GTh AExp AExp
         | Equ AExp AExp
         | NEq AExp AExp
         | Not Exp
         | AExp AExp
  deriving (Show, Eq)
data AExp = Num Integer
          | Var Id
          | Plus AExp AExp
          | Minus AExp AExp
          | Mult AExp AExp
          | Div AExp AExp
  deriving (Show, Eq)
type Id = String
 
idToken (TId id) = Just id
idToken _ = Nothing

numToken (TNum num) = Just num
numToken _ = Nothing

cmpToken (TCmp cmp) = Just cmp
cmpToken _ = Nothing

opToken (TOp op) = Just op
opToken _ = Nothing

cmpTokenToConstructor TLEq = LEq
cmpTokenToConstructor TGTh = GTh
cmpTokenToConstructor TEq = Equ
cmpTokenToConstructor TNEq = NEq

opTokenToConstructor TPlus = Plus
opTokenToConstructor TMinus = Minus
opTokenToConstructor TMult = Mult
opTokenToConstructor TDiv = Div

parseIf :: Parser Token Exp
parseIf = pure If
  <* lit TIf
  <*> parseExp
  <* lit TThen
  <*> parseExp
  <* lit TElse
  <*> parseExp
  <* lit TFi
  
parseCmp :: Parser Token Exp
parseCmp = do
  lExp <- parseAExp
  cmp <- try cmpToken
  rExp <- parseAExp
  return $ (cmpTokenToConstructor cmp) lExp rExp

parseNot :: Parser Token Exp
parseNot = pure Not <* lit TNot <*> parseExp
  
parseNum :: Parser Token AExp
parseNum = Num <$> try numToken

parseVar :: Parser Token AExp
parseVar = Var <$> try idToken

parseOp :: Parser Token AExp
parseOp = do
  lit LBrack
  lExp <- parseAExp
  op <- try opToken
  rExp <- parseAExp
  lit RBrack
  return $ (opTokenToConstructor op) lExp rExp

parseExp :: Parser Token Exp
parseExp = parseIf
  <|> parseCmp
  <|> parseNot
  <|> (AExp <$> parseAExp)

parseAExp :: Parser Token AExp
parseAExp = parseNum
  <|> parseVar
  <|> parseOp

parseAssign :: Parser Token Stmt
parseAssign = pure Asgn <*> try idToken <* lit TAsgn <*> parseExp

parseWhile :: Parser Token Stmt
parseWhile = pure While <* lit TWhile <*> parseExp <* lit TDo <*> parseStatements <* lit TDone
  
parseString :: String -> Maybe Program
parseString s = do
  l <- lexer s
  parse parser l

parseSep :: Parser Token ()
parseSep = () <$ lit TSep

parseStatement :: Parser Token Stmt
parseStatement = parseAssign <|> parseWhile

parseStatements :: Parser Token [Stmt]
parseStatements = pIntersperse parseStatement parseSep

parser :: Parser Token Program
parser = Program <$> parseStatements


-- ^ Lexing
-- Use this lexer to tokenize the input before parsing

data TOrd = TLEq | TGTh | TEq | TNEq
  deriving (Show, Eq)

data TOp = TPlus | TMinus | TMult | TDiv
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
t_cmp = TCmp TLEq <$ string "<=" <|> TCmp TGTh <$ string ">" <|> TCmp TEq <$ string "==" <|> TCmp TNEq <$ string "! ="
t_brack = LBrack <$ string "(" <|> RBrack <$ string ")"
t_op = TOp TPlus <$ string "+" <|> TOp TMinus <$ string "-" <|> TOp TMult <$ string "*" <|> TOp TDiv <$ string "/"
t_keyword = TWhile <$ string "while" <|> TDone <$ string "done" <|> TDo <$ string "do"
  <|> TIf <$ string "if" <|> TThen <$ string "then" <|> TElse <$ string "else"
  <|> TFi <$ string "fi" <|> TNot <$ string "not"

-- ^ Utilities
string xs = foldr (liftA2 (:)) (pure []) $  map lit xs 
many1 p = (:) <$> p <*> many p
