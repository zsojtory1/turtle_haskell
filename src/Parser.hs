module Parser where
import Ast
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language
import Text.Parsec.Token
import Graphics.Gloss.Data.Color

{- This file contains the parser, which uses Parsec to parse a MiniTurtle
 - program into the AST. -}

-- Parser type: a Parsec parser, operating on strings, with no state
type Parser a = Parsec String () a

-- Two examples provided: parsing a colour and an integer
color :: Parser Color
color =
    (reserved lexer "red" >> return red)
    <|> (reserved lexer "green" >> return green)
    <|> (reserved lexer "blue" >> return blue)
    <|> (reserved lexer "yellow" >> return yellow)
    <|> (reserved lexer "black" >> return black)

int :: Parser Expr
int = do
    i <- (integer lexer)
    return (EInt (fromIntegral i))

unit :: Parser Expr
unit = char '(' >> char ')' >> (return EUnit)

var :: Parser Expr
var = do
    v <- identifier lexer
    return (EVar v)

changeColor :: Parser Expr
changeColor = do
    reserved lexer "changeColor"
    col <- (parens lexer color)
    return (EChangeColor col)

{- Exercise 4 -}
bool :: Parser Expr
bool = 
    (reserved lexer "true" >> return (EBool True))
    <|> (reserved lexer "false" >> return (EBool False))

action :: Parser Expr
action = 
    changeColor
    <|> move
    <|> rotate
    <|> pen

pen :: Parser Expr
pen = 
    (reserved lexer "penUp" >> char '(' >> char ')' >> return EPenUp)
    <|> (reserved lexer "penDown" >> char '(' >> char ')' >> return EPenDown)

move :: Parser Expr
move = do
    d <- direction
    e <- (parens lexer expression)
    return $ (EMove d e)

direction :: Parser Direction
direction = 
    (reserved lexer "forward" >> return Forward)
    <|> (reserved lexer "backward" >> return Backward)

rotate :: Parser Expr
rotate = do
    rd <- rotateDirection
    e <- (parens lexer expression)
    return $ (ERotate rd e)

rotateDirection :: Parser RotateDirection
rotateDirection = 
    (reserved lexer "left" >> return RotateLeft)
    <|> (reserved lexer "right" >> return RotateRight)

letFunc :: Parser Expr
letFunc = do
    reserved lexer "let"
    i <- identifier lexer
    reservedOp lexer "="
    e1 <- expression
    reserved lexer "in"
    e2 <- expression
    return $ (ELet i e1 e2)

appFunc :: Parser Expr
appFunc = do
    id <- identifier lexer
    a <- (parens lexer argList)
    return $ (EApp id a)

argList :: Parser [Expr]
argList = do
    a <- commaSep lexer expression
    return a

ifFunc :: Parser Expr
ifFunc = do
    reserved lexer "if"
    e1 <- expression
    e2 <- block
    reserved lexer "else"
    e3 <- block
    return $ (EIf e1 e2 e3)

block :: Parser Expr
block = do
    lexeme lexer (char '{')
    e <- expression
    lexeme lexer (char '}')
    return e

whileFunc :: Parser Expr
whileFunc = do 
    reserved lexer "while"
    cond <- (parens lexer expression)
    e1 <- block
    return $ (EWhile cond e1)

expr :: Parser Expr
expr =
  (try (lexeme lexer unit))
  <|> (try (lexeme lexer appFunc))
  <|> (try (lexeme lexer var))
  <|> (try (lexeme lexer int))
  <|> (try (lexeme lexer bool))
  <|> (try (lexeme lexer ifFunc))
  <|> (try (lexeme lexer whileFunc))
  <|> (try (lexeme lexer action))
  <|> (try (lexeme lexer letFunc))
  <|> (try (lexeme lexer (parens lexer expression)))

program :: Parser Program
program = do
    d <- many def
    e <- expression
    return (d, e)

def :: Parser (DefName, Definition)
def = do
    reserved lexer "def"
    i <- identifier lexer
    p <- (parens lexer paramList)
    reservedOp lexer ":"
    t <- typeFunc
    e <- block
    return (i, (p, t, e))

paramList :: Parser [(Parameter, Type)]
paramList = do
    pList <- (commaSep lexer parameter)
    return pList

parameter :: Parser (Parameter, Type)
parameter = do
    i <- identifier lexer
    reservedOp lexer ":"
    t <- typeFunc
    return (i, t)

typeFunc :: Parser Type 
typeFunc = 
    (reserved lexer "Int" >> return TyInt)
    <|> (reserved lexer "Bool" >> return TyBool)
    <|> (reserved lexer "Unit" >> return TyUnit)

{- The remainder of the file is boilerplate which we have set up for you.
 - This includes setting up the expression parser, which means you do not
 - need to worry about unary and binary operators. -}
keywords = [
        "forward", "backward", "left", "right",
        "penUp", "penDown", "changeColor", "red",
        "green", "blue", "yellow", "black", "if", "else",
        "let", "in", "true", "false", "def"
    ]

ops = [
        "+", "-", "*", "/", "==", "!=", ">", "<",
        ">=", "<=", "&&", "||"
    ]

langDef :: LanguageDef ()
langDef = emptyDef {
        commentStart = "/*",
        commentEnd = "*/",
        commentLine = "//",
        identStart = letter <|> (char '_'),
        identLetter = alphaNum <|> char '_',
        opStart = oneOf ";!&*+/<=>|-",
        opLetter = oneOf "&/=|",
        reservedNames = keywords,
        reservedOpNames = ops,
        caseSensitive = True
    }

lexer :: TokenParser ()
lexer = makeTokenParser langDef

binary  name fun assoc =
    Infix ( do { reservedOp lexer name; return fun }) assoc

prefix  name fun =
    Prefix (do { reservedOp lexer name; return fun })

postfix name fun =
    Postfix (do{ reservedOp lexer name; return fun })

table =
    [  [prefix "-" (EUnOp Neg), prefix "!" (EUnOp Not) ]
       , [binary "*" (EBinOp Mul) AssocLeft, binary "/" (EBinOp Div) AssocLeft ]
       , [binary "+" (EBinOp Add) AssocLeft, binary "-" (EBinOp Sub) AssocLeft ]
       , [ binary "<" (EBinOp Less) AssocLeft, binary "<=" (EBinOp LessEq) AssocLeft,
           binary ">" (EBinOp Greater) AssocLeft, binary ">=" (EBinOp GreaterEq) AssocLeft,
           binary "==" (EBinOp Eq) AssocLeft, binary "!=" (EBinOp Neq) AssocLeft
         ]
       , [binary "&&" (EBinOp And) AssocLeft, binary "||" (EBinOp Or) AssocLeft]
       , [binary ";" ESeq AssocRight]
    ]

expression :: Parser Expr
expression = buildExpressionParser table expr

-- External definition, which runs the parser
parse :: String -> String -> Either ParseError Program
parse = runP program ()
