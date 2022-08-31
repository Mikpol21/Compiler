module Parser.ASTParser where

import AST
import Parser.GenericParser
import Control.Applicative
import MonadStack
import Prelude hiding (fail, log)

reservedWords :: [String]
reservedWords = ["let", "lambda", "in", "=", "end", "else", "then", "if", "true", "false", "::", "Bool", "Int", "Char", ";", "ref", "of", "List"]

var :: Parser String
var = do
    name <- word
    if name `elem` reservedWords 
    then fail (name ++ " is reserved") 
    else return name 

parseExpr:: Parser Expr
parseExpr = parseSeq
    
parseAcyclicExpr = parseParens parseExpr
    <|> parseNum
    <|> parseBool
    <|> parseChar
    <|> parseUnit
    <|> parseVar

parseTailLess =
    parseLet <|> parseLambda <|> parseIf <|> parseApply
    

parseType :: Parser Type
parseType = parseFunction

parseSimpleType = 
    parseParens parseType
    <|> parseUnitT
    <|> parseBoolT
    <|> parseIntT
    <|> parseCharT
    <|> parseTagged
    <|> parseRef
    <|> parseList
    <|> parseVarT

-------------------------------------------------------

parseLet = do
    symbol "let"
    param <- parseTypedParam
    symbol "="
    expr1 <- parseTailLess
    symbol "in"
    expr2 <- parseTailLess
    return $ Let param expr1 expr2
parseLambda = do
    symbol "lambda"
    params <- chainr (symbol "," >> return (.)) (fmap Lambda $ parseParens parseTypedParam)
    symbol "->"
    expr <- parseTailLess
    return $ params expr
parseIf = do
    symbol "if"
    cond <- parseTailLess
    symbol "then"
    expr1 <- parseTailLess
    symbol "else"
    expr2 <- parseTailLess
    return $ If cond expr1 expr2
parseVar = 
    fmap Variable var
parseNum =
    fmap Num num
parseBool =
    (symbol "true" >> return (Bool True)) <|> (symbol "false" >> return (Bool False))
parseChar = do
    string "'"
    c <- consume
    string "'"
    return $ Char c
parseUnit = do
    symbol "()"
    return Unit
parseSeq = (do
    expr1 <- parseTyped
    symbol ";"
    expr2 <- parseSeq
    return $ Seq expr1 expr2
    ) <|> parseTyped
parseTyped = (do
    expr <- parseApply
    symbol "::"
    t <- parseType
    return $ Typed expr t
    ) <|> parseTailLess
parseApply = chainl (string " " >> return Apply) parseAcyclicExpr


parseTypedParam = (do
    name <- var
    symbol "::"
    t <- parseType
    return $ TypedParam name t) <|> fail "expected typed param"

-------------------------------------------------------

parseUnitT = do
    symbol "()"
    return UnitT
parseBoolT = do
    symbol "Bool"
    return BoolT
parseIntT = do
    symbol "Int"
    return IntT
parseCharT = do
    symbol "Char"
    return CharT
parseList = do
    symbol "["
    typeExpr <- parseType
    symbol "]"
    return $ ListT typeExpr
parseTagged = do
    name <- word
    symbol "of"
    t <- parseType
    return $ Tagged name t
parseRef = do
    symbol "ref of"
    t <- parseType
    return $ Ref t
parseVarT = do
    name <- var
    return $ VariableT name

parseProduct =
    chainl (symbol "*" >> return (:*:)) parseSimpleType
parseCoproduct =
    chainl (symbol "+" >> return (:+:)) parseProduct
    <|> parseProduct
parseFunction =
    chainr (symbol "->" >> return (:->)) parseCoproduct

-------------------------------------------------------

parseParens parser = 
    do
        symbol "("
        expr <- parser
        symbol ")"
        return expr
    <|> do
        symbol "{"
        expr <- parser
        symbol "}"
        return expr

log :: String -> Parser ()
log msg =  Parser $ \str -> fmap (\x -> (x, str)) (logMsg msg)


run parser str = runPipe Config $ runParser parser str