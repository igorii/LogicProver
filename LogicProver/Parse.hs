module LogicProver.Parse (parseString, parseFile, prop) where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Control.Applicative ((<*))
import Text.Parsec.Expr

import LogicProver.Lang

languageDef =
  emptyDef { Token.commentLine = "--"
           , Token.identStart = letter
           , Token.identLetter = alphaNum
           , Token.reservedOpNames = [ "~" , "and" , "or" , "impl" ]
           , Token.reservedNames = [ "and" , "or" , "impl" ] 
           }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reserved   = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens lexer
opTable    = [ [Prefix (reservedOp "~"    >> return PNegate)]
             , [Infix  (reservedOp "and"  >> return PAnd) AssocLeft]
             , [Infix  (reservedOp "or"   >> return POr) AssocLeft]
             , [Infix  (reservedOp "impl" >> return PCond) AssocLeft]
             ]

prop :: Parser Prop
prop = buildExpressionParser opTable term

term =  parens prop 
    <|> liftM PVar identifier

parseString    :: Parser Prop -> String -> Either ParseError Prop
parseString e s = parse (e <* eof) "" s
parseFile      :: Parser Prop -> FilePath -> Either ParseError Prop
parseFile e f   = parse (e <* eof) "" f



