module TheParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Language(emptyDef)
import AbstractSyntaxTree
import Semantics

parseProgram:: String -> Either ParseError Program
parseProgram input = myParse program input

--Defines the valid language "tokens" 
languageDef =
  emptyDef { Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.reservedNames   = [ "IF"
                                     , "THEN"
                                     , "ELSE"
                                     , "WHILE"
                                     , "BEGIN"
                                     , "END"
                                     , "RETURN"
                                     , "FUNCTION"
                                     , "VAR"
                                     ]
           , Token.reservedOpNames = []
           }
           
--makes a set of tokens accepted by the defined language
lexer = Token.makeTokenParser languageDef
  
--Extracting the useful tokens that will need to be accessed often, and giving them more convenient names/handles
token_reservedOp = Token.operator lexer
tokenIdentifier  = Token.identifier lexer
tokenNat         = Token.natural    lexer
tokenInt         = Token.integer    lexer
semi             = Token.semi       lexer
parens           = Token.parens     lexer
comma            = Token.comma      lexer

tokenIF         = Token.reserved lexer "IF"
tokenTHEN       = Token.reserved lexer "THEN"
tokenELSE       = Token.reserved lexer "ELSE"
tokenWHILE      = Token.reserved lexer "WHILE"
tokenBEGIN      = Token.reserved lexer "BEGIN"
tokenEND        = Token.reserved lexer "END"
tokenRETURN     = Token.reserved lexer "RETURN"
tokenFUNCTION   = Token.reserved lexer "FUNCTION"
tokenVAR        = Token.reserved lexer "VARS"

tokenPlus       = char '+' <+-> spaces0 >> return Plus
tokenMinus      = char '-' <+-> spaces0 >> return Minus
tokenMultiply   = char '*' <+-> spaces0 >> return Multiply
tokenDivide     = char '/' <+-> spaces0 >> return Divide
tokenGreater    = char '>' <+-> spaces0 >> return GreaterThan
tokenLess       = char '<' <+-> spaces0 >> return LessThan
tokenEquals     = char '=' <+-> char '=' <+-> spaces0 >> return Equals
tokenLetEq      = char '=' <+-> spaces0

myParse :: Parser a -> String -> Either ParseError a
myParse p s = parse (p <+-> eof) "myparser" s
              
program :: Parser Program
program = spaces <-+> liftM Program (many function)

function :: Parser Function
function = tokenFUNCTION <-+> liftM4 Function ident arguments variables block 
              
arguments :: Parser Arguments
arguments = try(parens (liftM Arguments idList))

variables :: Parser Variables
variables = tokenVAR  <-+> liftM Variables idList <+-> semi

idList :: Parser [ID] 
idList = ident `sepBy1` (char ',' <-+> many space)
              
block :: Parser Statements
block =  tokenBEGIN <-+> statements <+-> tokenEND             

statements :: Parser Statements
statements = liftM Statements (option [] (endBy1 statement semi))       
              
--
statement :: Parser Statement 
statement  = liftM2 AssignStatement (ident <+-> tokenLetEq) expression
          --If Else
           <|> try (liftM3 IfElseStatement (tokenIF <-+> ident) 
                                           (tokenTHEN <-+> block)  
                                           (tokenELSE <-+> block))
          --If 
           <|> liftM2 IfStatement (tokenIF <-+> ident) 
                                  (tokenTHEN <-+> block)
          --Return ID
           <|> liftM ReturnStatement (tokenRETURN <-+> ident)
          
--Parses a valid expression using other parsers to parse the underlying sections, combining the results into a expression data type
expression :: Parser Expression
expression = try (liftM2 FunctionExpression ident arguments)
          <|> try (parens (liftM3 OperatorExpression expression op expression))
          <|> try (liftM IDExpression ident )
          <|> try (liftM NumExpression num)
          
--Parses a valid integer, positive or negative and returns it in Number data type
num :: Parser Number
num = liftM Number tokenInt
        
--Parses a valid identifier as defined in the language definition and returns it as Identifier data type
ident :: Parser ID
ident = try (liftM ID tokenIdentifier)

--Parses a valid operator and returns the operator in Op data type              
op :: Parser Op
op =  try (tokenPlus)
  <|> try (tokenMinus)
  <|> tokenMultiply  
  <|> tokenDivide 
  <|> tokenLess 
  <|> tokenGreater 
  <|> tokenEquals
              
--------------------------------------------------------------------------------------------------------------------
--Helper functions to get rid of the unnecessary parsers used to parse the static tokens eg: IF, ELSE, BEGIN, etc 

--Takes in 2 parsers and only returns the result of the 2nd one
(<-+>) :: Parser a -> Parser b -> Parser b 
(<-+>) c p = do 
  c
  extract <- p
  return extract

--Takes in 2 parsers and only returns the result of the 1st one 
(<+->) :: Parser a -> Parser b -> Parser a 
(<+->) p c = do 
  extract <- p
  c
  return extract
 
--Creates a parser for at least 1 space
spaces :: Parser ()
spaces = skipMany1 space

spaces0 :: Parser ()
spaces0 = skipMany space