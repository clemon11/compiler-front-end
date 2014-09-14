import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad (liftM, liftM2, liftM3, liftM4)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.Parsec.Language(emptyDef)
import AbstractSyntaxTree

main :: IO ()
main = do
  print (readOp "*")

  
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
           , Token.reservedOpNames = ["+", "-", "*", "/", "<", ">","==", "="]

           }
lexer = Token.makeTokenParser languageDef
  

token_reservedOp = Token.operator lexer
tokenIdentifier  = Token.identifier lexer
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


tokenPlus       = Token.reservedOp lexer "+" >> return Plus
tokenMinus      = Token.reservedOp lexer "-" >> return Minus
tokenMultiply   = Token.reservedOp lexer "*" >> return Multiply
tokenDivide     = Token.reservedOp lexer "/" >> return Divide
tokenGreater    = Token.reservedOp lexer ">" >> return GreaterThan
tokenLess       = Token.reservedOp lexer "<" >> return LessThan
tokenEquals     = Token.reservedOp lexer "==" >> return Equals
tokenLetEq      = Token.reservedOp lexer "="

        
passToken :: Parser a -> Parser a
passToken t = do
  extract <- t
  return extract

op :: Parser Op
op =  tokenPlus 
  <|> tokenMinus 
  <|> tokenMultiply  
  <|> tokenDivide 
  <|> tokenLess 
  <|> tokenGreater 
  <|> tokenEquals
         
          
readOp :: String -> String
readOp input = case parse op "MyLanguage" input of
              Left error -> "Error: Operator Not Found: " ++ show error
              Right value -> show value
              
              
expression :: Parser Expression
expression = try (liftM2 FunctionExpression ident arguments)
          <|> try (parens (liftM3 OperatorExpresssion expression op expression))
          <|> try (liftM IDExpression ident )
          <|> try (liftM NumExpression num)
          
num :: Parser Number
num = liftM Number (liftM read (endWith (startWith minus digits ) space ))
   where minus = option ' ' (char '-')
         digits = (many1 digit)
        
ident :: Parser ID
ident = try (liftM ID tokenIdentifier)

arguments :: Parser Arguments
arguments = try(parens (liftM Arguments idList))

idList :: Parser [ID] 
idList = try (sepBy1 (ident) ((char ',') >> (many space)))

statement :: Parser Statement 
statement  = liftM2 AssignStatement (chuckNextToken ident tokenLetEq) (expression)

          <|> try (liftM3 IfElseStatement (chuckPrevToken tokenIF ident) (chuckPrevToken tokenTHEN block)  (chuckPrevToken tokenELSE block))
          --if 
          <|> try (liftM2 IfStatement (chuckPrevToken tokenIF ident) (chuckPrevToken tokenTHEN block) )
          --Return ID
          <|> liftM ReturnStatement (chuckPrevToken tokenRETURN ident)
          
          
block :: Parser Statements
block = (chuckNextToken (chuckPrevToken tokenBEGIN statements) tokenEND)
  
statements :: Parser Statements
statements = try (liftM Statements ( sepBy statement semi))
  
variables :: Parser Variables
variables = (chuckNextToken (tokenVAR  >> (liftM Variables idList)) semi)

function :: Parser Function
function = tokenFUNCTION >> (liftM4 Function ident arguments variables block)

program :: Parser Program
program = liftM Program (many function)

chuckPrevToken :: Parser a -> Parser b -> Parser b 
chuckPrevToken c p = do 
  c
  extract <- p
  return extract

chuckNextToken :: Parser a -> Parser b -> Parser a 
chuckNextToken p c = do 
  extract <- p
  c
  return extract
  
startWith :: Monad m => m a -> m [a] -> m [a]
startWith x xs = liftM2 (:) x xs

endWith :: Monad m => m [a] -> m a -> m [a]
endWith xs x =  liftM2 (++) xs (liftM (\y -> [y]) x)
  
spaces :: Parser ()
spaces = skipMany1 space
