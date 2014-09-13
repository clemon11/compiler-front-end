module AbstractSyntaxTree where

data Program = Program [Function]
  deriving Show

data Function = Function ID Arguments Variables Statements
  deriving Show

data Arguments = Arguments [ID]
  deriving Show

data Variables = Variables [ID]
  deriving Show

data Statement = AssignStatement ID Expression 
               | IfStatement ID Statements
               | IfElseStatement ID Statements Statements
               | ReturnStatement ID
               deriving Show
               
data Expression = NumExpression Number
                | IDExpression ID
                | FunctionExpression ID Arguments
                | OperatorExpresssion Expression Op Expression
               deriving Show
               
data Statements = Statements [Statement]
             deriving Show
               
data ID = ID String
  deriving Show

data Number = Number Integer
  deriving Show
  
data Op = Plus 
        | Minus
        | Divide
        | Multiply
        | GreaterThan
        | LessThan
        | Equals
        deriving Show
        
