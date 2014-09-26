{-|
  Processes AST "Expression"s to produce the associated
  Intermediate Code (IC).
-}


module ExpressionProcessing where

import State
import Instructions
import StringProcessing
import AbstractSyntaxTree
import Data.List


{-|
  Process an "Expression" in a given "State" to produce some (IC)
  and a new "State".

  Takes two arguments:
    - an "Expression" to process.
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.

  The "Expression" is processed according to its type 
  ("NumExpression", "IDExpression", "FunctionExpression" or
  OperatorExpression")
 
-}
expr :: Expression -> State -> (State, String)

expr (NumExpression n') state
   = (updtRegs state, ldCnst (nxtReg state) n')

expr (IDExpression id') state
   = (updtRegs state, ldVar (nxtReg state) id')

expr (FunctionExpression id' (Arguments args)) state
   = bind (loadArgs args state) (callFunc id' args)

expr (OperatorExpression exp1 op exp2) state
  = let (state'@(Used b1 (Registers r1)), s1) = (expr exp1 state)
        (state''@(Used b2 (Registers r2)), s2) = (expr exp2 state')
    in (updtRegs state'',
        s1 ++ s2 ++ bgnInstr ++ (operator op) ++ " " ++ (reg (nxtReg state''))
        ++ " " ++ (reg r1) ++ " " ++ (reg r2) ++ endInstr)


{-|
  Load arguments into registers.

  Takes two arguments:
    - a list of "ID"s representing arguments to load
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
loadArgs :: [ID] -> State -> (State, String)
loadArgs [] state           = (state, "")
loadArgs (a:as) state   = bind (loadArg a state) (loadArgs as)


{-|
  Load an argument into the next unassigned register.

  Takes two arguments:
    - an "ID" representing an argument to load
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
loadArg :: ID -> State -> (State, String)
loadArg id' state = (updtRegs state, (ldVar (nxtReg state) id'))


{-|
  Produce IC to call a function with some arguments.

  Takes three arguments:
    - an "ID" representing the name of the function to call.
    - a list of "ID"s representing the arguments to pass in.
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
callFunc :: ID -> [ID] -> State -> (State, String)
callFunc (ID id) ids state
      = let n = toInteger (length ids) 
            r = prvReg state
        in (updtRegs state, bgnInstr ++ "call " ++ (reg (r+1)) ++ " " ++ id ++ (listRegs (r-n+1) (r)) ++ endInstr)


{-|
  Produce a list of registers between two numbers.
  For example: listRegs 3 5 would produce "r3 r4 r5"

  Takes two arguments: "Integer"s representing the first and last
  register to list.

  Returns a "String" containing the space-separated list of 
  registers.
-}
listRegs :: Integer -> Integer -> String
listRegs lo hi 
    = foldr (\r -> (++) (" " ++ r)) ""
            (unfoldr (\(l,h) -> if (l <= h) then Just (("r" ++ (show l)), (l+1,h)) else Nothing) (lo,hi))


{-|
  Takes an Abstract Syntax Tree "Op" node and returns the
  corresponding IC instruction as a "String".
-}
operator :: Op -> String
operator Plus        = "add"
operator Minus       = "sub"
operator Divide      = "div"
operator Multiply    = "mul"
operator GreaterThan = "gt"
operator LessThan    = "lt"
operator Equals      = "eq"


