module ExpressionProcessing where

import State
import Instructions
import StringProcessing
import AbstractSyntaxTree
import Data.List


---- Expression processing  -----

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


loadArgs :: [ID] -> State -> (State, String)
loadArgs [] state           = (state, "")
loadArgs (a:as) state   = bind (loadArg a state) (loadArgs as)


loadArg :: ID -> State -> (State, String)
loadArg id' state = (updtRegs state, (ldVar (nxtReg state) id'))

callFunc :: ID -> [ID] -> State -> (State, String)
callFunc (ID id) ids state
      = let n = toInteger (length ids) 
            r = prvReg state
        in (updtRegs state, bgnInstr ++ "call " ++ (reg (r+1)) ++ " " ++ id ++ (listRegs (r-n+1) (r)) ++ endInstr)


listRegs :: Integer -> Integer -> String
listRegs lo hi 
    = foldr (\r -> (++) (" " ++ r)) ""
            (unfoldr (\(l,h) -> if (l <= h) then Just (("r" ++ (show l)), (l+1,h)) else Nothing) (lo,hi))


operator :: Op -> String
operator Plus        = "add"
operator Minus       = "sub"
operator Divide      = "div"
operator Multiply    = "mul"
operator GreaterThan = "gt"
operator LessThan    = "lt"
operator Equals      = "eq"


