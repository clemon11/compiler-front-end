module GenerateIntermediate where

import State
import AbstractSyntaxTree
import Data.Either
import Data.List -- unfoldr
import Data.Char (isSpace) -- for stripping leading whitespace


------- Convert AST to String representing Intermediate Code ---------

-- TODO:
-- left = "( "; loadIntoNextReg id; etc
-- tidy up generateIntermediate, arguments, statements, psStatements

generateIntermediate :: Program -> String
generateIntermediate (Program fs) 
   = "( " ++ (foldr (\(Function (ID id) a v s) 
                -> (++) ("( " ++ id ++ " (" ++ (arguments a) ++ ") " ++ (statements s) ++ " )" ))
           "" fs) ++ " )"

arguments :: Arguments -> String
arguments (Arguments as) = dropWhile isSpace (foldr (\(ID a) -> (++) (" " ++ a)) "" as)


statements :: Statements -> String
statements (Statements ss) = "(0" ++ (psStatements (Used (Blocks 0) (Registers 0)) ss) ++ ")"


psStatements :: State -> [Statement] -> String
psStatements u [] = ""
psStatements state (s:ss) 
   = let (newState, code) = (statement state s)
     in code ++ (psStatements newState ss)


statement :: State -> Statement -> (State, String)
statement state stmt 
          = case stmt of (AssignStatement id exp)     -> assign id exp state
                         (IfStatement id ss)          -> ifThenElse id ss (Statements []) state
                         (IfElseStatement id ss1 ss2) -> ifThenElse id ss1 ss2 state
                         (ReturnStatement id)         -> returnId id state


assign :: ID -> Expression -> State -> (State, String)
assign id' exp state
    = case exp of (NumExpression n)                 -> assignNum id' exp state
                  (IDExpression id'')               -> assignId id' exp state
                  (FunctionExpression id'' args)    -> assignFunc id' exp state 
                  (OperatorExpression exp1 op exp2) -> assignOp id' exp state


---------------------------------------------


assignNum :: ID -> Expression -> State -> (State, String)
assignNum (ID id) (NumExpression (Number num)) state 
     = let r = nextRegister state in
           (updateRegisters state, " (lc r" ++ (show r) ++ " " ++ (show num) ++ ") (st " ++ id ++ " r" ++ (show r) ++ ") ")



---------------------------------------------



assignId :: ID -> Expression -> State -> (State, String)
assignId (ID id1) (IDExpression (ID id2)) state 
     = let r = nextRegister state in
           (updateRegisters state, " (ld r" ++ (show r) ++ " " ++ id2 ++ ") (st " ++ id1 ++ " r" ++ (show r) ++ ") ")


---------------------------------------------


assignFunc :: ID -> Expression -> State -> (State, String)
assignFunc (ID id) (FunctionExpression id' (Arguments args)) state 
     = let (state', str) = (bind (loadArgs args state) (callFunc id' args)) in 
           (state', str ++ " (st " ++ id ++ " r" ++ (show (prevRegister state')) ++ ") ")

loadArgs :: [ID] -> State -> (State, String)
loadArgs [] state           = (state, "")
loadArgs (arg:args) state   = bind (loadArg arg state) (loadArgs args)

loadArg :: ID -> State -> (State, String)
loadArg (ID id) state = (updateRegisters state, (" (ld r" ++ (show (nextRegister state)) ++ " " ++ id ++ ") "))

callFunc :: ID -> [ID] -> State -> (State, String)
callFunc (ID id) ids state 
      = let n = toInteger (length ids) 
            r = prevRegister state 
        in (updateRegisters state, " (call r" ++ (show (r+1)) ++ " " ++ id ++ (listRegisters (r-n+1) (r)) ++ ")")

listRegisters :: Integer -> Integer -> String
listRegisters lo hi 
    = foldr (\x -> (++) (" " ++ x)) 
            "" (unfoldr (\(l,h) 
                    -> if (l <= h) then Just (("r" ++ (show l)), (l+1,h)) else Nothing) 
               (lo,hi)) 


---------------------------------------------

--- this generates redundant (st id' <REG>) expressions.
--- work out how to remove these if time allows...
assignOp :: ID -> Expression -> State -> (State, String)
assignOp id'@(ID id) (OperatorExpression exp1 op exp2) state
  = let (state'@(Used b1 (Registers r1)), s1) = (assign id' exp1 state)
        (state''@(Used b2 (Registers r2)), s2) = (assign id' exp2 state')
    in (updateRegisters state'', 
        s1 ++ s2 ++ " (" ++ (operator op) ++ " r" ++ (show (nextRegister state'')) 
        ++ " r" ++ (show r1) ++ " r" ++ (show r2) ++ ") ")

operator :: Op -> String
operator Plus        = "add"
operator Minus       = "sub"
operator Divide      = "div"
operator Multiply    = "mul"
operator GreaterThan = "gt"
operator LessThan    = "lt"
operator Equals      = "eq"



----------------RETURN---------------------------


returnId :: ID -> State -> (State, String)
returnId (ID id) state
   = let r = (nextRegister state) 
     in (updateRegisters state, 
         "(ld r" ++ (show r) ++ " " ++ id ++ ") (ret r" ++ (show r) ++ ") ")



-----------------IF-THEN-ELSE--------------------
-- <HANGING BLOCK>   (ld nextReg id) (br r4 nextBlock nextBlock.nextBlock) )  <END BLOCK>
-- <BEGIN BLOCK> (nextBlock ( ss1 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nextBlock.nextBlock ( ss2 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nextBlock.nextBlock.nextBlock     <HANGING BLOCK>


ifThenElse :: ID -> Statements -> Statements -> State -> (State, String)
ifThenElse id ss1 ss2 state 
   = bind (bind (bind (bind (state, "") (condition id)) (branch ss1)) (branch ss2)) endIf


condition :: ID -> State -> (State, String)
condition (ID id) state 
  = let b1 = nextBlock state
        b2 = b1 + 1
        r  = nextRegister state
    in (updateRegisters state, 
        " (ld r" ++ (show r) ++ " " ++ id ++ ") (br r" ++ (show r) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ") ) ")

branch :: Statements -> State -> (State, String)
branch (Statements ss) state 
  = (updateBlocks state, 
     " (" ++ (show (nextBlock state)) ++ " " ++ (psStatements state ss) ++ ") ")

endIf :: State -> (State, String)
endIf state = (updateBlocks state, " (" ++ (show (nextBlock state)) ++ " ")
