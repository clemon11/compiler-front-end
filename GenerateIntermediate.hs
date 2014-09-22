module GenerateIntermediate where

import AbstractSyntaxTree
import Data.Either
import Data.List -- unfoldr
import Data.Char (isSpace) -- for stripping leading whitespace



---- keep track of the number of blocks and registers used in the current method ----

data UsedBlocks = Blocks Integer
  deriving Show

data UsedRegisters = Registers Integer
  deriving Show

data State = Used UsedBlocks UsedRegisters
  deriving Show

updateBlocks :: State -> State
updateBlocks (Used (Blocks b) r') = (Used (Blocks (b + 1)) r') -- TODO tidy this up

updateRegisters :: State -> State
updateRegisters (Used b' (Registers r)) = (Used b' (Registers (r + 1)))

-- next unassigned register
nextRegister :: State -> Integer
nextRegister (Used b' (Registers r)) = r + 1

-- most recently assigned register
prevRegister :: State -> Integer
prevRegister (Used b' (Registers r)) = r

-- I don't know why I wrote a bind function. It seemed like a good idea at the time
bind :: (State, String) -> (State -> (State, String)) -> (State, String)
bind (state, str) f = let (state', str') = f state in (state', (str ++ str'))

------- Convert AST to String representing Intermediate Code ---------

generateIntermediate :: Program -> String
generateIntermediate ast = "( " ++ ( functions ast ) ++ ")"

functions :: Program -> String
functions (Program fs) 
   = foldr (\(Function (ID id) a v s) 
                -> (++) ("( " ++ id ++ " (" ++ (arguments a) ++ ") " ++ (statements s) ++ " ) " ))
           "" fs 

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
          = case stmt of (AssignStatement id exp) -> assign id exp state
                         _                        -> (state, "Unimplemented")

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

