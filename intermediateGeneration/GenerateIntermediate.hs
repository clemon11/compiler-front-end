module GenerateIntermediate where

import State
import ExpressionProcessing
import Instructions
import StringProcessing
import AbstractSyntaxTree
import Data.List


--------Convert AST to String representing Intermediate Code----------


generateIntermediate :: Program -> String
generateIntermediate (Program fs) 
   = bgnProg ++ (trim (foldr (\f -> (++) (fn f)) "" fs)) ++ endProg


fn :: Function -> String
fn (Function (ID id) a v (Statements ss)) 
  = (bgnFn ++ id ++ (args a) ++ "\n    (0" ++ newBlkWs ++ (trim (snd (stmts ss newState))) ++ endFn)


args :: Arguments -> String
args (Arguments as) = "(" ++ trimWs (foldr (\(ID a) -> (++) (" " ++ a)) "" as) ++ ")"


stmts :: [Statement] -> State -> (State, String)
stmts [] state     = (state, "")
stmts (s:ss) state = bind (stmt state s) (stmts ss)


stmt :: State -> Statement -> (State, String)
stmt state stmt 
   = case stmt of (AssignStatement id exp)     -> assign id exp state
                  (IfStatement id ss)          -> ifThenElse id ss (Statements []) state
                  (IfElseStatement id ss1 ss2) -> ifThenElse id ss1 ss2 state
                  (ReturnStatement id)         -> retn id state


------------------ASSIGNMENT-------------------------


assign :: ID -> Expression -> State -> (State, String)
assign id' exp state
   = let (state', code) = (expr exp state)
     in (state', code ++ (store id' (prvReg state')))


-----------------IF-THEN-ELSE--------------------
-- <HANGING BLOCK>   (ld nxtReg id) (br r4 nxtBlk nxtBlock.nxtBlock) )  <END BLOCK>
-- <BEGIN BLOCK> (nxtBlk ( ss1 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nxtBlock.nxtBlk ( ss2 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nxtBlock.nxtBlock.nxtBlk     <HANGING BLOCK>


ifThenElse :: ID -> Statements -> Statements -> State -> (State, String)
ifThenElse id ss1 ss2 state
   = let b1                    = nxtBlk state
         (state', ifbranch)    = bind (branch ss1 state) (branchOut 1)
         b2                    = nxtBlk state'
         (state'', elsebranch) = bind (branch ss2 state') (branchOut 0)
     in bind (bind (cond b1 b2 id state'') (\s -> (s, (ifbranch ++ elsebranch)))) endIf


condOnReg :: Integer -> Integer -> Integer -> State -> (State, String)
condOnReg b1 b2 r state
    = (state, bgnInstr ++ "br " ++ (reg r) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ")" ++ endBrchCnd)


cond :: Integer -> Integer -> ID -> State -> (State, String)
cond b1 b2 id' state 
    = let r = nxtReg state
    in bind (updtRegs state, (ldVar r id')) (condOnReg b1 b2 r)


branch :: Statements -> State -> (State, String)
branch (Statements ss) state
  = bind (updtBlks state, "(" ++ (show (nxtBlk state)) ++ newBlkWs) 
         (stmts ss)


endIf :: State -> (State, String)
endIf state = (updtBlks state, "(" ++ (show (nxtBlk state)))


branchOut :: Integer -> State -> (State, String)
branchOut n state 
   = let zero = NumExpression (Number 0)
         r = nxtReg state
         b = (nxtBlk state) + n
   in bind (expr (OperatorExpression zero Equals zero) state) 
           (condOnReg b b r)


----------------RETURN---------------------------


retn :: ID -> State -> (State, String)
retn id' state
   = let r = (nxtReg state) 
     in (updtRegs state, ((ldVar r id') ++ bgnInstr ++ "ret " ++ (reg r) ++ endInstr))
