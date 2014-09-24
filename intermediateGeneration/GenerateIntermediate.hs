module GenerateIntermediate where

import State
import AbstractSyntaxTree
import Data.Either
import Data.List -- unfoldr
import Data.Char (isSpace) -- for stripping leading whitespace



--------Convert AST to String representing Intermediate Code----------


generateIntermediate :: Program -> String
generateIntermediate (Program fs) 
   = beginProg ++ (trimAll 
                (foldr (\(Function (ID id) a v (Statements ss)) 
                            -> (++) (beginFunc ++ id ++ (arguments a) ++ "\n    (0" ++ newBlockWhitespace 
                                     ++ (trimAll (snd (statements ss (Used (Blocks 0) (Registers 0))))) ++ endFunc )) 
                       "" fs)) ++ endProg


arguments :: Arguments -> String
arguments (Arguments as) = "(" ++ dropWhile isSpace (foldr (\(ID a) -> (++) (" " ++ a)) "" as) ++ ")"


statements :: [Statement] -> State -> (State, String)
statements [] state     = (state, "")
statements (s:ss) state = bind (statement state s) (statements ss)


statement :: State -> Statement -> (State, String)
statement state stmt 
          = case stmt of (AssignStatement id exp)     -> assign id exp state
                         (IfStatement id ss)          -> ifThenElse id ss (Statements []) state
                         (IfElseStatement id ss1 ss2) -> ifThenElse id ss1 ss2 state
                         (ReturnStatement id)         -> returnId id state


------------------ASSIGNMENT-------------------------


assign :: ID -> Expression -> State -> (State, String)
assign id' exp state
   = let (state', code) = (processExpression exp state)
     in (state', code ++ (storeInVar id' (prevRegister state')))



----------------RETURN---------------------------


returnId :: ID -> State -> (State, String)
returnId id' state
   = let r = (nextRegister state) 
     in (updateRegisters state, ((loadVarIntoReg r id') ++ beginInstr ++ "ret " ++ (register r) ++ endInstr))



-----------------IF-THEN-ELSE--------------------
-- <HANGING BLOCK>   (ld nextReg id) (br r4 nextBlock nextBlock.nextBlock) )  <END BLOCK>
-- <BEGIN BLOCK> (nextBlock ( ss1 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nextBlock.nextBlock ( ss2 ) ) <END BLOCK>
-- <BEGIN BLOCK> (nextBlock.nextBlock.nextBlock     <HANGING BLOCK>


ifThenElse :: ID -> Statements -> Statements -> State -> (State, String)
ifThenElse id ss1 ss2 state
   = let b1                  = nextBlock state
         (state', ifcode)    = branch ss1 state
         b2                  = nextBlock state'
         (state'', elsecode) = branch ss2 state'
         (state''', end)     = branchOut state''
     in bind (bind (condition b1 b2 id state''') 
             (\s -> (s, (ifcode ++ end ++ elsecode ++ end)))) endIf

 
--   = let (state', ifcode) = (bind (bind (state, "") (condition id)) (branch ss1))
--         (state'', elsecode) = (branch ss2 state')
--         (state''', endBranch) = branchOut state''
--    in bind (state''', (ifcode ++ endBranch ++ elsecode ++ endBranch)) 
--       endIf


condition :: Integer -> Integer -> ID -> State -> (State, String)
condition b1 b2 id' state 
    = let r  = nextRegister state
    in (updateRegisters state,
        (loadVarIntoReg r id') ++ beginInstr ++ "br " ++ (register r) ++ " " ++ (show b1) ++ " " ++ (show b2) ++ ")" ++ endBranchCond)


branch :: Statements -> State -> (State, String)
branch (Statements ss) state
  = bind (updateBlocks state, "(" ++ (show (nextBlock state)) ++ newBlockWhitespace) 
         (statements ss)

--(updateBlocks state, 
--     "(" ++ (show (nextBlock state)) ++ newBlockWhitespace ++ (trimLeadingWhitespace (statements state ss)))


endIf :: State -> (State, String)
endIf state = (updateBlocks state, "(" ++ (show (nextBlock state)))


branchOut :: State -> (State, String)
branchOut state 
   = let zero = NumExpression (Number 0)
         b = nextBlock state
   in bind (processExpression (OperatorExpression zero Equals zero) state) (\s -> (s, beginInstr ++ "br " ++ (register (prevRegister s)) ++ " " ++ (show b) ++ " " ++ (show b) ++ ")" ++ endBranchCond))



---- Expression processing  -----


processExpression :: Expression -> State -> (State, String)

processExpression (NumExpression n') state 
   = (updateRegisters state, loadConstIntoReg (nextRegister state) n')

processExpression (IDExpression id') state 
   = (updateRegisters state, loadVarIntoReg (nextRegister state) id')

processExpression (FunctionExpression id' (Arguments args)) state 
   = bind (loadArgs args state) (callFunc id' args)

processExpression (OperatorExpression exp1 op exp2) state 
  = let (state'@(Used b1 (Registers r1)), s1) = (processExpression exp1 state)
        (state''@(Used b2 (Registers r2)), s2) = (processExpression exp2 state')
    in (updateRegisters state'', 
        s1 ++ s2 ++ beginInstr ++ (operator op) ++ " " ++ (register (nextRegister state''))
        ++ " " ++ (register r1) ++ " " ++ (register r2) ++ endInstr)


loadArgs :: [ID] -> State -> (State, String)
loadArgs [] state           = (state, "")
loadArgs (arg:args) state   = bind (loadArg arg state) (loadArgs args)


loadArg :: ID -> State -> (State, String)
loadArg id' state = (updateRegisters state, (loadVarIntoReg (nextRegister state) id'))


callFunc :: ID -> [ID] -> State -> (State, String)
callFunc (ID id) ids state 
      = let n = toInteger (length ids) 
            r = prevRegister state 
        in (updateRegisters state, beginInstr ++ "call " ++ (register (r+1)) ++ " " ++ id ++ (listRegisters (r-n+1) (r)) ++ endInstr)


listRegisters :: Integer -> Integer -> String
listRegisters lo hi 
    = foldr (\x -> (++) (" " ++ x)) 
            "" (unfoldr (\(l,h) 
                    -> if (l <= h) then Just (("r" ++ (show l)), (l+1,h)) else Nothing) 
               (lo,hi)) 


operator :: Op -> String
operator Plus        = "add"
operator Minus       = "sub"
operator Divide      = "div"
operator Multiply    = "mul"
operator GreaterThan = "gt"
operator LessThan    = "lt"
operator Equals      = "eq"


----------------------common instructions-------------------------


endProg :: String
endProg = " )"


endFunc :: String
endFunc = " ) )\n" 


endInstr :: String
endInstr = ")\n      "


endBranchCond :: String
endBranchCond = " )\n    "


beginProg :: String
beginProg = "( "


beginFunc :: String
beginFunc = "  ("


beginInstr :: String
beginInstr = "     ("


newBlockWhitespace :: String
newBlockWhitespace = "     "


register :: Integer -> String
register r = "r" ++ (show r)


loadVarIntoReg :: Integer -> ID -> String
loadVarIntoReg r (ID id) = beginInstr ++ "ld " ++ (register r) ++ " " ++ id ++ endInstr


loadConstIntoReg :: Integer -> Number -> String
loadConstIntoReg r (Number n) = beginInstr ++ "lc " ++ (register r) ++ " " ++ (show n) ++ endInstr


storeInVar :: ID -> Integer -> String
storeInVar (ID id) r = beginInstr ++ "st " ++ id ++ " " ++ (register r) ++ endInstr


----other convenience methods-----
trimNewline :: String -> String
trimNewline s = (reverse . dropWhile (=='\n') . reverse) ((reverse . dropWhile (==' ') . reverse) s)


trimLeadingWhitespace :: String -> String
trimLeadingWhitespace = dropWhile (==' ')


trimAll :: String -> String
trimAll s = trimNewline (trimLeadingWhitespace s)
