{-|
  Converts an Abstract Syntax Tree (AST) (i.e. a Program node) to
  a String representing the Intermediate Code (IC).  
-}

module GenerateIntermediate where


import State
import ExpressionProcessing
import Instructions
import StringProcessing
import AbstractSyntaxTree
import Data.List


{-|
  Generates Intermediate Code from an AST.

  Takes one argument, of type "Program" which represents the
  Abstract Syntax Tree, and returns a String representing the IC.
-}
generateIntermediate :: Program -> String
generateIntermediate (Program fs) 
   = bgnProg ++ (trim (foldr (\f -> (++) (fn f)) "" fs)) 
     ++ endProg


{-|
  Processes a "Function" to produce the corresponding IC.

  Takes one argument, of type "Function", and returns a String
  representing the corresponding IC.

  Functions in IC consist of an ID (function name), a set of
  arguments, and at least one block which may contain some IC
  instructions. The "Statements" contained in the Function are
  processes using a new "State" (that is, with the number of
  already assigned "Blocks" and "Registers" set to 0).
-}
fn :: Function -> String
fn (Function (ID id) a v (Statements ss)) 
  = (bgnFn ++ id ++ (args a) ++ "\n    (0" ++ newBlkWs 
    ++ (trim (snd (stmts ss newState))) ++ endFn)


{-|
  Processes the "Arguments" of a "Function".

  Takes one argument, of type "Arguments", and returns a String
  consisting of a list of IDs (variable names) separated by
  spaces.
-}
args :: Arguments -> String
args (Arguments as) 
  = "(" ++ trimWs (foldr (\(ID a) -> (++) (" " ++ a)) "" as)
     ++ ")"


{-|
  Processes a list of "Statement"s.

  Takes two arguments:
    - a "Statement" list
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple of:
    - the new "State", with any registers or blocks used in the
      processing of the statements updated accordingly
    - a "String" representing the IC produced be processing the
      statements given the provided "State".

  Statements are processed sequentially, with the new "State"
  resulting from processing each "Statement" being used in the
  processing of the next "Statement".
-}
stmts :: [Statement] -> State -> (State, String)
stmts [] state     = (state, "")
stmts (s:ss) state = bind (stmt s state) (stmts ss)


{-|
  Process a single "Statement".

  Takes two arguments:
    - a "Statement" consisting of either an assignment,
      if-then/if-then-else, or a return.
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.

  The "Statement" is processed according to its type.
-}
stmt :: Statement -> State -> (State, String)
stmt s state 
   = case s of (AssignStatement id exp)     
                     -> assign id exp state
               (IfStatement id ss)          
                     -> ifThenElse id ss (Statements []) state
               (IfElseStatement id ss1 ss2) 
                     -> ifThenElse id ss1 ss2 state
               (ReturnStatement id)
                     -> retn id state


----------------------------------------------------------------
-- ASSIGNMENT STATEMENT
----------------------------------------------------------------

{-|
  Process the assignment of an expression to a variable.

  Takes three arguments:
    - an "ID" representing a variable.
    - an "Expression".
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.

  The expression will be evaluated according to its type and
  then assigned to the variable.
-}
assign :: ID -> Expression -> State -> (State, String)
assign id' exp state
   = let (state', code) = (expr exp state)
     in (state', code ++ (store id' (prvReg state')))


----------------------------------------------------------------
-- IF-THEN-ELSE STATEMENT
----------------------------------------------------------------


{-|
  Process an if-then-else statement.

  Takes four arguments:
    - an "ID" representing the variable to branch on.
    - some "Statements" to execute if the condition in the
      variable is true.
    - some "Statements" to execute if the condition in the
      variable is false.
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.

  At the end of both the if-branch and the else-branch, we
  branch to a block containing any "Statements" which occur
  after the if-then-else. If there are no more "Statements",
  this block will be empty.

  A separate function for if-then statements is not provided.
  Instead this function is used with an empty second set of
  "Statements".
-}
ifThenElse :: ID -> Statements -> Statements -> State -> (State, String)
ifThenElse id ss1 ss2 state
   = let b1 = nxtBlk state
         (state', ifbranch)    
            = bind (condStmts ss1 state) (branchOut 1)
         b2 = nxtBlk state'
         (state'', elsebranch) 
            = bind (condStmts ss2 state') (branchOut 0)
     in bind (bind (cond b1 b2 id state'') 
                   (\s -> (s, (ifbranch ++ elsebranch))))
        endIfThenElse


{-|
  Process the conditional part of an if-then-else statement.
  That is, produce the register to branch on and the branch
  instruction itself.
 
  Takes four arguments:
    - an "Integer" indicating the block to branch to in the
      "if" case.
    - an "Integer" indicating the block to branch to in the
      "else" case.
    - an "ID" indicating the variable containing the conditional
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
cond :: Integer -> Integer -> ID -> State -> (State, String)
cond b1 b2 id' state 
    = let r = nxtReg state
    in bind (updtRegs state, (ldVar r id')) (\s -> (s, brn b1 b2 r))


{-|
  Process the "Statements" to be executed in a branch of an
  if-then-else statement.

  Takes two arguments:
    - the "Statements" to execute in this branch
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
condStmts :: Statements -> State -> (State, String)
condStmts (Statements ss) state
  = bind (updtBlks state, 
          "(" ++ (show (nxtBlk state)) ++ newBlkWs) (stmts ss)


{-|
  At the end of the if-statements, and at the end of the 
  else-statements, branch to the the block after the if-then-else
  section.

  Takes two arguments:
    - an "Integer" indicating how many blocks the branch should
      skip over: 1 for an if-section, 0 for an else section
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
branchOut :: Integer -> State -> (State, String)
branchOut skip state 
   = let zero = NumExpression (Number 0)
         r = nxtReg state
         b = (nxtBlk state) + skip
   in bind (expr (OperatorExpression zero Equals zero) state) 
           (\s -> (s, brn b b r))


{-|
  End an if-then-else section by getting a new block ready.

  Takes one argument:
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
endIfThenElse :: State -> (State, String)
endIfThenElse state = (updtBlks state, "(" ++ (show (nxtBlk state)))


----------------------------------------------------------------
-- RETURN STATEMENT
----------------------------------------------------------------


{-|
  Process a return statement.

  Takes two arguments:
    - an "ID" representing the variable to return.
    - a "State", representing the number of registers and blocks
      used in the current function so far.

  Returns a tuple containing the updated "State" and the generated
  "String" of IC.
-}
retn :: ID -> State -> (State, String)
retn id' state
   = let r = (nxtReg state) 
     in (updtRegs state, ((ldVar r id') ++ bgnInstr ++ "ret " 
                         ++ (reg r) ++ endInstr))
