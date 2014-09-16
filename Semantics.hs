module Semantics where

import AbstractSyntaxTree
import Data.List
import Data.Maybe (fromJust)

checkSemantics :: Either a Program -> Either String Program
checkSemantics p = case p of
                 Left error -> Left "Syntax Error."
                 Right ast -> treeSemantics ast
        
treeSemantics :: Program -> Either String Program
treeSemantics p = if null errors then
                  Right p
                  else 
                  Left (unlines errors)
  where funcsUndefinedErrors = map (\s -> "Error: function '" ++ s ++ "' undefined.") (funcsUndefined p)
        funcsDuplicatedErrors = map (\s -> "Error: function '" ++ s ++ "' redefined.") (funcsDuplicated p)
        checkArgNumsMatchErrors = map (\(s, n) -> "Error: function '" ++ s ++ "' expects " ++ show n ++ " argument(s).") (checkArgNumsMatch p)
        checkUndefinedVarsErrors = map (\s -> "Error: variable '" ++ s ++ "' undefined") (checkUndefinedVars p)
        checkRedefinedVarsErrors = map (\s -> "Error: variable '" ++ s ++ "' redefined") (checkRedefinedVars p)
        checkMainFuncErrors = if not (checkMainFunc p) then
                              ["Error: No main function defined."]
                              else []        
        errors = funcsUndefinedErrors ++ funcsDuplicatedErrors ++ checkArgNumsMatchErrors ++ checkUndefinedVarsErrors ++ checkRedefinedVarsErrors ++ checkMainFuncErrors
        
        
        
        
        ---------------------------------------------------------------------------
--Top level functions

--Checks to see if all called functions are defined, if functions are undefined returns names, other wise empty list
funcsUndefined :: Program -> [String]
funcsUndefined p = filter (\x ->( not (elem x (nub (getDefinedFuncNames p)) )))  (getCalledFuncNames p) 
        
--Checks if 2 functions have the same name
funcsDuplicated :: Program -> [String]
funcsDuplicated p = getDefinedFuncNames p \\ nub (getDefinedFuncNames p)

--Checks if it has a main function
checkMainFunc :: Program -> Bool
checkMainFunc p = elem "main" (getDefinedFuncNames p)

--Checks to see if any functions are called with the incorrect number of arguments
checkArgNumsMatch :: Program -> [(String, Int)]
checkArgNumsMatch p = getFuncCallsWrongArgs (recurseProgramFuncCall p) (getDefinedFuncNamesArgs p)

--Checks for undefined variables
checkUndefinedVars :: Program -> [String]
checkUndefinedVars (Program fs) = concat(map funcCheckUsedVars fs)

--Checks to see if there are 2 variables or arguments with the same name for any function
checkRedefinedVars :: Program -> [String]
checkRedefinedVars (Program fs) = concat(map funcRedefinedVars fs)
------------------------------------------------------------------------------        
--Gets the defined function names and argument numbers
getDefinedFuncNamesArgs :: Program -> [(String, Int)]
getDefinedFuncNamesArgs (Program fs) = map getDefinedFuncNameArg fs

getDefinedFuncNameArg :: Function -> (String, Int)
getDefinedFuncNameArg (Function (ID ident) (Arguments args) var state) = (ident, (length args))

getDefinedFuncNames :: Program -> [String]
getDefinedFuncNames p = map fst (getDefinedFuncNamesArgs p)
-----------------------------------------------------------------------------
--Gets the called list of functions and how many arguments are passed to them

--takes in the list of function calls and their arguments (where the functions were defined) and the list of expected function calls and arguments
--removes functions with undefined names
--removes all correct calls
--gets names of all wrong calls
--gets corresponding correct argument number for each wrong function call and returns
getFuncCallsWrongArgs :: [(String, Int)] -> [(String, Int)] ->  [(String, Int)]
getFuncCallsWrongArgs calls definitions = zip invalidArgsFuncNames (map (\x -> fromJust (lookup x definitions)) invalidArgsFuncNames)
  where invalidArgsCalls = listDifference (validNamesCalls calls definitions) definitions
        invalidArgsFuncNames = map fst invalidArgsCalls

--removes alls of undefined functions from the list of invalid calls
validNamesCalls :: [(String, Int)] -> [(String, Int)] -> [(String, Int)]
validNamesCalls calls definitions = filter (\(x, y) -> x `elem` (map fst definitions)) calls

--Gets a list of all the called function names
getCalledFuncNames :: Program -> [String]
getCalledFuncNames p = map (\(x,y) -> x) (recurseProgramFuncCall p)

--Traversing the tree to find the function calls and num args
recurseProgramFuncCall :: Program -> [(String, Int)]
recurseProgramFuncCall (Program fs) = concat (map recurseFunctionFuncCall fs)

recurseFunctionFuncCall :: Function -> [(String, Int)]
recurseFunctionFuncCall (Function ident args vars states) = recurseStatementsFuncCall states

recurseStatementsFuncCall :: Statements -> [(String, Int)]
recurseStatementsFuncCall (Statements states) = concat (map recurseStatementFuncCall states)

recurseStatementFuncCall :: Statement -> [(String, Int)]
recurseStatementFuncCall (AssignStatement id ex)= recurseExpressionFuncCall ex
recurseStatementFuncCall (IfElseStatement id states1 states2) = (recurseStatementsFuncCall states1) ++ (recurseStatementsFuncCall states2)
recurseStatementFuncCall (IfStatement id states) =  recurseStatementsFuncCall states
recurseStatementFuncCall (ReturnStatement id) = [] 

recurseExpressionFuncCall :: Expression -> [(String, Int)]
recurseExpressionFuncCall (OperatorExpression ex1 op ex2) = recurseExpressionFuncCall ex1 ++ recurseExpressionFuncCall ex2
recurseExpressionFuncCall (FunctionExpression (ID id) (Arguments args)) = [(id, length args)] 
recurseExpressionFuncCall  otherEx = []

---------------------------------------------------------------------------------------------------------------
--Gets declared variables and arguments for each function
getFuncVars :: Function -> [String]
getFuncVars (Function (ID id) (Arguments args) (Variables v) (Statements s)) = map getIDName v

--getVarNames :: Variables -> [String]
--getVarNames (Variables ids) = map getIDName ids

getFuncArgs :: Function -> [String]
getFuncArgs (Function (ID id) (Arguments args) (Variables v) (Statements s)) = map getIDName args

--getArgNames :: Arguments -> [String]
--getArgNames (Arguments ids) = map getIDName ids
------------------------------------------------------------------------------------------------------------------
--Gets all vars and args used in function (assigned to or used)
getFuncUsedVars :: Function -> [String]
getFuncUsedVars (Function id arg var (Statements states)) = nub(getStatementsUsedVars states)

getStatementsUsedVars :: [Statement] -> [String]
getStatementsUsedVars states = concat (map getStatementUsedVars states)

getStatementUsedVars :: Statement -> [String]
getStatementUsedVars (AssignStatement (ID id) ex) = [id] ++ getExpressionUsedVars ex
getStatementUsedVars (IfElseStatement (ID id) (Statements states1) (Statements states2)) = [id] ++ (getStatementsUsedVars states1) ++ (getStatementsUsedVars states2)
getStatementUsedVars (IfStatement (ID id) (Statements states)) = [id] ++ (getStatementsUsedVars states)
getStatementUsedVars (ReturnStatement (ID id)) = [id]

getExpressionUsedVars :: Expression -> [String]
getExpressionUsedVars (FunctionExpression (ID funcId) (Arguments args)) = map getIDName args
getExpressionUsedVars (OperatorExpression ex1 op ex2) = getExpressionUsedVars ex1 ++ getExpressionUsedVars ex2
getExpressionUsedVars (IDExpression (ID id)) = [id]
getExpressionUsedVars num = []

getDuplicateNames :: [String] -> [String]
getDuplicateNames names = nub (deleteFirstsBy (==) names (nub names))

getIDName :: ID -> String
getIDName (ID name) = name
-------------------------------------------------------------------------------
--Compares defined variables and args and used variables for each function
funcCheckUsedVars :: Function -> [String]
funcCheckUsedVars f = listDifference (getFuncUsedVars f) (nub (getFuncArgs f ++ getFuncVars f))

funcRedefinedVars :: Function -> [String]
funcRedefinedVars f = (getFuncArgs f ++ getFuncVars f) \\ (nub(getFuncArgs f ++ getFuncVars f))
-------------------------------------------------------------------------------------------------------------------------------
--Helper Functions

--Finds the union - the intersection = difference
listDifference :: Eq a => [a] -> [a] -> [a]
listDifference calls expected = filter (\x -> not (x `elem` expected)) calls