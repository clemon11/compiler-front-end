import TheParser
import AbstractSyntaxTree
import Semantics
import System.Environment
import GenerateIntermediate

--main :: IO ()
main = do
         [fileIn, fileOut] <- getArgs
         programText <- readFile fileIn 
         
         --Runs the parser and semantics check, then if they succeed can run the intermediate code generator
         putStrLn (generateCode programText)
         


generateCode :: String -> String    
generateCode text = case checkSemantics (parseProgram text) of 
               Left error -> show error 
               Right ast -> generateIntermediate ast
               
