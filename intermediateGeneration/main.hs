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
         
         case generateCode programText of 
              Left error -> putStrLn error
              Right code -> writeFile fileOut code


generateCode :: String -> Either String String   
generateCode text = case checkSemantics (parseProgram text) of 
               Left error -> Left error 
               Right ast -> Right (generateIntermediate ast)
               
 