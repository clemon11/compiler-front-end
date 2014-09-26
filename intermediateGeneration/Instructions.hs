module Instructions where

import AbstractSyntaxTree

-- Common instructions and building blocks for instructions


endProg :: String
endProg = " )"


endFn :: String
endFn = " ) )\n"


endInstr :: String
endInstr = ")\n      "


endBrchCnd :: String
endBrchCnd = " )\n    "


bgnProg :: String
bgnProg = "( "


bgnFn :: String
bgnFn = "  ("


bgnInstr :: String
bgnInstr = "     ("

newBlkWs :: String
newBlkWs = "     "


reg :: Integer -> String
reg r = "r" ++ (show r)


ldVar :: Integer -> ID -> String
ldVar r (ID id) = bgnInstr ++ "ld " ++ (reg r) ++ " " ++ id ++ endInstr


ldCnst :: Integer -> Number -> String
ldCnst r (Number n) = bgnInstr ++ "lc " ++ (reg r) ++ " " ++ (show n) ++ endInstr


store :: ID -> Integer -> String
store (ID id) r = bgnInstr ++ "st " ++ id ++ " " ++ (reg r) ++ endInstr

