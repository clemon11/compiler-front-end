{-|
  Produce common Intermediate Code (IC) instructions and building 
  blocks for instructions.
-}

module Instructions where


import AbstractSyntaxTree


{-|
  Produce the "String" associated with the end of an IC program.
-}
endProg :: String
endProg = " )"


{-|
  Produce the "String" associated with the end of an IC function.
-}
endFn :: String
endFn = " ) )\n"


{-|
  Produce the "String" associated with the end of an IC 
  instruction.
-}
endInstr :: String
endInstr = ")\n      "


{-|
  Produce the "String" associated with the end of an IC branch
  condition.
-}
endBrchCnd :: String
endBrchCnd = " )\n    "


{-|
  Produce the "String" associated with the beginning of an IC 
  program.
-}
bgnProg :: String
bgnProg = "( "


{-|
  Produce the "String" associated with the beginning of an IC
  function.
-}
bgnFn :: String
bgnFn = "  ("


{-|
  Produce the "String" associated with the beginning of an IC
  instruction.
-}
bgnInstr :: String
bgnInstr = "     ("


{-|
  Produce the "String" associated with the whitespace before a
  new "Block"
-}
newBlkWs :: String
newBlkWs = "     "


{-|
  Given an "Integer" produce the "String" corresponding to the
  associated "Register".
-}
reg :: Integer -> String
reg r = "r" ++ (show r)


{-|
  Given an "Integer" representing a "Register", and an "ID"
  representing a variable, produce the "String" for the
  associated IC load instruction.
-}
ldVar :: Integer -> ID -> String
ldVar r (ID id) = bgnInstr ++ "ld " ++ (reg r) 
                  ++ " " ++ id ++ endInstr


{-|
  Given an "Integer" representing a "Register", and a "Number", 
  produce the "String" for the associated IC load instruction.
-}
ldCnst :: Integer -> Number -> String
ldCnst r (Number n) = bgnInstr ++ "lc " ++ (reg r) 
                      ++ " " ++ (show n) ++ endInstr


{-|
  Given an "ID" representing a variable, and an "Integer" 
  representing a variable, produce the "String" for the
  associated IC store instruction.
-}
store :: ID -> Integer -> String
store (ID id) r = bgnInstr ++ "st " ++ id ++ " " ++ (reg r) ++ endInstr


{-|
  Produce an IC branch instruction.

  Takes three arguments:
    - an "Integer" representing the first "Block" to branch
      to (the if-block)
    - an "Integer" representing the second "Block" to branch
      to (the else-block)
    - an "Integer" representing the "Register" containing the
      contition to branch on.

  Returns a "String" containing the IC branch instruction.
-}
brn :: Integer -> Integer -> Integer -> String
brn b1 b2 r
    = bgnInstr ++ "br " ++ (reg r) ++ " " ++ (show b1)
      ++ " " ++ (show b2) ++ ")" ++ endBrchCnd
