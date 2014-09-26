{-|
  Keeps track of the number of blocks and registers used in the
  current function
-}

module State where


{-|
  "State" keeps track of the number of "Blocks" and "Registers"
  which have so far been assigned in the current block.
-}
data Blocks = Blocks Integer
  deriving Show

data Registers = Registers Integer
  deriving Show

data State = Used Blocks Registers
  deriving Show


{-|
  Returns a fresh "State" with no assigned "Blocks" or
  "Registers".
-}
newState :: State
newState = (Used (Blocks 0) (Registers 0))


{-|
  Update the current "State" to reflect the assignment of an
  additional "Block".

  Takes one argument, the old "State", and returns the updated
  "State"
-}
updtBlks :: State -> State
updtBlks (Used (Blocks b) r') = (Used (Blocks (b + 1)) r')


{-|
  Update the current "State" to reflect the assignment of
  multiple additional "Block"s.

  Takes two arguments, the old "State" and the number of "Block"s
  which have been assigned, and returns the updated "State"
-}
updtManyBlks :: Integer -> State -> State
updtManyBlks n (Used (Blocks b) r') = (Used (Blocks (b + n)) r')


{-|
  Update the current "State" to reflect the assignment of an
  additional "Register".

  Takes one argument, the old "State", and returns the updated
  "State"
-}
updtRegs :: State -> State
updtRegs (Used b' (Registers r)) = (Used b' (Registers (r + 1)))


{-|
  Update the current "State" to reflect the assignment of 
  multiple additional "Register"s.

  Takes two arguments, the old "State" and the number of
  "Register"s which have been assigned, and returns the updated
  "State"
-}
updtManyRegs :: Integer -> State -> State
updtManyRegs n (Used b' (Registers r)) = (Used b' (Registers (n + 1)))


{-|
  Get the next unnassigned "Register".

  Takes one argument, the current "State", and returns an
  "Integer" indicating the number of the next unnassigned
  "Register"
-}
nxtReg :: State -> Integer
nxtReg (Used b' (Registers r)) = r + 1


{-|
  Get the most recently assigned "Register".

  Takes one argument, the current "State", and returns an
  "Integer" indicating the number of the most recently assigned
  "Register"
-}
prvReg :: State -> Integer
prvReg (Used b' (Registers r)) = r


{-|
  Get the next unnassigned "Block".

  Takes one argument, the current "State", and returns an
  "Integer" indicating the number of the next unnassigned
  "Block"
-}
nxtBlk :: State -> Integer
nxtBlk (Used (Blocks b) r') = b + 1


{-|
  Chain functions which take a "State" and return an updated
  "State" and a "String" representing some Intermediate Code.

  Takes two arguments:
    - a "State", "String" tuple representing the "State" and
      "String" returned by a previous function.
    - a new function which takes a "State" and returns another
      "State", "String" tuple.

  And returns a "State", "String" tuple representing the result
  of chaining the output of a previous function with the provided
  function.

  This allows many functions to be used to generate Intermediate
  Code without having to explicitly keep track of the updated
  "State" between each function call.
-}
bind :: (State, String) -> (State -> (State, String)) -> (State, String)
bind (state, str) f = let (state', str') = f state 
                      in (state', (str ++ str'))
