module State where


---- keep track of the number of blocks and registers used in the current method ----

data UsedBlocks = Blocks Integer
  deriving Show

data UsedRegisters = Registers Integer
  deriving Show

data State = Used UsedBlocks UsedRegisters
  deriving Show

updateBlocks :: State -> State
updateBlocks (Used (Blocks b) r') = (Used (Blocks (b + 1)) r')

updateManyBlocks :: Integer -> State -> State
updateManyBlocks n (Used (Blocks b) r') = (Used (Blocks (b + n)) r')

updateRegisters :: State -> State
updateRegisters (Used b' (Registers r)) = (Used b' (Registers (r + 1)))

updateManyRegisters :: Integer -> State -> State
updateManyRegisters n (Used b' (Registers r)) = (Used b' (Registers (n + 1)))

-- next unassigned register
nextRegister :: State -> Integer
nextRegister (Used b' (Registers r)) = r + 1

-- most recently assigned register
prevRegister :: State -> Integer
prevRegister (Used b' (Registers r)) = r

-- next unassigned block
nextBlock :: State -> Integer
nextBlock (Used (Blocks b) r') = b + 1

-- I don't know why I wrote a bind function. It seemed like a good idea at the time
bind :: (State, String) -> (State -> (State, String)) -> (State, String)
bind (state, str) f = let (state', str') = f state in (state', (str ++ str'))

