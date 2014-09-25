module State where


---- keep track of the number of blocks and registers used in the current method ----

data UsedBlocks = Blocks Integer
  deriving Show

data UsedRegisters = Registers Integer
  deriving Show

data State = Used UsedBlocks UsedRegisters
  deriving Show

newState :: State
newState = (Used (Blocks 0) (Registers 0))

updtBlks :: State -> State
updtBlks (Used (Blocks b) r') = (Used (Blocks (b + 1)) r')

updtManyBlks :: Integer -> State -> State
updtManyBlks n (Used (Blocks b) r') = (Used (Blocks (b + n)) r')

updtRegs :: State -> State
updtRegs (Used b' (Registers r)) = (Used b' (Registers (r + 1)))

updtManyRegs :: Integer -> State -> State
updtManyRegs n (Used b' (Registers r)) = (Used b' (Registers (n + 1)))

-- next unassigned register
nxtReg :: State -> Integer
nxtReg (Used b' (Registers r)) = r + 1

-- most recently assigned register
prvReg :: State -> Integer
prvReg (Used b' (Registers r)) = r

-- next unassigned block
nxtBlk :: State -> Integer
nxtBlk (Used (Blocks b) r') = b + 1

-- I don't know why I wrote a bind function. It seemed like a good idea at the time
bind :: (State, String) -> (State -> (State, String)) -> (State, String)
bind (state, str) f = let (state', str') = f state in (state', (str ++ str'))

