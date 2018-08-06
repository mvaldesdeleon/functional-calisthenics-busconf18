import Control.Monad.State

data Instruction = F | B | L | R
    deriving (Eq, Show)
data Direction = N | E | S | W
    deriving (Eq, Show, Enum)
data Position = Position Integer Integer
    deriving (Eq, Show)

data WorldState = WorldState { roverPosition :: Position, roverDirection :: Direction, width :: Integer, height :: Integer, obstacles :: [Position]  }
    deriving (Eq, Show)

turnRight :: Direction -> Direction
turnRight W = N
turnRight direction = succ direction

turnLeft :: Direction -> Direction
turnLeft N = W
turnLeft direction = pred direction

step :: WorldState -> Instruction -> WorldState
step (WorldState (Position roverX roverY) roverDirection width height obstacles) instruction = case instruction of
    F -> case roverDirection of
        N -> WorldState (Position roverX (roverY + 1)) roverDirection width height obstacles
        S -> WorldState (Position roverX (roverY - 1)) roverDirection width height obstacles
        E -> WorldState (Position (roverX + 1) roverY) roverDirection width height obstacles
        W -> WorldState (Position (roverX - 1) roverY) roverDirection width height obstacles
    B -> case roverDirection of
        N -> WorldState (Position roverX (roverY - 1)) roverDirection width height obstacles
        S -> WorldState (Position roverX (roverY + 1)) roverDirection width height obstacles
        E -> WorldState (Position (roverX - 1) roverY) roverDirection width height obstacles
        W -> WorldState (Position (roverX + 1) roverY) roverDirection width height obstacles
    L -> WorldState (Position roverX roverY) (turnLeft roverDirection) width height obstacles
    R -> WorldState (Position roverX roverY) (turnRight roverDirection) width height obstacles

initialPosition :: Position
initialPosition = Position 10 10

initialState :: WorldState
initialState = WorldState initialPosition N 50 50 []

instructions :: [Instruction]
instructions = [F, F, F, L, F, F, F, R, R, B, B, R, B, F, F, F]

processInstruction :: Instruction -> State WorldState ()
processInstruction = modify . flip step
-- processInstruction instruction = do
--     oldState <- get
--     put $ step oldState instruction

evalInstructions :: [Instruction] -> WorldState -> WorldState
evalInstructions instructions initialState = execState rover initialState
    where rover = forM_ instructions processInstruction

finalState :: WorldState
finalState = evalInstructions instructions initialState

evalInstructions' :: [Instruction] -> WorldState -> WorldState
evalInstructions' instructions initialState = foldl step initialState instructions

finalState' :: WorldState
finalState' = evalInstructions' instructions initialState
