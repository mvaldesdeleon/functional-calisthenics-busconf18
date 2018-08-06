{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State

data Instruction = F | B | L | R
    deriving (Eq, Show)
data Direction = N | E | S | W
    deriving (Eq, Show, Enum)
data Position = Position Integer Integer
    deriving (Eq, Show)

data WorldState = WorldState { roverPosition :: Position, roverDirection :: Direction, width :: Integer, height :: Integer, obstacles :: [Position]  }
    deriving (Eq, Show)

right :: Direction -> Direction
right W = N
right direction = succ direction

left :: Direction -> Direction
left N = W
left direction = pred direction

turnRight :: WorldState -> WorldState
turnRight worldState@WorldState { roverDirection } = worldState { roverDirection = right roverDirection }

turnLeft :: WorldState -> WorldState
turnLeft worldState@WorldState { roverDirection } = worldState { roverDirection = left roverDirection }

step :: WorldState -> Instruction -> WorldState
step worldState@(WorldState (Position roverX roverY) roverDirection _ _ _) instruction = case instruction of
    F -> case roverDirection of
        N -> worldState { roverPosition = (Position roverX (roverY + 1)) }
        S -> worldState { roverPosition = (Position roverX (roverY - 1)) }
        E -> worldState { roverPosition = (Position (roverX + 1) roverY) }
        W -> worldState { roverPosition = (Position (roverX - 1) roverY) }
    B -> case roverDirection of
        N -> worldState { roverPosition = (Position roverX (roverY - 1)) }
        S -> worldState { roverPosition = (Position roverX (roverY + 1)) }
        E -> worldState { roverPosition = (Position (roverX - 1) roverY) }
        W -> worldState { roverPosition = (Position (roverX + 1) roverY) }
    L -> turnLeft worldState
    R -> turnRight worldState

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
