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

advance :: Integer -> Integer -> Integer
advance max value = if value == max - 1 then 0 else value + 1

retreat :: Integer -> Integer -> Integer
retreat max value = if value == 0 then max else value - 1

goUp :: WorldState -> WorldState
goUp worldState@WorldState { roverPosition = Position roverX roverY, height } = worldState { roverPosition = Position roverX (advance height roverY) }

goDown :: WorldState -> WorldState
goDown worldState@WorldState { roverPosition = Position roverX roverY, height } = worldState { roverPosition = Position roverX (retreat height roverY) }

goRight :: WorldState -> WorldState
goRight worldState@WorldState { roverPosition = Position roverX roverY, width } = worldState { roverPosition = Position (advance width roverX) roverY }

goLeft :: WorldState -> WorldState
goLeft worldState@WorldState { roverPosition = Position roverX roverY, width } = worldState { roverPosition = Position (retreat width roverX) roverY }

step :: WorldState -> Instruction -> WorldState
step worldState@(WorldState (Position roverX roverY) roverDirection _ _ _) instruction = case instruction of
    F -> case roverDirection of
        N -> goUp worldState
        S -> goDown worldState
        E -> goRight worldState
        W -> goLeft worldState
    B -> case roverDirection of
        N -> goDown worldState
        S -> goUp worldState
        E -> goLeft worldState
        W -> goRight worldState
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
