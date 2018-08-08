{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.State
import Control.Applicative

data Instruction = F | B | L | R
    deriving (Eq, Show)
data Direction = N | E | S | W
    deriving (Eq, Show, Enum)
data Position = Position Integer Integer
    deriving (Eq, Show)

data WorldState = WorldState { roverPosition :: Position, roverDirection :: Direction, width :: Integer, height :: Integer, obstacles :: [Position] }
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
step worldState@WorldState { roverDirection } instruction = case instruction of
    F -> checkObstacles $ case roverDirection of
        N -> goUp worldState
        S -> goDown worldState
        E -> goRight worldState
        W -> goLeft worldState
    B -> checkObstacles $ case roverDirection of
        N -> goDown worldState
        S -> goUp worldState
        E -> goLeft worldState
        W -> goRight worldState
    L -> turnLeft worldState
    R -> turnRight worldState
  where
    -- After moving, check if the current position matches an obstacle position. If so, rollback to the original state.
    checkObstacles newWorldState@WorldState { roverPosition, obstacles } = if elem roverPosition obstacles then worldState else newWorldState

initialPosition :: Position
initialPosition = Position 10 10

initialState :: WorldState
initialState = WorldState initialPosition N 50 50 []

instructions :: [Instruction]
instructions = [F, F, F, L, F, F, F, R, R, B, B, R, B, F, F, F]

processInstruction :: Instruction -> State WorldState Bool
processInstruction instruction = do
    oldState <- get
    let newState = step oldState instruction
    put $ step oldState instruction
    return $ if oldState == newState then False else True

untilS :: (b -> Bool) -> [a] -> (a -> State s b) -> State s (Maybe b)
untilS _ [] _ = return Nothing
untilS pr (a:as) f = state $ newState
    where
        newState s =
            if (pr b) then (Just b, ns) else (mb <|> Just b, fs)
            where
                next = f a
                (b, ns) = runState next s
                tailState = untilS pr as f
                (mb, fs) = runState tailState ns

evalInstructions :: [Instruction] -> WorldState -> WorldState
evalInstructions instructions initialState = execState rover initialState
    -- We now need an enhanced version of forM that will break if the Bool is false
    where rover = forM_ instructions processInstruction

finalState :: WorldState
finalState = evalInstructions instructions initialState

evalInstructions' :: [Instruction] -> WorldState -> WorldState
evalInstructions' instructions initialState = foldl step initialState instructions

finalState' :: WorldState
finalState' = evalInstructions' instructions initialState
