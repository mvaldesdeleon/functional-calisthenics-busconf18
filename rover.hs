data Instruction = F | B | L | R
    deriving (Eq, Show)
data Direction = N | S | E | W
    deriving (Eq, Show)
data Position = Position Integer Integer
    deriving (Eq, Show)

data WorldState = WorldState { roverPosition :: Position, roverDirection :: Direction, width :: Integer, height :: Integer, obstacles :: [Position]  }
    deriving (Eq, Show)

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
    L -> case roverDirection of
        N -> WorldState (Position roverX roverY) W width height obstacles
        S -> WorldState (Position roverX roverY) E width height obstacles
        E -> WorldState (Position roverX roverY) N width height obstacles
        W -> WorldState (Position roverX roverY) S width height obstacles
    R -> case roverDirection of
        N -> WorldState (Position roverX roverY) E width height obstacles
        S -> WorldState (Position roverX roverY) W width height obstacles
        E -> WorldState (Position roverX roverY) S width height obstacles
        W -> WorldState (Position roverX roverY) N width height obstacles

initialPosition = Position 10 10
initialState = WorldState initialPosition N 50 50 []

finalPosition = Position 10 11
finalState = WorldState finalPosition N 50 50 []

main :: IO ()
main = let
        test1 = (step initialState F) == finalState
       in
        if test1 == True then print "yay" else print "boo"
