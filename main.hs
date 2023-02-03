-- A Cell is a type of single cell that can be either all black or all white.
data Cell = Black Int | White Int | Block Cell Cell Cell Cell deriving (Eq, Show)

-- Function to get size of a cell.
size :: Cell -> Int
size = \x -> case x of
    Black n -> n
    White n -> n
    Block a b c d -> (size a)*2

-- A block is a type of NxN block that is all Black.
allBlack :: Int -> Cell
allBlack n = (Black n)

-- A block is a type of NxN block that is all White.
allWhite :: Int -> Cell
allWhite n = (White n)

-- Return a block with data in clockwise order starting from the top left, if sizes match.
clockwise :: Cell -> Cell -> Cell -> Cell -> Cell
clockwise a b c d = if (size a == size b) && (size b == size c) && (size c == size d) 
                    then Block a b c d
                    else error "Sizes do not match."

-- Return a block with data in counter-clockwise order starting from the top left, if sizes match.
anticlockwise :: Cell -> Cell -> Cell -> Cell -> Cell
anticlockwise a b c d = clockwise a d c b

-- Test cases for data structure.
equal = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) == anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)
ineql = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) /= anticlockwise (allBlack 1) (allBlack 1) (allWhite 1) (allBlack 1)
iesze = allBlack 128 /= allBlack 2

-- Blur a cell - this is a constructive function.
blur :: Cell -> Cell
blur x = bluring x x 0 0 (size x)

-- Bluring a cell - go through the tree and blur the cell.
bluring :: Cell -> Cell -> Int -> Int -> Int -> Cell
bluring whole this i j n = case this of
    Black n -> if calc whole i j n
                then (Black n)
                else (White n)
    White n -> if calc whole i j n
                then (Black n)
                else (White n)
    Block a b c d -> do
        let a' = bluring whole a i j (n `div` 2)
        let b' = bluring whole b i (j + (n `div` 2)) (n `div` 2)
        let c' = bluring whole c (i + (n `div` 2)) j (n `div` 2)
        let d' = bluring whole d (i + (n `div` 2)) (j + (n `div` 2)) (n `div` 2)
        clockwise a' b' c' d'

-- Stats - show the neighbours and number of them who are BLACK.
data Stats = Stats Int Int deriving (Eq, Show)

statsAdd :: Stats -> Stats -> Stats
statsAdd (Stats a b) (Stats c d) = Stats (a + c) (b + d)

-- Calc a cell - obtain the correct colour of the cell.
calc :: Cell -> Int -> Int -> Int -> Bool
calc tree x y n = case tree of
    Black n -> tree
    White n -> tree
    Block a b c d -> do
        let result = statsAdd (Stats 0 0) 
        

-- Near of two cells - check if two cells are near.
near :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
near x1 y1 n1 x2 y2 n2 = do
    let x1' = x1 + n1
    let y1' = y1 + n1
    let x2' = x2 + n2
    let y2' = y2 + n2
    True

-- Find a cell - obtain all neighbours of a cell.



test = do
    print equal
    print ineql
    print iesze

-- Remove comment of following line to run the test.
main = test

