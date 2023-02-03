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

test = do
    print equal
    print ineql
    print iesze

-- Remove comment of following line to run the test.
main = test

