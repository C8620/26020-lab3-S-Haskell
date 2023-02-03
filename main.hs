import Debug.Trace

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
blur x = case x of
    Black n -> allBlack n
    White n -> allWhite n
    Block a b c d -> bluring x x 0 0 n
    where n = size x

-- Bluring a cell - go through the tree and ask for the correct colour of each cell.
bluring :: Cell -> Cell -> Int -> Int -> Int -> Cell
bluring whole this i j n = case this of
    Black n' -> calc whole this i j
    White n' -> calc whole this i j
    Block a b c d -> do
        let s = size a
        if (s /= (n `div` 2))
            then error "Size does not match"
            else do
                let a' = bluring whole a i j s
                let b' = bluring whole b (i + s) j s
                let c' = bluring whole c (i + s) (j + s) s
                let d' = bluring whole d i (j + s) s
                clockwise a' b' c' d'


statsAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
statsAdd (a, b) (c, d) = ((a + c), (b + d))

-- Calc a cell - obtain the correct colour of the cell.
calc :: Cell -> Cell -> Int -> Int -> Cell
calc whole this x y = do
        let (black, total) = find whole x y (size this) 0 0
        case this of
            Black n -> do
                if ((black * 2) < total)
                    then allWhite (n)
                    else allBlack (n)
            White n -> do
                if ((black * 2) > total)
                    then allBlack (n)
                    else allWhite (n)
            Block a b c d -> do
                error "Block in calc"

-- Find a cell - obtain all neighbours of a cell.
find :: Cell -> Int -> Int -> Int -> Int -> Int -> (Int, Int)
find tree x y n i j = case tree of
    Black n' -> do
        if near x y n i j n'
            then (1, 1)
            else (0, 0)
    White n' -> do
        if near x y n i j n'
            then (0, 1)
            else (0, 0)
    Block a b c d -> do
        let s = size a
        let a' = find a x y n i j
        let b' = find b x y n (i + s) j
        let c' = find c x y n (i + s) (j + s)
        let d' = find d x y n i (j + s)
        statsAdd (statsAdd a' b') (statsAdd c' d')

-- Near of two cells - check if two cells are near.
near :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
near x1 y1 n1 x2 y2 n2 = do
    if (x1 == x2) && (y1 == y2) && (n1 == n2)
    then False
    else do
        let x1' = x1 + n1
        let y1' = y1 + n1
        let x2' = x2 + n2
        let y2' = y2 + n2
        if ((x1==x2') || (x1'==x2)) && ((((y2>y1) && (y2<y1')) || (((y2'>y1)) && (y2'<y1'))) || (((y1>y2) && (y1<y2')) || (((y1'>y2)) && (y1'<y2'))) || ((y1==y2) && (y1'==y2')))
            then do 
                True
                -- trace ( (show x1) ++ " " ++ (show y1) ++ " " ++ (show n1) ++ " ; " ++ (show x2) ++ " " ++ (show y2) ++ " " ++ (show n2)) True
        else if ((y1==y2') || (y1'==y2)) && ((((x2>x1) && (x2<x1')) || (((x2'>x1)) && (x2'<x1')))|| ((((x1>x2) && (x1<x2'))) || (((x1'>x2)) && (x1'<x2'))) || ((x1==x2) && (x1'==x2')))
            then do
                True
                -- trace ( (show x1) ++ " " ++ (show y1) ++ " " ++ (show n1) ++ " ; " ++ (show x2) ++ " " ++ (show y2) ++ " " ++ (show n2)) True
        else False

-- Test cases for blur function.
bl :: Cell -> Cell -> Cell -> Cell -> Cell
bl a b c d = clockwise a b c d
b n = allBlack n
w n = allWhite n
bTest0  = bTest01 && bTest02 && bTest03 && bTest04
bTest01 = blur (allBlack 1) == allBlack 1
bTest02 = blur (allWhite 1) == allWhite 1
bTest03 = blur (allBlack 2) == allBlack 2
bTest04 = blur (allWhite 2) == allWhite 2
bTest1  = blur (clockwise (allWhite 2) (clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allWhite 1)) (allBlack 2) (allWhite 2)) == clockwise (allWhite 2) (clockwise (allWhite 1) (allBlack 1) (allBlack 1) (allBlack 1)) (allWhite 2) (allWhite 2)
bTest2i = (bl (bl (bl (b 1) (b 1) (w 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (w 2) (b 2)) (bl (b 2) (b 2) (bl (b 1) (b 1) (b 1) (w 1)) (bl (b 1) (b 1) (w 1) (w 1))) (w 4) (bl (b 2) (bl (b 1) (b 1) (w 1) (b 1)) (bl (b 1) (w 1) (w 1) (b 1)) (b 2)))
bTest2a = (bl (bl (bl (b 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (b 2) (b 2)) (bl (b 2) (b 2) (bl (b 1) (b 1) (w 1) (w 1)) (bl (b 1) (b 1) (w 1) (w 1))) (w 4) (bl (b 2) (bl (b 1) (w 1) (w 1) (b 1)) (bl (b 1) (w 1) (w 1) (b 1)) (b 2)))
bTest2  = blur bTest2i == bTest2a
bTest3i = (bl (bl (bl (w 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (b 2) (b 2)) (bl (b 2) (b 2) (bl (w 1) (w 1) (w 1) (w 1)) (bl (w 1) (b 1) (b 1) (b 1))) (w 4) (bl (b 2) (bl (w 1) (w 1) (b 1) (b 1)) (bl (b 1) (w 1) (b 1) (b 1)) (b 2)))
bTest3a = (bl (bl (bl (b 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (b 1)) (w 2) (b 2)) (bl (b 2) (w 2) (bl (w 1) (w 1) (w 1) (w 1)) (bl (b 1) (b 1) (b 1) (b 1))) (w 4) (bl (b 2) (bl (b 1) (w 1) (w 1) (b 1)) (bl (b 1) (b 1) (w 1) (b 1)) (b 2)))
bTest3  = blur bTest3i == bTest3a

printspace :: Int -> IO ()
printspace 0 = return ()
printspace n = do
    putStr " "
    printspace (n - 1)

printcell :: Cell -> Int -> IO ()
printcell tree n = case tree of
    Black n' -> do
        printspace n
        putStr "B"
        print n'
    White n' -> do
        printspace n
        putStr "W"
        print n'
    Block a b c d -> do
        printcell a (n + 1)
        printcell b (n + 1)
        printcell c (n + 1)
        printcell d (n + 1)

test = do
    print "DS Equality Test"
    print equal
    print "DS Inequality Test"
    print ineql
    print "DS Size Test"
    print iesze
    print "Blur Test Basic"
    print bTest0
    print "Blur Test 1"
    print bTest1
    print "Blur Test 2"
    print bTest2
    print "Blur Test 3"
    print bTest3


-- Remove comment of following line to run the test.
main = test