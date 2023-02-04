-- ====================================================================== --
-- Codes for testing, comment out the main function to run the test.      --
-- ====================================================================== --
main = test
import Debug.Trace
-- ====================================================================== --


-- ====================================================================== --
-- Exercise 1: Data Structures                                            --
-- ====================================================================== --

-- ADT:      Cell
-- Purpose:  Represent a cell in a 2D grid.
-- Example:  Black 1 -- A 1x1 Black cell
--           White 2 -- A 2x2 White cell
--           Block (Black 2) (Black 2) (Black 2) (Black 2) -- A 4x4 block 
--             of 4 Black cells
-- Notes:    A Cell could be a Black cell, a White cell, or a Block of 4 
--             cells in clockwise order starting from the top left.
data Cell = Black Int | White Int | Block Cell Cell Cell Cell deriving (Eq, Show)

-- Function: size
-- Purpose:  Return the size of a cell.
-- Example:  size (Black 2) = 2
--           size (Block (Black 2) (Black 2) (Black 2) (Black 2)) = 4
size :: Cell -> Int
size = \x -> case x of
    Black n -> n
    White n -> n
    Block a b c d -> (size a)*2

-- Function: allBlack
-- Purpose:  Return a cell unit (not block) of size NxN that is Black.
-- Example:  allBlack 2 = Black 2
--           allBlack 4 = Black4
allBlack :: Int -> Cell
allBlack n = (Black n)

-- Function: allWhite
-- Purpose:  Return a cell unit (not block) of size NxN that is White.
-- Example:  allWhite 2 = White 2
--           allWhite 4 = White 4
allWhite :: Int -> Cell
allWhite n = (White n)

-- Function: clockwise
-- Purpose:  Return a block with data in clockwise order starting from the
--             top left, if sizes match.
-- Example:  clockwise (Black 2) (Black 2) (Black 2) (Black 2) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           clockwise (Black 2) (Black 2) (Black 2) (White 2) =
--             Block (Black 2) (Black 2) (Black 2) (White 2)
clockwise :: Cell -> Cell -> Cell -> Cell -> Cell
clockwise a b c d = if (size a == size b) && (size b == size c) && (size c == size d) 
                    then Block a b c d
                    else error "Sizes do not match."

-- Function: anticlockwise
-- Purpose:  Return a block with data in anticlockwise order starting from
--             the top left, if sizes match.
-- Example:  anticlockwise (Black 2) (Black 2) (Black 2) (Black 2) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           anticlockwise (Black 2) (Black 2) (Black 2) (White 2) =
--             Block (Black 2) (White 2) (Black 2) (Black 2)
anticlockwise :: Cell -> Cell -> Cell -> Cell -> Cell
anticlockwise a b c d = clockwise a d c b
-- ====================================================================== --


-- ====================================================================== --
-- Exercise 2: A crude ‘blurring’ operation                               --
-- ====================================================================== --

-- Function: blur
-- Purpose:  Return a blurred version of a cell.
-- Example:  blur (Block (Black 2) (Black 2) (Black 2) (Black 2)) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           blur (Block (Black 2) (Black 2) (Black 2) (White 2)) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           blur (Block (Black 2) (Black 2) (White 2) (White 2)) =
--             Block (Black 2) (Black 2) (White 2) (White 2)
-- Notes:    This function is a constructive function, it does not modify
--             the original cell. It calls for the function bluring.
blur :: Cell -> Cell
blur x = case x of
    Black n -> allBlack n
    White n -> allWhite n
    Block a b c d -> bluring x x 0 0 n
    where n = size x

-- Function: bluring
-- Purpose:  Return a blurred version of a cell.
-- Example:  bluring (Block (Black 2) (Black 2) (Black 2) (Black 2)) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           bluring (Block (Black 2) (Black 2) (Black 2) (White 2)) =
--             Block (Black 2) (Black 2) (Black 2) (Black 2)
--           bluring (Block (Black 2) (Black 2) (White 2) (White 2)) =
--             Block (Black 2) (Black 2) (White 2) (White 2)
-- Notes:    If this function is called by a function other than itself, 
--             `whole` should be the same as `this`, and is not a unit 
--             cell.
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

-- Function: statsAdd
-- Purpose:  Add two tuples of Ints.
-- Example:  statsAdd (1, 2) (3, 4) = (4, 6)
-- Notes:    This function is used in the function `calc`.
statsAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
statsAdd (a, b) (c, d) = ((a + c), (b + d))

-- Calc a cell - obtain the correct colour of the cell.
-- Function: calc
-- Purpose:  Return the correct colour of a given cell.
-- Example:  calc (Block (Black 2) (Black 2) (Black 2) (Black 2)) (Black 2) 0 0 =
--             Black 2
-- Notes:    This function is used in the function `bluring`.
--           `x` and `y` are the coordinates of `this` in the `whole` 
--             cell.
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
-- Function: find
-- Purpose:  Return the number of black cells and the total number of 
--             cells that is the neighbour of a given cell indicated
--             by the coordinates `x` and `y`.
-- Example:  find (Block (Black 2) (Black 2) (Black 2) (Black 2)) 0 0 2 0 0 =
--             (2, 2)
--           find (Block (Black 2) (Black 2) (Black 2) (White 2)) 0 0 2 0 0 =
--             (1, 2)
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
-- Function: near
-- Purpose:  Return True if two cells are near, False otherwise.
-- Example:  near 0 0 2 0 0 2 = False (same thing)
--           near 0 0 2 2 0 2 = True  (same row)
--           near 0 0 2 0 2 2 = True  (same column)
--           near 0 0 2 0 2 4 = True  (shared side)
--           near 0 0 2 2 2 2 = False (shared corner)
-- Notes:    This function is used in the function `find`.
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

-- ====================================================================== --


-- ====================================================================== --
-- Test cases.                                                            --
-- ====================================================================== --

-- Run all test cases / List of all test cases.
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

-- Data structure test cases.
-- Test for equality of data structure.
equal = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) == anticlockwise (allBlack 1) (allWhite 1) (allWhite 1) (allBlack 1)
-- Test for inequality of data structure.
ineql = clockwise (allBlack 1) (allBlack 1) (allWhite 1) (allWhite 1) /= anticlockwise (allBlack 1) (allBlack 1) (allWhite 1) (allBlack 1)
-- Test for size function.
iesze = allBlack 128 /= allBlack 2

-- Test cases for blur function.
-- Basic tests of Blur function's exceptions.
bTest0  = bTest01 && bTest02 && bTest03 && bTest04
bTest01 = blur (allBlack 1) == allBlack 1
bTest02 = blur (allWhite 1) == allWhite 1
bTest03 = blur (allBlack 2) == allBlack 2
bTest04 = blur (allWhite 2) == allWhite 2
-- Test for the correctness of the blur function.
-- Test 1, taken feom page 8's example.
bTest1  = blur (clockwise (allWhite 2) (clockwise (allBlack 1) (allBlack 1) (allBlack 1) (allWhite 1)) (allBlack 2) (allWhite 2)) == clockwise (allWhite 2) (clockwise (allWhite 1) (allBlack 1) (allBlack 1) (allBlack 1)) (allWhite 2) (allWhite 2)
-- Test 2, taken feom page 6's first example.
bTest2i = (bl (bl (bl (b 1) (b 1) (w 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (w 2) (b 2)) (bl (b 2) (b 2) (bl (b 1) (b 1) (b 1) (w 1)) (bl (b 1) (b 1) (w 1) (w 1))) (w 4) (bl (b 2) (bl (b 1) (b 1) (w 1) (b 1)) (bl (b 1) (w 1) (w 1) (b 1)) (b 2)))
bTest2a = (bl (bl (bl (b 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (b 2) (b 2)) (bl (b 2) (b 2) (bl (b 1) (b 1) (w 1) (w 1)) (bl (b 1) (b 1) (w 1) (w 1))) (w 4) (bl (b 2) (bl (b 1) (w 1) (w 1) (b 1)) (bl (b 1) (w 1) (w 1) (b 1)) (b 2)))
bTest2  = blur bTest2i == bTest2a
-- Test 3, taken feom page 6's second example.
bTest3i = (bl (bl (bl (w 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (w 1)) (b 2) (b 2)) (bl (b 2) (b 2) (bl (w 1) (w 1) (w 1) (w 1)) (bl (w 1) (b 1) (b 1) (b 1))) (w 4) (bl (b 2) (bl (w 1) (w 1) (b 1) (b 1)) (bl (b 1) (w 1) (b 1) (b 1)) (b 2)))
bTest3a = (bl (bl (bl (b 1) (b 1) (b 1) (b 1)) (bl (b 1) (b 1) (b 1) (b 1)) (w 2) (b 2)) (bl (b 2) (w 2) (bl (w 1) (w 1) (w 1) (w 1)) (bl (b 1) (b 1) (b 1) (b 1))) (w 4) (bl (b 2) (bl (b 1) (w 1) (w 1) (b 1)) (bl (b 1) (b 1) (w 1) (b 1)) (b 2)))
bTest3  = blur bTest3i == bTest3a

-- Auxiliary function for building test cases.
-- bl as in block.
bl :: Cell -> Cell -> Cell -> Cell -> Cell
bl a b c d = clockwise a b c d
-- b as in black.
b n = allBlack n
-- w as in white.
w n = allWhite n
-- Print a cell entity.

-- Auxiliary function for printing a cell entity.
-- Function: printcell
-- Purpose:  Print a cell entity.
-- Example:  printcell (allBlack 1) 0 = B1
-- Notes:    If called by other functions, Int should be 0.
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
-- Function: printspace
-- Purpose:  Print spaces.
-- Example:  printspace 3 = "   "
printspace :: Int -> IO ()
printspace 0 = return ()
printspace n = do
    putStr " "
    printspace (n - 1)