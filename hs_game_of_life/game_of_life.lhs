A simple and minimal implementation of Conway's game of life.

From wikipedia:

"The universe of the Game of Life is an infinite two-dimensional orthogonal grid of square cells,
each of which is in one of two possible states, alive or dead."

We will use a set as the underlying data structure, thus:


>import qualified Data.Set as Set

See http://haskell.org/ghc/docs/latest/html/libraries/containers-0.4.1.0/Data-Set.html for the documentation about the Set module.


And we define the following type aliases:


>type Coord2d = (Int, Int)
>type World = Set.Set Coord2d

We build our world from the coordinate of the living cells:

>buildWorld :: [Coord2d] -> World
>buildWorld = Set.fromList


"Every cell interacts with its eight neighbours, which are the cells that are horizontally, vertically, or diagonally adjacent. "

So we need a function that given a coordinate, it generate the coordinates of the neighbours (using a list comprehension):


>neighbours :: Coord2d -> [Coord2d]
>neighbours (x,y) = [(xn,yn) | xn <- [x-1 .. x+1], yn <- [y-1 .. y+1], (x,y) /= (xn, yn) ]


The rules are for the live cells: 

"At each step in time, the following transitions occur:

    Any live cell with fewer than two live neighbours dies, as if caused by under-population.
    Any live cell with two or three live neighbours lives on to the next generation.
    Any live cell with more than three live neighbours dies, as if by overcrowding."


So we need to know the state :

>data CellState = Die | Live deriving (Show, Eq)

>transitionLiveCell :: Int -> CellState
>transitionLiveCell 2 = Live
>transitionLiveCell 3 = Live
>transitionLiveCell _ = Die

Then define the count of live cell given a coordinate:

>countLiveCellAtCoord :: World -> Coord2d -> Int
>countLiveCellAtCoord world coord = Set.size livingCells where
>					livingCells = world `Set.intersection` (Set.fromList neighboursCoords)
>					neighboursCoords = neighbours coord 


Next we compose the function that handle a single cell:

>nextLiveCellState :: World -> Coord2d -> CellState
>nextLiveCellState world coord = transitionLiveCell $ countLiveCellAtCoord world coord


With the functions currently defined, we build the next state for the living cells.
We will keep the cells that have their state as Live:

>transitionWorldLiveCell :: World -> World
>transitionWorldLiveCell world = Set.filter (\coord-> nextLiveCellState world coord == Live) world


Now we need to take care of the dead cell rule:

"Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction."

So we will define the following function:

>transitionDeadCell :: Int -> CellState
>transitionDeadCell 3 = Live
>transitionDeadCell _ = Die


Our area of interest are all the dead neighbours of the living cells. How do we extract the coordinates?

Well, it's the difference between the set of all the neighbours and the living cells. 

Let's first generate the Set of all the neighbours, folding over world:

>allNeighbours :: World -> World
>allNeighbours world = Set.fold collectAllNeighbours Set.empty world where
>			collectAllNeighbours coord neighboursSet = (Set.fromList (neighbours coord)) `Set.union` neighboursSet


We define the function that get all the dead cells we are interested in:

>allDeadNeighbours :: World -> World
>allDeadNeighbours world = (allNeighbours world) `Set.difference` world


Now we define the transition for a dead cell:

>nextDeadCellState :: World -> Coord2d -> CellState
>nextDeadCellState world  coord = transitionDeadCell $ countLiveCellAtCoord world coord

Then we define the transition for all the dead cell:

>transitionWorldDeadCell :: World -> World
>transitionWorldDeadCell world = Set.filter(\coord -> nextDeadCellState world coord == Live) $ allDeadNeighbours world

Finally the whole transition is the union of the living and dead transition:


>transitionWorld :: World -> World
>transitionWorld world = (transitionWorldLiveCell world) `Set.union` (transitionWorldDeadCell world)


Iterating continously the transitionWorld function, we have all the possible worlds given the initial state:

>allTransitions :: World -> [World]
>allTransitions world = iterate transitionWorld world

If we want to keep track of the generation number:

>allTransitionsWithGenerationNumber :: World -> [(Int, World)]
>allTransitionsWithGenerationNumber world = zip [1..] $ allTransitions world


For the graphical part, we need some kind of function for printing the whole board.

We need to extract the extremes.

>type BoardCoord = (Coord2d, Coord2d) -- (top left corner, bottom right corner), or better ((MinX,MaxY), (MaxX,MinY))

Given a coordinate and the BoardCoord we have potentially a new BoardCoord:

>extractExtremes :: Coord2d -> BoardCoord -> BoardCoord
>extractExtremes (x,y) ((minX, maxY), (maxX, minY)) = ((newMinX, newMaxY), (newMaxX, newMinY)) where
>							newMinX = min x minX
>							newMaxY = max y maxY
>							newMaxX = max x maxX
>							newMinY = min y minY


We fold over the whole world for extracting the top left corner coordinate and the bottom right that can fit the whole game:

>extractBoardCorners :: World -> BoardCoord
>extractBoardCorners world = Set.fold extractExtremes (initialCoordinate, initialCoordinate) world
>				where initialCoordinate = if Set.null world then (0,0) else Set.findMin world


so now we can build:

>printBoard :: World -> [(Char, Bool)]
>printBoard world = [(printCell (Set.member (x,y) world), nextLine x) | y <- [minY..maxY], x <- [minX..maxX]] where
>			((minX,maxY), (maxX, minY)) = extractBoardCorners world
>			printCell cond = if cond then '*' else '.'
>			nextLine x = x == maxX


Finally, welcome to the impure world!

>printBoardIO :: World -> IO()
>printBoardIO world = mapM_ putCellAndNewLine $ printBoard world where
>				putCellAndNewLine (char, newLine) = putChar char >> if newLine then putChar '\n' else return ()


For example, we define the function that print the N iterations of the board:

>printBoardNIterationsIO :: Int -> World -> IO()
>printBoardNIterationsIO n world = mapM_ (\(gen, world) -> newIteration gen >> printBoardIO world) nTransitions where
>					nTransitions = take n $ allTransitionsWithGenerationNumber world
>					newIteration gen = putStrLn "--------------" >> putStr "gen" >> ((putStrLn . show) gen) >> putStrLn ""



Example of output:

*Main> printBoardNIterationsIO 10 $ buildWorld [(0,0), (0,1),(0,2)]
--------------
gen1

*
*
*
--------------
gen2

***
--------------
gen3

*
*
*
--------------
...


*Main>  printBoardNIterationsIO 20 $ buildWorld [(0,0), (0,1),(0,2), (1,1)]
--------------
gen1

*.
**
*.
--------------
gen2

.**
***
.**
--------------
gen3

*.*.
*..*
*.*.
--------------
gen4

..*..
**.**
..*..
--------------
gen5

***
*.*
***
--------------
gen6

..*..
.*.*.
*...*
.*.*.
..*..
--------------
gen7

..*..
.***.
**.**
.***.
..*..
--------------
gen8

.***.
*...*
*...*
*...*
.***.
--------------
gen9

...*...
..***..
.*.*.*.
***.***
.*.*.*.
..***..
...*...
--------------
gen10

..***..
.......
*.....*
*.....*
*.....*
.......
..***..
--------------
gen11

....*....
....*....
....*....
.........
***...***
.........
....*....
....*....
....*....
--------------
gen12

..***..
.......
*.....*
*.....*
*.....*
.......
..***..
--------------

