module Dijkstra where
import qualified Data.Map as Map
import Data.Maybe

main :: IO ()
main = do
    (n, m, q, s) <- readFour
    run n m q s

readFour :: IO (Int, Int, Int, Int)
readFour = do
    line <- getLine
    let [aStr, bStr, cStr, dStr] = words line
    let a = read aStr :: Int
    let b = read bStr :: Int
    let c = read cStr :: Int
    let d = read dStr :: Int
    return (a, b, c, d)

readThree :: IO (Int, Int, Int)
readThree = do
    line <- getLine
    let [aStr, bStr, cStr] = words line
    let a = read aStr :: Int
    let b = read bStr :: Int
    let c = read cStr :: Int
    return (a, b, c)

readOne :: IO Int
readOne = do
    line <- getLine
    return (read line :: Int)

run :: Int -> Int -> Int -> Int -> IO ()
run 0 0 0 0 = return ()
run n m q s = do
    edges <- readEdges m

    runQueries edges s q
    putStrLn ""

    (n, m, q, s) <- readFour
    run n m q s

readEdges :: Int -> IO [(Int, Int, Int)]
readEdges 0 = return []
readEdges n = do
    (nodeA, nodeB, weight) <- readThree
    let head = (nodeA, nodeB, weight)
    tail <- readEdges (n - 1)
    return (head : tail)

runQueries :: [(Int, Int, Int)] -> Int -> Int -> IO ()
runQueries edges startNode 0 = return ()
runQueries edges startNode n = do
    endNode <- readOne

    let distances = Map.fromList [(startNode, 0)]
    let queue = [startNode]
    let result = recurseFindShortestPath edges startNode endNode distances queue

    case result of
        Just cost -> print cost
        Nothing   -> putStrLn "Impossible"

    runQueries edges startNode (n-1)

recurseFindShortestPath :: [(Int, Int, Int)] -> Int -> Int -> Map.Map Int Int -> [Int] -> Maybe Int
recurseFindShortestPath edges start end distances [] = Nothing -- Impossible
recurseFindShortestPath edges start end distances queue = do
    let next = getSmallest distances queue
    let cost = Map.findWithDefault (maxBound :: Int) next distances
    if next == end then
        Just cost
    else do
        let neighbours = findNeighbours next edges
        -- remove next from queue and add all neighbours to queue
        -- neighbours is (from, to, weight), we only want to add "to" to the queue
        let newQueue = filter (/= next) queue ++ map (\(_, to, _) -> to) neighbours
        let newDistances = updateDistances neighbours distances
        recurseFindShortestPath edges next end newDistances newQueue

updateDistances :: [(Int, Int, Int)] -> Map.Map Int Int -> Map.Map Int Int
updateDistances [] distances = distances -- no more neighbours, return distances
updateDistances ((from, to, weight):rest) distances =
    updateDistances rest newDistances
    where
        newDistance = Map.findWithDefault (maxBound :: Int) from distances + weight
        currentDistance = Map.findWithDefault (maxBound :: Int) to distances
        newDistances = Map.insert to (min currentDistance newDistance) distances

getSmallest :: Map.Map Int Int -> [Int] -> Int
getSmallest _ [x] = x
getSmallest distances queue
    -- Recursivly remove the highest one and keep the smallest distance one in head.
    | firstDist < secondDist
    = getSmallest distances (first : tailTail)
    | otherwise
    = getSmallest distances (second : tailTail)
    where
        first = head queue
        queueTail = tail queue
        tailTail = tail queueTail
        second = head queueTail
        firstDist = fromMaybe (maxBound :: Int) (Map.lookup first distances)
        secondDist = fromMaybe (maxBound :: Int) (Map.lookup second distances)

findNeighbours :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
findNeighbours x edges = filter (\(node, _, _) -> node == x) edges
