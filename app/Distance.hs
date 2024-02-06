module Distance where
import Data.Char (chr)

main :: IO ()
main = do
    (width, height) <- readInts
    resultPart height width 0

readInts :: IO (Int, Int)
readInts = do
    line <- getLine
    let [aStr, bStr] = words line
    let a = read aStr :: Int
    let b = read bStr :: Int
    return (a, b)

distance :: Int -> Int -> Int -> Int -> Int
distance width height x y =
    minimum [left, right, top, bottom]
    where
        left = x + 1
        right = width - x
        top = y + 1
        bottom = height - y

icon :: Int -> Char
icon dist
    | dist < 10 = chr (dist + 48)
    | otherwise = '.'

linePart :: Int -> Int -> Int -> Int -> String
linePart width height x y
    | x < width
    = (square:(linePart width height (x + 1) y))
    | otherwise
    = ""
    where
        dist = distance width height x y
        square = icon dist

resultPart :: Int -> Int -> Int -> IO ()
resultPart width height y
    | y < height = do
        putStrLn (linePart width height 0 y)
        resultPart width height (y + 1)
    | otherwise = return ()
