module Sum where
import Data.List (sort)

main :: IO ()
main = do
    line <- getLine
    let n = read line :: Int
    let nHalf = numbersToSum n
    line2 <- getLine
    let numbers = map (read :: String -> Int) (words line2)
    putStrLn (show (sumHighest nHalf numbers))

numbersToSum :: Int -> Int
numbersToSum x
    | even x
    = x `div` 2
    | otherwise
    = (x + 1) `div` 2

sumHighest :: Int -> [Int] -> Int
sumHighest n numbers
    = sum sublist
    where
        sorted = sort numbers
        sublist = drop (length(sorted) - n) sorted