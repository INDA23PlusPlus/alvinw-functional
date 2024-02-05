module Main where
import Data.List (sort)

main :: IO ()
main = putStrLn "Hello, Haskell!"

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse (x:s) = myReverse s ++ [x]

medianLength :: [String] -> Int
medianLength [] = 0
medianLength [x] = length x
medianLength list
    | odd (length list)
    = lengths !! (mid - 1)
    | otherwise
    = (lengths !! (mid - 1) + lengths !! mid) `div` 2
    where
        lengths = sort (map length list)
        mid = length list `div` 2
