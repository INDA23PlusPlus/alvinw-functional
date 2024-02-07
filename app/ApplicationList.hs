module ApplicationList where
import qualified Data.Set as Set

main :: IO ()
main = do
    line <- getLine
    let n = read line :: Int
    firstNames <- readNLines n
    lastNames <- readNLines n
    let count = uniquePeople firstNames lastNames
    putStrLn (show count :: String)

readNLines :: Int -> IO [String]
readNLines 0 = return []
readNLines n = do
    line <- getLine
    tail <- readNLines(n - 1)
    return (line : tail)

uniquePeople :: [String] -> [String] -> Int
uniquePeople firstNames lastNames
    = length people
    where
        people = Set.fromList(zip firstNames lastNames)