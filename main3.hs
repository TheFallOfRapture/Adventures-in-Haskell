import Control.Monad
import Data.Char

main :: IO ()
main = forever $ do
    putStrLn "Type some input: "
    input <- getLine
    putStrLn $ map toUpper input
