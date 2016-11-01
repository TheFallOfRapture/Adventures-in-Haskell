module Math
    (
    testFunc,
    testFuncTwo
    ) where

testFunc :: (Num a, Show a) => a -> IO a
testFunc f = do
    putStrLn ("INPUT: " ++ show f)
    putStrLn (show f ++ " IS BEING DOUBLED")
    return (f * 2)

printNum :: (Num a, Show a) => a -> IO a
printNum x = do
    putStrLn $ show x
    return x

testFuncTwo :: (Num a, Show a) => a -> IO a
testFuncTwo f =
    putStrLn (show f ++ " HAS BEEN PROVIDED AS INPUT.") >> return f >>=
        \x -> putStrLn (show x ++ " HAS BEEN DOUBLED.") >> return (x * 2)
