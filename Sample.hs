import VM
import Data.Time.Clock
import Text.Printf

-- test module
testModule :: Module
testModule = createModule
    [("FibRecur", 0, [
        [Clone 2, Const 3, Ge, JmpZ 2 1],
        [Clone 2, Const 1, Sub, Call "FibRecur", Cycle 2 1, Const 2, Sub, Call "FibRecur", Add, Ret],
        [Discard 1, Const 1, Ret]
    ]),
    ("FibIter", 2, [
        [Const 1, Clone 2, Save 0, Save 1, Clone 2, Const 3, Ge, JmpZ 3 1],
        [Const 2, Sub, Jmp 2],
        [Load 0, Load 1, Clone 2, Save 0, Add, Save 1, Const 1, Sub, Clone 2, JmpZ 3 2],
        [Discard 1, Load 1, Ret]
    ])]

-- execute Function from Module returning Int
executePrint :: Module -> String -> Int -> IO ()
executePrint m n a = case runVM m n [a] of
    Just [v] -> printf "%s %d = %d\n" n a v
    _        -> printf "Error\n"

-- measure execution time
timeExec :: IO t -> IO t
timeExec a = do
    start <- getCurrentTime
    v <- a
    stop <- getCurrentTime
    putStr "\tTime: "
    print $ diffUTCTime stop start
    return v

-- run specific test with defined argument
test :: String -> Int -> IO ()
test n a = timeExec $ executePrint testModule n a

-- run FibRecur test with defined argument
testFibRecur :: Int -> IO ()
testFibRecur = test "FibRecur"

-- run FibIter test with defined argument
testFibIter :: Int -> IO ()
testFibIter = test "FibIter"

-- run all samples
main :: IO ()
main = do
    testFibRecur 5
    testFibRecur 10
    testFibRecur 15
    testFibRecur 20
    testFibRecur 25
    testFibIter  5
    testFibIter  10
    testFibIter  15
    testFibIter  20
    testFibIter  25
    testFibIter  42
