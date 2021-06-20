module VM where

import Data.Array
import qualified Data.Map as Map

-- Virtual machine instructions
data Instruction
    = Const Int
    | Clone Int | Discard Int | Cycle Int Int
    | Add | Sub | Mul | Div | Rem
    | Gt | Ge | Lt | Le | Eq | Ne | Not
    | CallI Int | Call String
    | Jmp Int | JmpZ Int Int | Ret
    | Err String
    | Save Int | Load Int
    deriving (Eq, Show)

-- local storage (array or empty)
data LocalStorage
    = ArrayLS (Array Int Int)
    | EmptyLS
    deriving (Show)

-- Virtual machine state
-- stack and local storage if valid
-- error message otherwise
data VM
    = VMError String
    | VMState [Int] [LocalStorage]
    deriving (Show)

-- function (name, number of local variables, array of blocks of instructions)
data Function = Function {
        functionName   :: String,
        functionLocals :: Int,
        functionBlocks :: Array Int [Instruction]
    } deriving (Show)

-- module (array of functions, name-index function map)
data Module = Module {
        moduleFuns :: Array Int Function,
        moduleMap  :: Map.Map String Int
    } deriving (Show)

-- create empty valid vm state
emptyVM :: VM
emptyVM = VMState [] []

-- push int to the virtual machine stack
-- returns new vm state
pushInts :: [Int] -> VM -> VM
pushInts v (VMState s l) = VMState (v ++ s) l
pushInts _ vm            = vm

-- pop value from the vm stack
-- returns new vm state and the value
popInt :: VM -> (VM, Maybe Int)
popInt (VMState (x : xs) l) = (VMState xs l, Just x)
popInt (VMState _        _) = (VMError "Empty stack", Nothing)
popInt vm                   = (vm, Nothing)

-- clone the value on top of the vm stack n times
-- returns new vm state
cloneTop :: Int -> VM -> VM
cloneTop n (VMState (x : xs) l) = VMState (take n (repeat x) ++ xs) l
cloneTop _ (VMState _        _) = VMError "Empty stack"
cloneTop _ vm                   = vm

-- negates the value on the top of the stack
-- returns new vm state
notTop :: VM -> VM
notTop (VMState (x : xs) l)
    | x == 0    = VMState (1 : xs) l
    | otherwise = VMState (0 : xs) l
notTop (VMState _        _) = VMError "Empty stack"
notTop vm                   = vm

-- discard n values from the top of vm stack
-- returns new vm state
discardTop :: Int -> VM -> VM
discardTop n (VMState s l) = VMState (drop n s) l
discardTop _ vm            = vm

-- cycle top of the vm stack
-- returns new vm state
cycleTop :: Int -> Int -> VM -> VM
cycleTop n c (VMState s l) = VMState ((cycleList c $ take n s) ++ drop n s) l
    where
        cycleList n' l' = drop n' l' ++ take n' l'
cycleTop _ _ vm            = vm

-- apply integer binary operator on the top of the stack
-- returns new vm state
binOp :: (Int -> Int -> Int) -> VM -> VM
binOp f (VMState (x1:x2:xs) l) = VMState (x2 `f` x1 : xs) l
binOp _ (VMState _          _) = VMError "Not enough arguments"
binOp _ vm                     = vm

-- wrap binary operator so it returns int
boolOp :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
boolOp f = \a b -> boolToInt $ f a b
    where
        boolToInt False = 0
        boolToInt True  = 1

-- push local storage
-- returns new vm state
pushLocal :: Int -> VM -> VM
pushLocal 0 (VMState s l) = VMState s $ EmptyLS : l
pushLocal n (VMState s l) = VMState s $ ArrayLS (listArray (0, n - 1) [0..]) : l
pushLocal _ vm            = vm

-- pop local storage
-- returns new vm state
popLocal :: VM -> VM
popLocal (VMState s (_ : ls)) = VMState s ls
popLocal (VMState _ _)        = VMError "No local storage to pop"
popLocal vm                   = vm

-- pop single value from the stack and save the value to the local storage
-- returns new vm state
saveLocal :: Int -> VM -> VM
saveLocal i (VMState (s:ss) (ArrayLS l : ls)) = VMState ss $ ArrayLS (l // [(i, s)]) : ls
saveLocal _ (VMState _      _)                = VMError "Failed to save local variable"
saveLocal _ vm                                = vm

-- pop single value from the stack and save the value to the local storage
-- returns new vm state
loadLocal :: Int -> VM -> VM
loadLocal i (VMState s ls@(ArrayLS l : _)) = VMState (l ! i : s) ls
loadLocal _ (VMState _    _)               = VMError "Failed to load local variable"
loadLocal _ vm                             = vm

-- execute instruction list
-- returns new vm state and next executed block index or Nothing on function return
execute :: Module -> VM -> [Instruction] -> (VM, Maybe Int)
execute _ (VMError e) _      = (VMError e, Nothing)
execute _ _           []     = (VMError "No more instructions", Nothing)
execute m vm          (x:xs) = case x of
        Const v    -> apply1 $ pushInts [v]
        Clone n    -> apply1 $ cloneTop n
        Discard n  -> apply1 $ discardTop n
        Save i     -> apply1 $ saveLocal i
        Load i     -> apply1 $ loadLocal i
        Cycle n c  -> apply1 $ cycleTop n c
        Not        -> apply1 $ notTop
        Add        -> apply2 (+)
        Sub        -> apply2 (-)
        Mul        -> apply2 (*)
        Div        -> apply2 div
        Rem        -> apply2 rem
        Gt         -> apply2 $ boolOp (>)
        Ge         -> apply2 $ boolOp (>=)
        Lt         -> apply2 $ boolOp (<)
        Le         -> apply2 $ boolOp (<=)
        Eq         -> apply2 $ boolOp (==)
        Ne         -> apply2 $ boolOp (/=)
        CallI i    -> apply1 $ executeFunction m i
        Ret        -> (vm, Nothing)
        Jmp b      -> (vm, Just b)
        JmpZ b1 b2 -> case popInt vm of
            (vm', Just v ) -> (vm', Just $ if v == 0 then b1 else b2)
            (vm', Nothing) -> (vm', Nothing)
        Call n     -> case lookupFunction m n of
            Just i  -> apply1 $ executeFunction m i
            Nothing -> (VMError "Unknown function", Nothing)
        Err e      -> (VMError e, Nothing)
    where
        apply1 f = execute m (f vm) xs
        apply2 f = execute m (binOp f vm) xs

-- get Function on index from Module
getFunction :: Module -> Int -> Function
getFunction m i = (moduleFuns m) ! i

-- lookup function index by name
lookupFunction :: Module -> String -> Maybe Int
lookupFunction m n = Map.lookup n $ moduleMap m

-- execute Function on index from Module
executeFunction :: Module -> Int -> VM -> VM
executeFunction m fi v = popLocal $ go (pushLocal fl v) 0
    where
        fn = getFunction m fi
        fl = functionLocals fn
        go vm bi = case execute m vm $ functionBlocks fn ! bi of
            (vm', Just bi') -> go vm' bi'
            (vm', Nothing ) -> vm'

-- create module from the list of functions
createModule :: [(String, Int, [[Instruction]])] -> Module
createModule f = Module mf mm
    where
        -- module function name-index map
        mm = Map.fromList $ zip (map (\(fn,_,_) -> fn) f) [0..]
        -- module functions
        mf = listArray (0, length f - 1) $ map (constructTransform) f
        -- construct function
        construct n l b = Function n l $ listArray (0, length b - 1) b
        -- transform function
        constructTransform (fn,fl,fi) = construct fn fl $ map (map swapCall) fi
        -- swap named calls for indexed calls
        swapCall (Call cn) = case Map.lookup cn $ mm of
            Just i  -> CallI i
            Nothing -> Err "Unknown function"
        swapCall c = c


-- run function on empty VM with predefined stack
runVM :: Module -> String -> [Int] -> Maybe [Int]
runVM m n a = case lookupFunction m n of
    Just i -> case executeFunction m i (pushInts a emptyVM) of
        VMState s _ -> Just s
        _           -> Nothing
    _      -> Nothing
