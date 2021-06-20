# Haskell-VM
Small stack-based virtual machine made in Haskell.
Virtual machine operates only with integers and is generally designed only as a proof of concept.

## Instructions
 - Const *c* - push c on the top of the stack
 - Clone *n* - pop value, push it *n* times
 - Discard *n* - discard n values from the top of the stack
 - Cycle *n* *m* - pop n values, rotate them m times and push them in that order
 - Add | Sub | Mul | Div | Rem - pop two values, apply corresponding operation (top value being the second operand), push result
 - Gt | Ge | Lt | Eq | Ne - pop two values, compare them and push 1 or 0 to the stack
 - Not - pop value, push 1 if value is 0, otherwise push 0
 - Call *name* - call function called *name*
 - CallI *i* - call *i*-th function in current module
 - Jmp *i* - jump to *i*-th block in current function
 - JmpZ *i* *j* - pop value, jump to *i*-th block in current function if value is 0, otherwise jump to *j*-th block
 - Ret - return from current function
 - Err *err* - throw error
 - Save *i* - pop value, save the value to *i*-th local storage
 - Load *i* - push value from *i*-th local storage to the stack

## Module
module is created using `createModule :: [(String, Int, [[Instruction]])] -> Module` function
by passing list of function descriptions. Function is described by tuple `(String, Int, [[Instruction]])` where first (`String`) element is name of the function, second (`Int`)  element is number of local storage cells  and third (`[[Instruction]]`) element is list of instruction blocks.
for example:

    testModule :: Module
    testModule = createModule [
        ("addOne", 0, [
          [Const 1, Add, Ret]
        ])
      ]

creates module with one function called addOne with 0 local storage cells and one instruction block that adds 1 to the value on the top of the stack and returns.

## Execution
VM module can be executed using `runVM :: Module -> String -> [Int] -> Maybe [Int]`
runVM takes module, function name, and initial stack, executes the function and returns resulting stack if function executed successfully. 
for example:

    runVM testModule "addOne" [1]

should return `Just [2]`

## Samples
two samples are provided Samples.hs, both calculating n-th Fibonacci number.
