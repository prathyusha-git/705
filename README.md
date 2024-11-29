Design :
Project 4 is done and good.....

Next to do:
Separate the tasks and the function to finish 

Additionally add something more to the final project skeleton.

We want to add prolog basic features to the project

How to assign the environment and the store to the logical variables and expression 

Test:
We can use torben examples.ml to test case...
We can use the test cases from project5 to evaluate the logical programming paradigm.

Timeline

Future Work: To create a hybrid language which combines the imperative, the functional and logic programming paradigm.


Passed Test cases:
testBinOp
testCond
testLet
testLetFun
testFun
testPairs
testTree
testRecTree1
testRecTree2

Unpassed Test cases:

testRecNat
Scanned identifier/keyword token: let
Scanned identifier/keyword token: rec
Scanned identifier/keyword token: fac
Scanned '(' token
Scanned identifier/keyword token: n
Scanned ':' token
Scanned identifier/keyword token: int
Scanned ')' token
Scanned '=' token
Scanned '(' token
Scanned '(' token
Scanned identifier/keyword token: if
Scanned '<' token
Scanned identifier/keyword token: n
Scanned identifier/keyword token: then
Scanned '(' token
Scanned identifier/keyword token: n
Scanned '*' token
Scanned '(' token
Scanned identifier/keyword token: fac
Scanned '(' token
Scanned identifier/keyword token: n
Scanned '-' token
Scanned ')' token
Scanned ')' token
Scanned ')' token
Scanned identifier/keyword token: else
Scanned ')' token
Scanned ':' token
Scanned identifier/keyword token: int
Scanned ')' token
Scanned '(' token
Scanned identifier/keyword token: fac
Scanned ')' token
Matched token: )
Matched token: =
Parsed token: (
Parsed token: (
Parsed token: if
Parsed token: number
Parsed condition of if
Parsed token: <
Parsed token: identifier n
Matched token: then
Parsed token: (
Parsed token: identifier n
Parsed expression inside parentheses
Parsed token: *
Parsed token: (
Parsed token: identifier fac
Parsed expression inside parentheses
Parsed token: (
Parsed token: identifier n
Parsed expression inside parentheses
Parsed token: -
Parsed token: number
Matched token: )
Matched token: )
Matched token: )
Matched token: else
Parsed token: number
Matched token: )
Parsed if-then-else block
Parsed expression inside parentheses
- : string = "saw ':' but had expected 'the start of an expression'\n"


Issue: The problem of nested right parentheses
