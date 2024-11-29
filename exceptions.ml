let testInputEndsButExpected1 = "(if 3 < 4" 
let testInputEndsButExpected2 = "(fun"
let testTokenSeenButExpected1 = "(match x of"
let testTokenSeenButExpected2 = "(fun x ->"
let testExtraInput = "(3 + 4) 5"

let testNotDeclared = "(3 + y)" 

let testOperandNotIntegerType1 = "(if (fun x : int -> x) < 7 then 8 else 9)"
let testOperandNotIntegerType2 = "(3 + (fun y : int -> y))"
let testFunNotFunType = "(3 4)"
let testFunMismatchType = "((fun x : int -> x) (fun y : int -> y))" 
let testBranchesMismatch = "(if 3 < 4 then 7 else (fun y : int -> y))" 
let testFstNotPairType = "(fst 7)"
let testSndNotPairType = "(snd 8)"
let testConstructorMismatch =
       "type t = | C of int | D of (int * int)
        (D 7)"
let testMatchWithNotSumType = 
       "type t = | C of int 
        (match 7 with | C x -> 8)"  
let testMatchNoClauses = 
       "type t = | C of int 
        (match (C 7) with)"
let testClauseNotMatch =
       "type t = | C of int 
        type u = | D of int
        (fun x : t -> (match x with | D y -> 7))" 
let testClausesMismatch =
       "type t = | C of int | D of int
        (match (C 7) with | C x -> 8 | D x -> (fun y : int -> y))"
let testRecConflictType = "let rec f (x : int) = (f : int) ;; 7"

let testOperandNotInteger1 = "(if (fun x : int -> x) < 7 then 8 else 9)"
let testOperandNotInteger2 = "(3 + (fun y : int -> y))"
let testApplyNotClosure = "(3 4)"
let testFstNotPair = "(fst 7)"
let testSndNotPair = "(snd 8)"
let testMatchWithNotConstructor =
      "type t = | C of int
       (match 7 with | C x -> 8)" 
let testMatchNotFound = 
      "type t = | C of int | D of int 
       (match (C 7) with | D x -> x)" 
let testOutputClosure = "(fun x : int -> x)"  


