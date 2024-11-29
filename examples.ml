let testBinOp = 
   "((7 - 4) * (9 - 1))"
(*
# run testBinOp ;;
Type is: int
- : string = "24"
*)
  
let testCond = 
   "(if 2 < 3 
     then (if 5 < 4 
           then 1 
           else (if 9 = 8 
                 then 2 
                 else (if 8 = 8 
                       then 3 
                       else 4)))
     else 5)"
(*
# run testCond ;;
Type is: int
- : string = "3"
*)

let testPairs =
  "((fst (8,5)) + (snd (6,7)))"
(*
# run testPairs ;;
Type is: int
- : string = "15"
*)

let testLet =
  "let x = 7 ;; let y = (x * 4) ;; (x + y)"
(*
# run testLet ;;
Type is: int
- : string = "35"
*)

let testLetFun =
   "let four = 4 ;;
    let five = 5 ;;
    let add4 (x : int) = (x + four) ;;
    let mul5 (y : int) = (y * five) ;;
   (mul5 (add4 6))"
(*
# run testLetFun ;;
Type is: int
- : string = "50"
*)

let testFun =
   "let twice (f : (int -> int)) = (fun x : int -> (f (f x))) ;;
    let mult2 (x : int) = (x * 2) ;;
     ((twice mult2) 3)"
(*
# run testFun ;;
Type is: int
- : string = "12"
*)

let testRecNat = 
   "let rec fac (n:int) = 
        ((if 0 < n then (n * (fac (n - 1))) else 1) : int) ;;
     (fac 6)"
(*
# run testRecNat ;;
Type is: int
- : string = "720"
*)

let testTree =
   "type tree = | Leaf of int | Node of (tree * tree)
    let t = (Node ((Leaf 27), (Leaf 8))) ;;
    (match t with
    | Node pr -> (match (snd pr) with
                   | Leaf x -> (x + 1)
                   | Node x -> 27)
    | Leaf y -> 35)"
(*
# run testTree ;;
Type is: int
- : string = "9"
*)

let testRecTree1 =
   "type tree = | Leaf of int | Node of (tree * tree)
    let rec add (t : tree) = 
       ((match t with 
          | Leaf n -> n
          | Node t1 ->  ((add (fst t1)) + (add (snd t1)))) : int) ;;
    let n1 = 3 ;;
    let n2 = 5 ;;
   (add (Node ((Node ((Leaf n1), (Leaf 9))), (Leaf n2))))"
(*
# run testRecTree1 ;;
Type is: int
- : string = "17"
*)

let testRecTree2 =
   "type tree = | Leaf of int | Node of (tree * tree)
    let rec map (ft : ((int -> int) * tree)) = 
      ((match (snd ft) with 
         | Leaf n -> (Leaf ((fst ft) n))
         | Node t0 -> (Node ((map ((fst ft), (fst t0))), 
                          (map ((fst ft), (snd t0))))))
        : tree) ;;
   (map ((fun n : int -> (n + 1)), 
         (Node ((Leaf 2), (Leaf 4)))))"
 
(*
# run testRecTree2 ;;
Type is: tree
- : string = "Node (Leaf 3 , Leaf 5 )"
*)

