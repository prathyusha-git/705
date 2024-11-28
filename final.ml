(* === Parser, Type Checker and Interpreter for the language (subset of OCaml)
     given as graduate credit project in 
          CIS705, Kansas State University, Fall 2024
*)

(* -- CONCRETE SYNTAX

- An identifier I is any sequence of letters, digits, and underscores that starts with a lowercase letter.

- A constructor C is any sequence of letters, digits, and underscores that starts with an uppercase letter.

- A number N is a sequence of digits.

- Types T are given by the syntax

  T ::= int
     |  I
     | (T * T)
     | (T -> T)

- Type declarations D are of the form

  type I = S

where S is a sequence of clauses of the form
     | C of T

- Expressions E are given by the syntax

  E ::= N
     |  I
     | (fun I:T -> E)
     | (E E)
     | (if E relop E then E else E)
     | (E op E)
     | (E , E)
     | (fst E)
     | (snd E)
     | (C E)
     | (match E with B)

where B is a sequence of clauses of the form
   | C I -> E

and a binary operator op is either 
   '+', '*' or '-'
and a relational operator relop is either
   '=' or '<'.

- Let bindings L are given by the syntax

L ::= let x = E
   |  let f (x : T) = E
   |  let rec f (x : T) = (E : T)

- A Program P is of the form (with n,m >= 0)

P : 
     D1 
     ...
     Dn 
     L1 ;;
     ...
     Lm ;;
     E

*)

(* -- EXCEPTIONS *)

(* for parsing *)
exception InputEndsButExpected of string
exception TokenSeenButExpected of string * string
exception ExtraInput
exception ParserIncomplete

(* for typing and interpretation *)
exception NotDeclared of string

(* for typing *)
exception OperandNotIntegerType
exception FunNotFunType
exception FunMismatchType
exception BranchesMismatch
exception FstNotPairType
exception SndNotPairType
exception ConstructorMismatch
exception MatchWithNotSumType
exception MatchNoClauses
exception ClauseNotMatch
exception ClausesMismatch
exception RecConflictType
exception TypeCheckerIncomplete

(* for interpretation *)
exception OperandNotInteger
exception ApplyNotClosure
exception FstNotPair
exception SndNotPair
exception MatchWithNotConstructor
exception MatchNotFound
exception OutputClosure
exception InterpreterIncomplete

(* -- ABSTRACT SYNTAX *)

type identifier = string

type constructor = string

type typeY =
  | IntY
  | DeclaredY of identifier
  | ProdY of typeY * typeY
  | FunY of typeY * typeY

let rec print_type : typeY -> string =
  fun t ->
   match t with
   | IntY -> "int"
   | DeclaredY s -> s
   | ProdY (t1,t2) ->
       "("^(print_type t1)^" * "^(print_type t2)^")"
   | FunY (t1,t2) ->
       "("^(print_type t1)^" -> "^(print_type t2)^")"

type declareD =
      identifier *
    (constructor * typeY) list

type expE =
  | NumE of int
  | IdE of identifier
  | FunE of identifier * typeY * expE
  | ApplyE of expE * expE
  | IfEqE of expE * expE * expE * expE
  | IfLtE of expE * expE * expE * expE
  | PlusE of expE * expE
  | MinusE of expE * expE
  | TimesE of expE * expE
  | PairE of expE * expE
  | FstE of expE
  | SndE of expE
  | ConstructE of constructor * expE
  | MatchE of expE * clauseC list

and clauseC = constructor * identifier * expE

type letL =
  | LetVarL of identifier * expE
  | LetFunL of identifier * identifier * typeY * expE
  | LetRecL of identifier * identifier * typeY * expE * typeY

type progP =
    (declareD list)
  * (letL list)
  * expE

(* -- SCANNER
    converts the input string into a list of "tokens" *)

type tokenT = 
 | PlusT
 | MinusT
 | TimesT
 | EqualT
 | LessT
 | LparenT
 | RparenT
 | VbarT
 | ArrowT
 | IntT
 | TypeT
 | OfT
 | FunT
 | ColonT
 | IfT
 | ThenT
 | ElseT
 | CommaT
 | FstT
 | SndT
 | MatchT
 | WithT
 | LetT
 | RecT
 | InT
 | SemisT
 | ConstructT of constructor
 | IdT of identifier
 | NumT of int

let print_token token = match token with
 | PlusT -> "+"
 | MinusT -> "-"
 | TimesT -> "*"
 | EqualT -> "="
 | LessT -> "<"
 | LparenT -> "("
 | RparenT -> ")"
 | VbarT -> "|"
 | ArrowT -> "->"
 | IntT -> "int"
 | TypeT -> "type"
 | OfT -> "of"
 | FunT -> "fun"
 | ColonT -> ":"
 | IfT -> "if"
 | ThenT -> "then"
 | ElseT -> "else"
 | CommaT -> ","
 | FstT -> "fst"
 | SndT -> "snd"
 | MatchT -> "match"
 | WithT -> "with"
 | LetT -> "let"
 | RecT -> "rec"
 | InT -> "in"
 | SemisT -> ";;"
 | (ConstructT id) -> ("constructor "^id)
 | (IdT id) -> ("identifier "^id)
 | (NumT n) -> "number"

let is_digit(ch) = 
   Char.code ch >= Char.code '0' && Char.code ch <= Char.code '9'

let char2digit(ch) = Char.code ch - Char.code '0'

let is_lower_case(ch) = 
   (Char.code ch >= Char.code 'a' && Char.code ch <= Char.code 'z')

let is_upper_case(ch) = 
   (Char.code ch >= Char.code 'A' && Char.code ch <= Char.code 'Z')

let is_letter(ch) = 
     is_lower_case(ch)
  || is_upper_case(ch)

let is_next : string -> char -> bool = fun str -> fun ch ->
  if str = ""
  then false
  else if String.get str 0 = ch
  then true
  else false

let scanNum : string -> (int * string) = fun str ->
  let rec get_num acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_digit c
      then get_num (10 * acc + (char2digit c)) str' 
      else (acc, str)
 in get_num 0 str

let scanName : string -> (string * string) = fun str ->
  let rec get_id acc str = 
    if str = "" 
    then (acc, str)
    else 
      let c = String.get str 0 and 
          str' = String.sub str 1 (String.length str - 1) in
      if is_letter c || is_digit c || c = '_' 
      then get_id (acc ^ (String.make 1 c)) str'
      else (acc, str)
 in get_id "" str

let rec scan : string -> tokenT list = 
  fun str -> 
   if str = ""
   then []
   else let c = String.get str 0 
        and str1 = String.sub str 1 (String.length str - 1) in
  if c = '\n' then
      scan str1
   else if is_digit c
   then let (n,str') = scanNum str
         in (NumT n :: (scan str'))
   else if is_upper_case c
   then let (s,str') = scanName str
     in (ConstructT s :: (scan str'))
   else if is_lower_case c
   then let (s,str') = scanName str
     in let token =
       if s = "int" then IntT
       else if s = "type" then TypeT
       else if s = "of" then OfT
       else if s = "fun" then FunT
       else if s = "if" then IfT
       else if s = "then" then ThenT
       else if s = "else" then ElseT
       else if s = "fst" then FstT
       else if s = "snd" then SndT
       else if s = "match" then MatchT
       else if s = "with" then WithT
       else if s = "let" then LetT
       else if s = "rec" then RecT
       else if s = "in" then InT
       else IdT s
     in (token :: scan str')
   else match c with
     | '+' -> PlusT :: (scan str1)
     | '*' -> TimesT :: (scan str1)
     | '=' -> EqualT :: (scan str1)
     | '<' -> LessT :: (scan str1)
     | '(' -> LparenT :: (scan str1)
     | ')' -> RparenT :: (scan str1)
     | '|' -> VbarT :: (scan str1)
     | ':' -> ColonT :: (scan str1)
     | ',' -> CommaT :: (scan str1)
     | '-' -> if is_next str1 '>'
              then ArrowT :: (scan (String.sub str1 1 (String.length str1 - 1)))
              else MinusT :: (scan str1)
     | ';' -> if is_next str1 ';'
              then SemisT :: (scan (String.sub str1 1 (String.length str1 - 1)))
              else scan str1
     | _ -> scan str1

(* -- PARSER *)

let expectToken : tokenT -> tokenT list -> tokenT list = 
  fun expected tokens -> 
   match tokens with
   |  [] -> raise (InputEndsButExpected (print_token expected))
   | token1 :: tokens' -> 
       if token1 = expected
       then (
         print_endline ("Matched token: " ^ print_token token1);
         tokens'
       )
       else raise (TokenSeenButExpected (print_token expected, print_token token1))

let getIdT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise (InputEndsButExpected "an identifier")
   | (IdT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (TokenSeenButExpected ("an identifier", print_token token))

let getConstructT : tokenT list -> string * tokenT list =
  fun tokens -> 
   match tokens with
   | [] -> raise (InputEndsButExpected "a constructor")
   | (ConstructT id) :: tokens' -> (id, tokens')
   | (token :: _) -> 
       raise (TokenSeenButExpected ("a constructor", print_token token))

let rec parseType : tokenT list -> typeY * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "a type")
    | (IntT :: tokens1) ->
         (IntY, tokens1)
    | (IdT id :: tokens1) ->
         (DeclaredY id, tokens1)
    | (LparenT :: tokens1) ->
         let (typ1, tokens2) = parseType tokens1 in
        (match tokens2 with
         | [] -> raise (InputEndsButExpected "'*' or '->'")
         | (TimesT :: tokens3) -> 
            let (typ2, tokens4) = parseType tokens3 in
            (ProdY (typ1, typ2), expectToken RparenT tokens4)
         | (ArrowT :: tokens3) ->
               let (typ2, tokens4) = parseType tokens3 in
                  (FunY (typ1,typ2), expectToken RparenT tokens4)
         | (token :: _) -> raise (TokenSeenButExpected ("'*' or '->'", print_token token)))
    | (token :: _) -> raise (TokenSeenButExpected ("a type", print_token token))

let rec parseConstructors : 
      tokenT list -> ((constructor * typeY) list) * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> ([], tokens)
    | (VbarT :: tokens1) ->
         let (c1,tokens2) = getConstructT tokens1 in
         let (typ1, tokens3) = parseType (expectToken OfT tokens2) in
         let (constrs, tokens4) = parseConstructors tokens3 in
        (((c1,typ1) :: constrs), tokens4)
    | _ -> ([], tokens)

let parseDecl : tokenT list -> declareD * tokenT list =
          (* called after the 'type' has been parsed *)
   fun tokens ->
      let (id, tokens1) = getIdT tokens in
      let (constrs, tokens2) = parseConstructors (expectToken EqualT tokens1) in
     ((id, constrs), tokens2)

let rec parseDecls : tokenT list -> declareD list * tokenT list =
   fun tokens ->
     match tokens with
     | [] -> ([], tokens)
     | (TypeT :: tokens1) ->
           let (decl, tokens2) = parseDecl tokens1 in
           let (decls, tokens3) = parseDecls tokens2 in
          (decl :: decls, tokens3)
     | _ -> ([], tokens)

let rec parseExp : tokenT list -> expE * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "an expression")
    | (NumT z) :: tokens1 ->
         (NumE z, tokens1)
    | (IdT s) :: tokens1 ->
         (IdE s, tokens1)
    | LparenT :: tokens1 ->
         (match tokens1 with
          | FunT :: tokens2 ->
             let (x, tokens3) = getIdT tokens2 in
             let (t, tokens4) = parseType (expectToken ColonT tokens3) in
             let (e, tokens5) = parseExp (expectToken ArrowT tokens4) in
            (FunE (x,t,e), expectToken RparenT tokens5)
         | IfT :: tokens2 -> 
            let (cond1, tokens3) = parseExp tokens2 in
             (match tokens3 with
              | EqualT :: tokens4 ->
                  let (cond2, tokens5) = parseExp tokens4 in
                  let tokens6 = expectToken ThenT tokens5 in
                  let (e1, tokens7) = parseExp tokens6 in
                  let tokens8 = expectToken ElseT tokens7 in
                  let (e2, tokens9) = parseExp tokens8 in
                  (IfEqE (cond1, cond2, e1, e2), tokens9)
              | LessT :: tokens4 -> 
                  let (cond2, tokens5) = parseExp tokens4 in
                  let tokens6 = expectToken ThenT tokens5 in
                  let (e1, tokens7) = parseExp tokens6 in
                  let tokens8 = expectToken ElseT tokens7 in
                  let (e2, tokens9) = parseExp tokens8 in
                  (IfLtE (cond1, cond2, e1, e2), tokens9)
              | RparenT :: _ -> 
                  raise (InputEndsButExpected "else")
              | token :: _ -> raise (TokenSeenButExpected ("'=' or '<'", print_token token))
              | [] -> raise (InputEndsButExpected "'=' or '<'"))
         | (FstT :: tokens2) -> 
              let (e1, tokens3) = parseExp tokens2 in
              (FstE e1, expectToken RparenT tokens3)
         | (SndT :: tokens2) -> 
              let (e1, tokens3) = parseExp tokens2 in
              (SndE e1, expectToken RparenT tokens3)
         | (ConstructT c :: tokens2) -> 
              let (e1, tokens3) = parseExp tokens2 in
              (ConstructE (c, e1), expectToken RparenT tokens3)
         | (MatchT :: tokens2) -> 
             let (e1, tokens3) = parseExp tokens2 in
             let tokens4 = expectToken WithT tokens3 in
             let rec parseClauses tokens =
               match tokens with
               | VbarT :: tokens1 ->
                   let (c, tokens2) = getConstructT tokens1 in
                   let (x, tokens3) = getIdT tokens2 in
                   let tokens4 = expectToken ArrowT tokens3 in
                   let (e, tokens5) = parseExp tokens4 in
                   let (rest, tokens6) = parseClauses tokens5 in
                   ((c, x, e) :: rest, tokens6)
               | _ -> ([], tokens)
             in
             let (clauses, tokens5) = parseClauses tokens4 in
             (MatchE (e1, clauses), expectToken RparenT tokens5)
          | _ -> 
            let (e1, tokens2) = parseExp tokens1 in
            (match tokens2 with
            | [] -> raise (InputEndsButExpected "the second operand")
            | (PlusT :: tokens3) -> 
                let (e2, tokens4) = parseExp tokens3 in
                (PlusE (e1, e2), expectToken RparenT tokens4)
            | (MinusT :: tokens3) -> 
                let (e2, tokens4) = parseExp tokens3 in
                (MinusE (e1, e2), expectToken RparenT tokens4)
            | (TimesT :: tokens3) -> 
                let (e2, tokens4) = parseExp tokens3 in
                (TimesE (e1, e2), expectToken RparenT tokens4)
            | (CommaT :: tokens3) -> 
                let (e2, tokens4) = parseExp tokens3 in
                (PairE (e1, e2), expectToken RparenT tokens4)
            | _ ->
                  let (e2, tokens3) = parseExp tokens2
                  in (ApplyE (e1,e2), expectToken RparenT tokens3)))
        | token :: _ -> raise (TokenSeenButExpected 
                          ("the start of an expression", print_token token))


and parseClauses : 
      tokenT list -> (clauseC list) * tokenT list =
   fun tokens ->
    match tokens with
    | [] -> ([], tokens)
    | (VbarT :: tokens1) -> 
        let (c, tokens2) = getConstructT tokens1 in  
        let (x, tokens3) = getIdT tokens2 in 
        let tokens4 = expectToken ArrowT tokens3 in  
        let (e, tokens5) = parseExp tokens4 in 
        let (rest, tokens6) = parseClauses tokens5 in  
        ((c, x, e) :: rest, tokens6)
    | _ -> ([], tokens)

let rec parseBinding : tokenT list -> letL * tokenT list =
          (* called after the 'let' has been parsed *)
   fun tokens ->
    match tokens with
    | [] -> raise (InputEndsButExpected "a binding")
    | (IdT x) :: EqualT :: tokens1 -> 
           let (e, tokens2) = parseExp tokens1 in
            (LetVarL (x, e), tokens2)
    | (IdT f) :: LparenT :: (IdT x) :: ColonT :: tokens1 ->
          let (t, tokens2) = parseType tokens1 in
          let (e, tokens3) = parseExp (expectToken RparenT (expectToken EqualT tokens2)) in
          (LetFunL (f, x, t, e), tokens3)
    | RecT :: (IdT f) :: LparenT :: (IdT x) :: ColonT :: tokens1 ->
         let (t1, tokens2) = parseType tokens1 in
         let (e, tokens3) = parseExp 
                              (expectToken LparenT
                                (expectToken EqualT 
                                  (expectToken RparenT tokens2))) in
         let (t2, tokens4) = parseType (expectToken ColonT tokens3) in
       (LetRecL (f, x, t1, e, t2), expectToken RparenT tokens4)
    | (token :: _) -> raise (TokenSeenButExpected ("a binding", print_token token))

let rec parseBindings : tokenT list -> letL list * tokenT list =
   fun tokens ->
     match tokens with
     | [] -> ([], tokens)
     | (LetT :: tokens1) -> 
        let (binding, tokens2) = parseBinding tokens1 in
        let (bindings, tokens3) = parseBindings (expectToken SemisT tokens2) in
        (binding :: bindings, tokens3)
     | _ -> ([], tokens)

let parseProg : tokenT list -> progP * tokenT list =
   fun tokens ->
     let (decls, tokens1) = parseDecls tokens in
     let (bindings, tokens2) = parseBindings tokens1 in
     let (main, tokens3) = parseExp tokens2 in
   ((decls, bindings, main), tokens3)

let parse : string -> progP =
  fun input_string ->
    let tokens = scan input_string in
    let (prog,tokens1) = parseProg tokens
    in if tokens1 = []
       then prog
       else raise ExtraInput

(* -- ENVIRONMENTS *)

type 'a environment = string -> 'a

let initEnv : 'a environment = 
  fun id -> raise (NotDeclared id)

let insertEnv : string -> 'a -> 'a environment -> 'a environment =
  fun new_str a env ->
    fun str -> if str = new_str then a else env str

let retrieveEnv : 'a environment -> string -> 'a =
  fun env str -> env str

(* -- TYPING *)

type constructorEnv = (identifier * typeY) environment

let updateCEnv : declareD -> constructorEnv -> constructorEnv =
   fun (tid, constrs) -> fun cenv0 ->
      List.fold_right 
          (fun (c1,t1) -> fun cenv ->
               insertEnv c1 (tid, t1) cenv)
          constrs
          cenv0
 
let updatesCEnv : declareD list -> constructorEnv -> constructorEnv =
   fun decls -> fun cenv0 ->
     List.fold_left (fun cenv decl -> updateCEnv decl cenv) cenv0 decls

let rec typeE : expE -> constructorEnv -> (typeY environment) -> typeY =
   fun exp cenv tenv -> 
    match exp with
   | NumE n -> IntY
   | IdE x -> retrieveEnv tenv x
   | FunE (x, tx, e) ->
        let t = typeE e cenv (insertEnv x tx tenv) in
       FunY(tx, t)
   | ApplyE (e1, e2) -> 
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
      (match t1 with
       | FunY(t0,t) -> 
            if t2 = t0 then t else raise FunMismatchType
       | _ -> raise FunNotFunType)
  | IfEqE(e0L,e0R,e1,e2) -> 
        let t0L = typeE e0L cenv tenv in
        let t0R = typeE e0R cenv tenv in
        if t0L <> IntY || t0R <> IntY then raise OperandNotIntegerType;
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        if t1 = t2 then t1 else raise BranchesMismatch

  | IfLtE(e0L,e0R,e1,e2) -> 
        let t0L = typeE e0L cenv tenv in
        let t0R = typeE e0R cenv tenv in
        if t0L <> IntY || t0R <> IntY then raise OperandNotIntegerType;
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        if t1 = t2 then t1 else raise BranchesMismatch
  | PlusE(e1,e2) -> 
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        if t1 = IntY && t2 = IntY then IntY else raise OperandNotIntegerType
  | MinusE(e1,e2) -> 
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        if t1 = IntY && t2 = IntY then IntY else raise OperandNotIntegerType
  | TimesE(e1,e2) -> 
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        if t1 = IntY && t2 = IntY then IntY else raise OperandNotIntegerType
        
  | PairE(e1,e2) -> 
        let t1 = typeE e1 cenv tenv in
        let t2 = typeE e2 cenv tenv in
        ProdY (t1, t2)
  | FstE(e0) -> 
        (match typeE e0 cenv tenv with
         | ProdY (t1, _) -> t1
         | _ -> raise FstNotPairType)
  | SndE(e0) -> 
        (match typeE e0 cenv tenv with
         | ProdY (_, t2) -> t2
         | _ -> raise SndNotPairType)
  | ConstructE(c,e0) -> 
      let (tid, t_expected) = retrieveEnv cenv c in
        let t_actual = typeE e0 cenv tenv in
        if t_actual = t_expected then DeclaredY tid
        else raise ConstructorMismatch
  | MatchE(e,clauses) ->
      let t_e = typeE e cenv tenv in
        (match t_e with
         | DeclaredY tid ->
              let rec check_clauses clauses =
                match clauses with
                | [] -> raise MatchNoClauses
                | (c, x, e1) :: rest ->
                     let (tid', t_arg) = retrieveEnv cenv c in
                     if tid <> tid' then raise ClauseNotMatch;
                     let t_body = typeE e1 cenv (insertEnv x t_arg tenv) in
                     match rest with
                     | [] -> t_body
                     | _ ->
                          let t_rest = check_clauses rest in
                          if t_body = t_rest then t_body
                          else raise ClausesMismatch
              in check_clauses clauses
         | _ -> raise MatchWithNotSumType)

let typeL : 
   letL -> constructorEnv -> (typeY environment) -> (typeY environment) =
  fun binding cenv tenv ->
    match binding with
    | LetVarL(x,e) -> 
        let t = typeE e cenv tenv in
        insertEnv x t tenv
    | LetFunL(f,x,tx,e) ->
        let t1 = typeE e cenv (insertEnv x tx tenv) in
           insertEnv f (FunY (tx, t1)) tenv 
    | LetRecL(f,x,tx,e,t) -> 
        let rec_tenv = insertEnv f (FunY(tx, t)) (insertEnv x tx tenv) in
        let t_body = typeE e cenv rec_tenv in
        if t_body = t then
          insertEnv f (FunY(tx, t)) tenv
        else
          raise RecConflictType

let typeLs :
   letL list -> constructorEnv -> (typeY environment) -> (typeY environment) =
  fun decls cenv tenv0 ->
    List.fold_left
        (fun tenv -> fun decl ->
             typeL decl cenv tenv)
        tenv0
        decls

let typeP : progP -> typeY =
   fun (decls, bindings, main) ->
     let cenv = updatesCEnv decls initEnv in
     let tenv = typeLs bindings cenv initEnv in
    typeE main cenv tenv

(* -- VALUES *)

type value =
   NumV of int
 | PairV of value * value
 | ConstructV of constructor * value
 | ClosureV of identifier * expE * value environment
 | FixV of identifier * identifier * expE * value environment

let rec print_value : value -> string =
  fun v -> 
   match v with
   | NumV v -> string_of_int v
   | PairV(v1,v2) ->
         "( "^(print_value v1)^" , "^(print_value v2)^" )"
   | ConstructV(c,v0) ->
         c^" "^(print_value v0)
   | ClosureV _ -> raise OutputClosure
   | FixV _ -> raise OutputClosure

(* -- EVALUATING *)

let rec evalE : expE -> (value environment) -> value =
  fun exp env ->
   match exp with
   | NumE n -> NumV n
   | IdE x -> retrieveEnv env x
   | FunE (x, tx, exp0) -> ClosureV (x, exp0, env)
   | ApplyE(exp1, exp2) -> 
       (match (evalE exp1 env, evalE exp2 env) with
         | (ClosureV(x,exp0,env0), v2) ->
               evalE exp0 (insertEnv x v2 env0)
         | (FixV(f,x,exp0,env0), v2) ->
               evalE exp0
                 (insertEnv f (FixV (f,x,exp0,env0)) 
                    (insertEnv x v2 env0))
         | _ -> raise ApplyNotClosure)
   | IfEqE(e0L,e0R,e1,e2) -> 
      (match (evalE e0L env, evalE e0R env) with
        | (NumV v0L, NumV v0R) -> if v0L = v0R then evalE e1 env else evalE e2 env
        | _ -> raise OperandNotInteger)
   | IfLtE(e0L,e0R,e1,e2) -> 
      (match (evalE e0L env, evalE e0R env) with
        | (NumV v0L, NumV v0R) -> if v0L < v0R then evalE e1 env else evalE e2 env
        | _ -> raise OperandNotInteger)
   | PlusE(e1,e2) -> 
      (match (evalE e1 env, evalE e2 env) with
        | (NumV v1, NumV v2) -> NumV (v1 + v2)
        | _ -> raise OperandNotInteger)
   | MinusE(e1,e2) -> 
      (match (evalE e1 env, evalE e2 env) with
        | (NumV v1, NumV v2) -> NumV (v1 - v2)
        | _ -> raise OperandNotInteger)
   | TimesE(e1,e2) -> 
      (match (evalE e1 env, evalE e2 env) with
        | (NumV v1, NumV v2) -> NumV (v1 * v2)
        | _ -> raise OperandNotInteger)
   | PairE(e1,e2) -> 
      PairV (evalE e1 env, evalE e2 env)
   | FstE(e0) -> 
      (match evalE e0 env with
        | PairV (v1, _) -> v1
        | _ -> raise FstNotPair)
   | SndE(e0) -> 
      (match evalE e0 env with
        | PairV (_, v2) -> v2
        | _ -> raise SndNotPair)
   | ConstructE(c,e0) ->
      let v0 = evalE e0 env in
          ConstructV (c, v0)
   | MatchE(e0,clauses) -> 
      let v0 = evalE e0 env in
          (match v0 with
              | ConstructV (c,v) ->
                  let rec find_match clauses = 
                      match clauses with
                      | [] -> raise MatchNotFound
                      | (c', x, e1) :: rest ->
                          if c = c' then evalE e1 (insertEnv x v env)
                          else find_match rest
                      in find_match clauses
              | _ -> raise MatchWithNotConstructor)

let rec eval_binding : letL -> value environment -> value environment =
   fun binding env ->
    match binding with
    | LetVarL(x,e) ->
         insertEnv x (evalE e env) env
    | LetFunL(f,x,tx,e) -> 
        (*Simple Let*)
        let closure = ClosureV(x, e, env) in
        insertEnv f closure env
    | LetRecL(f,x,tx,e,t) -> 
        (*Recursive Let*)
        let rec_closure = FixV(f, x, e, env) in
        insertEnv f rec_closure env

let rec eval_bindings : letL list -> value environment -> value environment =
  fun bindings env ->
    match bindings with
    | [] -> env
    | (binding :: bindings') ->
         eval_bindings bindings' (eval_binding binding env)

let rec evalP : progP -> value =
   fun (_, bindings, main) -> 
  evalE main (eval_bindings bindings initEnv)

let run input = 
 try (
  let p = parse input in 
  let t = typeP p in
  let _ = print_string ("Type is: "^(print_type t)^"\n") in
  let v = evalP p in
 print_value v )
  with
   | InputEndsButExpected s ->
       "input ends though had expected '"^s^"'\n"
   | TokenSeenButExpected (s1,s2) ->
       "saw '"^s2^"' but had expected '"^s1^"'\n"
   | ExtraInput ->
       "input continues after program is parsed\n"
   | ParserIncomplete ->
       "the parser doesn't yet handle this construct\n"
   | NotDeclared s ->
        "identifier "^s^" used but not declared\n"
   | OperandNotIntegerType ->
       "operand not of integer type\n"
   | FunNotFunType ->
       "function part of application not of function type\n"
   | FunMismatchType ->
       "argument type does not match function type\n"
   | BranchesMismatch ->
       "types of branches in 'if' expression do not match\n"
   | FstNotPairType ->
       "argument to 'fst' not of product type\n"
   | SndNotPairType ->
       "argument to 'snd' not of product type\n"
   | ConstructorMismatch ->
       "argument to constructor not of appropriate type\n"
   | MatchWithNotSumType ->
       "what is being matched is not of a sum type\n"
   | MatchNoClauses ->
       "match expression has no clauses\n"
   | ClauseNotMatch ->
       "constructor in branch does not form type of matched expression\n"
   | ClausesMismatch ->
       "types of branches do not match\n"
   | RecConflictType ->
       "body of recursive function not of declared type\n"
   | TypeCheckerIncomplete ->
       "the type checker doesn't yet handle this construct\n"
   | OperandNotInteger ->
       "operand not integer\n"
   | ApplyNotClosure ->
       "function part of application does not evaluate to closure\n"
   | FstNotPair ->
       "argument to 'fst' not a pair\n"
   | SndNotPair ->
       "argument to 'snd' not a pair\n"
   | MatchWithNotConstructor ->
       "what is being matched is not formed by constructor\n"
   | MatchNotFound ->
       "match fails: no case for the given constructor\n"
   | OutputClosure ->
       "a closure is part of what is being returned\n"
   | InterpreterIncomplete ->
       "the interpreter doesn't yet handle this construct\n"

