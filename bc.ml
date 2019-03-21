open Core
(* Input expression or list of expressions *)
type sExpr = 
    | Atom of string
    | List of sExpr list
(* Top level expression dealing with functions, operators, variables numbers *)
type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list


(* Statement handles our key words that have blocks or require special handling *)
type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

(* A block of code that requires its own environment for local variables *)
type block = statement list 

(* Environment to store variables in *)
type env = (string*float) list

(* List of Functions *)
type fctEnv = (string * string list * statement list  ) list

(* Queue of Environments to handle nested functions/recursion*)
type envQueue = env list

(* Prints anyt list passed into it it would seem. From head to tail. Expects each index to have two variables. *)
let printTupleList l = match l with
    | [] -> ()
    | h::t -> let (x,y) = h in
            printf "%s %f, " x y

(* Prints the environment queue *)
let rec printQueue que = match que with
    | [] -> printf "Nothing man"; ()
    | h::t -> printTupleList h; printf "Something\n" ; printQueue t


(* Find variable *)
let rec find_var_tuple_list var l =
    match l with
            [] -> None
            |(s, i)::tl -> if s = var then Some i
                                  else find_var_tuple_list var tl

(* Evaluates a variable *)
let rec varEval (_v: string) (_q:envQueue): float  =
    printf "Finding: %s" _v;
    printQueue _q;
    match List.hd _q with
    | Some h -> (
            match find_var_tuple_list _v h with
                None -> (match List.tl _q with 
                        | None -> failwith "Not found"
                        | Some y -> varEval _v y
                        )
                | Some x -> x
        )
                
    | None -> failwith "Not found Results Variable"

(* Evaluate operators for ++ and -- *)
let eval_op1 op (e1: float): float=
    match op with 
  | "++" -> e1 +. 1.0
  | "--" -> e1 -. 1.0
  | _ -> failwith "oof"

(* Evaluate simple math operators*)
let eval_op2 op e1 e2 =
    match op with
  | "+" -> (+.) e1 e2
  | "-"-> (-.) e1 e2
  | "*" -> ( *. ) e1 e2
  | "/" -> (/.) e1 e2
  | ">" -> if (e1 > e2) then 1.0 else 0.0
  | x -> printf "%s" x; (+.) e1 e2

let rec find_fct var p l =
    match l with
        [] -> failwith "Not found fct"
        |(s, pl, bl)::tl -> if s = var & List.length p = List.length pl then (s, pl, bl)
                                else find_fct var p tl




(* Inserts a number at the end?  *)
let insert_at_end l i =
    [i]@l
  (*match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)*)

(* This just finds the head? *)
let hd l = match List.hd l with
    | Some x -> x
    | None -> failwith "oof"

(* Insert into function list *)
let decFct f fe =
    match f with
        [] -> [fe]
    | h :: t -> h :: (insert_at_end t fe)

(* Evaluates a block of code using the environment queue, returns result of the code block. First place the input is sent *)
let rec evalCode (_code: block) (_q:envQueue) (_f:fctEnv): float = 

    printf "Length of block: %d\n" (List.length _code); 
    let q = [] @ _q in 
    (*printQueue q;*)
    (* let f = [] @ _f in
    printFunc f; *)
    match _code with 
    | [] -> 0.0;
    | h::t -> let out = evalStatement h q _f in
              let (outq, outf) = out in
              evalCode t outq outf
    (* let y = List.fold_left evalStatement  [] in ()*)
    (* let x = evalStatement (hd _code) q in () *)
    (* in let (a, b) = hd (hd x) in *)
    (* printf "%s %f\n" a b *)
    (* pop the local environment *)
    (* let y = List.fold_left evalStatem *)

(* UwU whats this? a floating and statement?
It appears to take in a statement and environment queue and creates a new environment for the statement. 
And allows you to call evalCode and allows evalCode to call evalStatement. Recursively. Mutually recursive types.*)
and evalStatement (s: statement) (q:envQueue) (f:fctEnv): (envQueue*fctEnv) =
    match s with 
        | Assign(_v, _e) -> let out = evalExpr _e q f in
                            let sc = List.hd q in
                            let q = insert_at_end q [(_v, out)]
                            in (q, f)
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q f in
                if(cond>0.0) then
                    let _ = evalCode codeT q f in
                    (q,f)
                else
                    let _ = evalCode codeF q f in
                    (q, f)
        | Expr e -> let out = evalExpr e q f in
                printf "%f\n" out; (q, f)
        | For (s1, e, s2, code) -> (q, f) (* ree *)
        | FctDef (n, p, bl) -> let fe = decFct f (n, p, bl) in
                            (q, fe)
        | _ -> (q,f) (*ignore *)

(* Evaluates expressions, matching it with an associated type. *)
and evalExpr (_e: expr) (_q:envQueue) (_f:fctEnv): float  = 
    match _e with
    | Num n -> n
    | Var v -> varEval v _q
    | Op1 (op, e1) -> (eval_op1 op (evalExpr e1 _q _f))
    | Op2 (op, e1, e2) -> (eval_op2 op 
                (evalExpr e1 _q _f) 
                (evalExpr e2 _q _f))
    | Fct (n, p) -> let (nQ, nF, block) = evalFct n p _q _f in 
            evalCode block nQ nF
    | _ -> 1.5

and evalExprList (p: expr list) (r: float list) (_q:envQueue) (_f:fctEnv): float list =
    match p with 
    | [] -> [];
    | h::t -> let e = evalExpr h _q _f in 
                    printf "Param value: %f\n" e;
                    r@[e]


and assign_var_list (param_names: string list) (expr_list: expr list) (_q :envQueue) (_f: fctEnv): (envQueue*fctEnv) =
    let topq = List.hd _q in
    match param_names, expr_list with
    | [], [] -> (_q, _f)
    | hd1::tl1, hd2::tl2 -> evalStatement (Assign(hd1, hd2)) _q _f

and evalFct (_n:string) (_p: expr list) (_q:envQueue) (_f:fctEnv) : envQueue*fctEnv*block =
    match List.hd _f with
    | Some h -> let (name, params, block) = find_fct _n _p _f in 
        let nQ, nF = assign_var_list params _p _q _f in 
        (nQ, nF, block)
    | None -> failwith "Not found Funcky boi"





(* Test for expression *)
let%expect_test "evalNum" = 
    evalExpr (Op1("++", Num 1.0)) [] [] |>
    printf "%F";
    [%expect {| 2. |}]


(* p999... what a descriptive name. This is a test of the assignment operator. *)
let p999: statement = Assign("v", Num(1.0))
(*let%test _ = evalStatement p999 [] [] = ([], [[("v", 1.0)]])*)
(* 
    v = 10; 
    v // display v
 *)
 (* Test assignment function. We assign V to the number 1, then just pass v in to see if it works. 
 What we should see in our output is v 1.0, 
 1.0
 Whereby v 1.0 is when it gets set and 1.0 being when v is evaluated.  *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    let _ = evalCode p1 [] [] in (); 
    [%expect {| 1. |}]

(*
    v = 1.0;
    if (v>10.0) then
        v = v + 1.0
    else
        for(i=2.0; i<10.0; i++) {
            v = v * i
        }
    v   // display v
*)
let p2: block = [
    Assign("v", Num(1.0));
    If(
        Op2(">", Var("v"), Num(10.0)), 
        [Assign("v", Op2("+", Var("v"), Num(1.0)))], 
        [For(
            Assign("i", Num(2.0)),
            Op2("<", Var("i"), Num(10.0)),
            Expr(Op1("++", Var("i"))),
            [
                Assign("v", Op2("*", Var("v"), Var("i")))
            ]
        )]
    );
    Expr(Var("v"))
]

let%expect_test "p2" =
    let _ = evalCode p2 [] [] in (); 
    [%expect {| 3628800. |}]

(*  Fibbonaci sequence
    define f(x) {
        if (x<1.0) then
            return (1.0)
        else
            return (f(x-1)+f(x-2))
    }

    f(3)
    f(5)
 *)
let p3: block = 
    [
        FctDef("f", ["x"], [
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Return(Num(1.0))],
                [Return(Op2("+",
                    Fct("f", [Op2("-", Var("x"), Num(1.0))]),
                    Fct("f", [Op2("-", Var("x"), Num(2.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    let _ = evalCode p3 [] [] in (); 
    [%expect {| 
        2. 
        5.      
    |}]


let p4: block = 
    [
        FctDef("f", ["x"; "y"], [
            Assign("x", Op2("+", Var("x"), Num(1.0)));
            Assign("y", Op2("*", Var("y"), Num(2.0)))
        ]);
        Expr(Fct("f", [Num(1.0); Num(3.0)]))
    
    ]

let%expect_test "p4" =
    let _ = evalCode p4 [] [] in (); 
    [%expect {| 
        2. 
        6.
    |}]


