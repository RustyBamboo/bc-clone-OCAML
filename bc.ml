open Core

exception ReturnValue of float

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



(* This just finds the head? *)
let hd l = match List.hd l with
    | Some x -> x
    | None -> failwith "no head"
let tl l = match List.tl l with
    | Some x -> x
    | None -> failwith "no tail"



(* Prints anyt list passed into it it would seem. From head to tail. Expects each index to have two variables. *)
let printTupleList l = match l with
    | [] -> ()
    | h::t -> let (x,y) = h in
            printf "\t%s %f, " x y

(* Prints the environment queue *)
let rec loop_through_que que =
    printf "\n\t-------\n";
    match que with
    | [] -> ()
    | h::t -> printTupleList h; loop_through_que t

let printQueue que =
    printf "STACK\n";
    loop_through_que que;
    printf "=================\n"


(* Find variable *)
let rec find_var_tuple_list var l =
    match l with
            [] -> None
            |(s, i)::tl -> if s = var then Some i
                                  else find_var_tuple_list var tl

(* Evaluates a variable *)
let varEval (_v: string) (_q:envQueue) =
    match find_var_tuple_list _v (hd _q) with
                | None -> printf "Not found in head\n"; ( match find_var_tuple_list _v (hd (List.rev _q)) with
                    | None -> None
                    | Some x -> Some x )
                | Some x -> printf "Found in head\n"; Some (x)
                
let rec find_and_replace_var_in_env ls (x:string*float) =
    let (nname, nvalue) = x in
    match ls with
    [] -> ls
    | (name, value)::t -> if name=nname then (name, nvalue)::(find_and_replace_var_in_env t x) else (name, value)::(find_and_replace_var_in_env t x)

(* Evaluate operators for ++ and -- *)
let eval_op1 op (e1: float): float=
    match op with 
  | "++" -> e1 +. 1.0
  | "--" -> e1 -. 1.0
  | _ -> failwith "oof"

(* Evaluate simple math operators*)
let eval_op2 op e1 e2 =
    printf "DOING EVAL\n";
    match op with
  | "+" -> e1 +. e2
  | "-"-> e1 -. e2
  | "*" -> e1 *. e2
  | "/" -> e1 *. e2
  | ">" -> if (e1 > e2) then 1.0 else 0.0
  | "<" -> if (e1 < e2) then 1.0 else 0.0
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


(* Insert into function list *)
let decFct f fe =
    match f with
        [] -> [fe]
    | h :: t -> h :: (insert_at_end t fe)

let rec print_statement s =
    match s with
    | Assign _ -> printf "ASSIGN ";
    | Return _ -> printf "RETURN ";
    | Expr _ -> printf "Expr ";
    | If _ -> printf "If ";
    | While _ -> printf "While ";
    | For _ -> printf "For ";
    | FctDef _ -> printf "FctDef ";
 

and print_block b =
    match b with
    | [] -> ()
    | h :: t -> print_statement h; print_block t


(* Evaluates a block of code using the environment queue, returns result of the code block. First place the input is sent *)
let rec evalCode (_code: block) (_q:envQueue) (_f:fctEnv): float = 

        printQueue _q;
    (* let f = [] @ _f in
    printFunc f; *)
    match _code with 
    | [] -> failwith "fuck"
    | h::t -> try (let out = evalStatement h _q _f in
              let (outq, outf) = out in
              evalCode t outq outf)
    with ReturnValue x-> printf "GOT %f\n" x; x
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
        | Assign(_v, _e) -> printf "STATMENT: ASSIGN %s\n" _v; let out = evalExpr _e q f in
                            (
                             match find_var_tuple_list _v (hd q) with
                             | Some _ -> let nq = find_and_replace_var_in_env (hd q) (_v, out) in ([nq]@(tl q), f)
                             | None -> ( let revQ = List.rev q in
                                match find_var_tuple_list _v (hd revQ) with
                                | Some _ -> let nq = find_and_replace_var_in_env (hd revQ) (_v, out) in (List.rev [nq]@(tl revQ), f)

                                | None -> let nenv = insert_at_end (hd q) (_v, out) in
                                ([nenv]@(tl q), f)
                             )
                            )
        | If(e, codeT, codeF) -> printf "STATMENT IF\n"; 
            let cond = evalExpr e q f in
                if(cond>0.0) then
                    let out = evalCode codeT q f in
                    printf "FINISHED IF AND GOT %f" out;
                    (q,f)
                else
                    let _ = evalCode codeF q f in
                    printf "FINISHED ELSE\n";
                    (q, f)
        | Expr e -> let out = evalExpr e q f in
                printf "Out: %f\n" out; (q, f)
        | For (s1, e, s2, code) -> printf "STATMENT: IF\n"; (q, f) (* ree *)
        | FctDef (n, p, bl) -> printf "STATEMENT: FCTDEF\n"; 
        
        printf "BLOCK\n ";
    print_block bl;
    printf "\n=============\n";


                        let fe = decFct f (n, p, bl) in
                            (q, fe)
        | Return e -> let out = evalExpr e q f in printf "STATEMENT RETURN %f" out;
raise (ReturnValue out)
        | _ -> printf "STATEMENT: IDK\n"; (q,f) (*ignore *)

(* Evaluates expressions, matching it with an associated type. *)
and evalExpr (_e: expr) (_q:envQueue) (_f:fctEnv): float  = 
    match _e with
    | Num n -> printf "NUM"; n
    | Var v -> printf "VAR"; (match varEval v _q with 
                    | Some x ->
    printf "Var: %s = %f\n" v x; x
                    | None -> printf "ME NOT FOUND %s " v; 0.0
                )
    | Op1 (op, e1) -> printf "Op1"; (eval_op1 op (evalExpr e1 _q _f))
    | Op2 (op, e1, e2) -> printf "Op2"; (eval_op2 op 
                (evalExpr e1 _q _f) 
                (evalExpr e2 _q _f))
    | Fct (n, p) -> printf "Fct"; printf "Doing func\n"; let evaluated_params = eval_expr_to_list p _q _f in
    let q = [[]] @ _q in let (nQ, nF, block) = evalFct n evaluated_params q _f in  
    let outf = evalCode block nQ nF in printf "FINISHED AND GOT %f" outf; outf
    | _ -> 1.5

and assign_var_list (param_names: string list) (expr_list: float list) (_q :envQueue) (_f: fctEnv): (envQueue*fctEnv) =
    let topq = List.hd _q in
    match param_names, expr_list with
    | [], [] -> (_q, _f)
    | hd1::tl1, hd2::tl2 -> evalStatement (Assign(hd1, Num(hd2))) _q _f

and evalFct (_n:string) (_p: float list) (_q:envQueue) (_f:fctEnv) : envQueue*fctEnv*block =
    match List.hd _f with
    | Some h -> let (name, params, block) = find_fct _n _p _f in 
        let nQ, nF = assign_var_list params _p _q _f in 
        (nQ, nF, block)
    | None -> failwith "Not found Funcky boi"


and eval_expr_to_list (e:expr list) (_q:envQueue) (_f:fctEnv) =
    match e with
    | [] -> []
    | h::t -> (evalExpr h _q _f)::(eval_expr_to_list t _q _f)





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
    let _ = evalCode p1 [[]] [] in (); 
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
    let _ = evalCode p2 [[]] [] in (); 
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
        Expr(Fct("f", [Num(0.0)]));
        (* Expr(Fct("f", [Num(5.0)])); *)
    ]

let%expect_test "p3" =
    let _ = evalCode p3 [[]] [] in (); 
    [%expect {| 
        2. 
        5.      
    |}]


let p4: block = 
    [
        FctDef("f", ["x"; "y"], [
            Expr(Var("x"));
            Assign("x", Op2("+", Var("x"), Num(1.0)));
            (*Assign("y", Op2("*", Var("y"), Num(2.0)))*)

            Expr(Var("x"));
            Return(Var("x"));
        ]);
        Expr(Fct("f", [Num(1.0); Num(3.0)]))
    
    ]

let%expect_test "p4" =
    let _ = evalCode p4 [[]] [] in (); 
    [%expect {| 
        2. 
        6.
    |}]

let p5: block = 
    [
        Assign("x", Num(0.0));
            If(
                Op2("<", Var("x"), Num(1.0)),
                [Assign("x", Num(2.0))],
                [Assign("x", Num(3.0))]
            );
               Expr(Var("x"))
    
    ]

let%expect_test "p4" =
    let _ = evalCode p5 [[]] [] in (); 
    [%expect {| 
    |}]


