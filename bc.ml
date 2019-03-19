open Core

type sExpr = 
    | Atom of string
    | List of sExpr list

type expr = 
    | Num of float
    | Var of string
    | Op1 of string*expr
    | Op2 of string*expr*expr
    | Fct of string * expr list

type statement = 
    | Assign of string*expr
    | Return of expr
    | Expr of expr
    | If of expr*statement list * statement list
    | While of expr*statement list
    | For of statement*expr*statement*statement list
    | FctDef of string * string list * statement list 

type block = statement list 

type env = (string*float) list

        (* complete *)

type envQueue = env list

let rec printTupleList l = match l with
    | [] -> ()
    | h::t -> let (x,y) = h in
            printf "%s %f, " x y
let rec printQueue que = match que with
    | [] -> ()
    | h::t -> printTupleList h; printf "\n" ; printQueue t


let rec find_var var l =
    match l with
            [] -> raise Not_found
            |(s, i)::tl -> if s = var then i
                                  else find_var var tl

let varEval (_v: string) (_q:envQueue): float  =
    match List.hd _q with
    | Some h -> find_var _v h
    | None -> failwith "Not found"

let eval_op1 op (e1: float): float=
    match op with 
  | "++" -> e1 +. 1.0
  | "--" -> e1 -. 1.0
  | _ -> failwith "oof"

let eval_op2 op e1 e2 =
    match op with
  | "+" -> (+.) e1 e2
  | "-"-> (-.) e1 e2
  | "*" -> ( *. ) e1 e2
  | "/" -> (/.) e1 e2
  | ">" -> if (e1 > e2) then 1.0 else 0.0
  | x -> printf "%s" x; (+.) e1 e2

let rec evalExpr (_e: expr) (_q:envQueue): float  = 
    match _e with
    | Num n -> n
    | Var v -> varEval v _q
    | Op1 (op, e1) -> (eval_op1 op (evalExpr e1 _q))
    | Op2 (op, e1, e2) -> (eval_op2 op (evalExpr e1 _q) (evalExpr e2 _q))
    | Fct _ -> 1.2

(* Test for expression *)
let%expect_test "evalNum" = 
    evalExpr (Op1("++", Num 1.0)) [] |>
    printf "%F";
    [%expect {| 10. |}]

let rec insert_at_end l i =
  match l with
    [] -> [i]
  | h :: t -> h :: (insert_at_end t i)

let hd l = match List.hd l with
    | Some x -> x
    | None -> failwith "oof"

let rec evalCode (_code: block) (_q:envQueue): unit = 
    let q = [] @ _q in 
    printQueue q;
    match _code with 
    | [] -> ();
    | h:: t -> let outq = evalStatement h q in
                evalCode t outq; ()
    (* let y = List.fold_left evalStatement  [] in ()*)
    (* let x = evalStatement (hd _code) q in () *)
    (* in let (a, b) = hd (hd x) in *)
    (* printf "%s %f\n" a b *)
    (* pop the local environment *)


and evalStatement (s: statement) (q:envQueue): envQueue =
    match s with 
        | Assign(_v, _e) -> let out = evalExpr _e q in
                            let sc = List.hd q in
                            insert_at_end q [(_v, out)]
        | If(e, codeT, codeF) -> 
            let cond = evalExpr e q in
                if(cond>0.0) then
                    evalCode codeT q 
                else
                    evalCode codeF q
            ;q
        | Expr e -> let out = evalExpr e q in
                printf "%f\n" out; q
        | For (s1, e, s2, code) -> q (* ree *)
        | _ -> q (*ignore *)

let p999: statement = Assign("v", Num(1.0))
let%test _ = evalStatement p999 [] = [[("v", 1.0)]]

(* 
    v = 10; 
    v // display v
 *)
let p1: block = [
        Assign("v", Num(1.0));
        Expr(Var("v")) 
]

let%expect_test "p1" =
    evalCode p1 []; 
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

let%expect_test "p1" =
    evalCode p2 []; 
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
                    Fct("f", [Op2("-", Var("x"), Num(1.0))])
                ))])
        ]);
        Expr(Fct("f", [Num(3.0)]));
        Expr(Fct("f", [Num(5.0)]));
    ]

let%expect_test "p3" =
    evalCode p3 []; 
    [%expect {| 
        2. 
        5.      
    |}]



