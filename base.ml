open List

type ident = string

type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp

(* switch(E){ case <#>: C … case <#>: C default: C } *)
type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | IfC of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp
           | Switch of exp * (int * cmd) list * cmd


(* type checker *)
type context = ident -> typ option
let empty_context = fun x -> None
let lookup (gamma : context) (x : ident) : typ option = gamma x

let rec type_of (gamma : context) (e : exp) : typ option =
  match e with
  | Num i -> Some IntTy
  | Add (e1, e2) | Mul (e1, e2) ->
      (match type_of ct gamma e1, type_of ct gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _, _ -> None)
  | Var x -> lookup gamma x
  
let rec typecheck_cmd (gamma : context) (c : cmd) : bool =
  match c with
  | Assign (i, e) ->
      (match gamma i, type_of ct gamma e with
       | Some t1, Some t2 -> subtype ct t2 t1
       | _, _ -> false)
  | Seq (c1, c2) -> typecheck_cmd ct gamma c1 && typecheck_cmd ct gamma c2
  | Skip -> true
  | Switch (e, cl, def) -> type_of gamma e = Some IntTy && typecheck_cmd gamma def && typecheck_list gamma (List.map snd cl)
and typecheck_list gamma (cl : cmd list) : bool =
...


(* semantics *)
type value = IntVal of int | BoolVal of bool

type entry = Val of value | Fun of ident list * cmd

type state = ident -> entry option
let empty_state = fun x -> None
let lookup (s : state) (x : ident) : entry option = s x
let update (s : state) (x : ident) (e : entry) : state = fun y -> if y = x then Some e else s y

let rec eval_exp (e : exp) (s : state) : value option =
  match e with
  | Var x -> (match lookup s x with Some (Val v) -> Some v | _ -> None)
  | Num i -> Some (IntVal i)
  | Add (e1, e2) -> (match eval_exp e1 s, eval_exp e2 s with
                     | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 + i2))
                     | _, _ -> None)
  | Sub (e1, e2) -> (match eval_exp e1 s, eval_exp e2 s with
                     | Some (IntVal i1), Some (IntVal i2) -> Some (IntVal (i1 - i2))
                     | _, _ -> None)
  | Bool b -> Some (BoolVal b)
  | And (e1, e2) -> (match eval_exp e1 s, eval_exp e2 s with
                     | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 && b2))
                     | _, _ -> None)
  | Or (e1, e2) -> (match eval_exp e1 s, eval_exp e2 s with
                     | Some (BoolVal b1), Some (BoolVal b2) -> Some (BoolVal (b1 || b2))
                     | _, _ -> None)
  | Eq (e1, e2) -> (match eval_exp e1 s, eval_exp e2 s with
                     | Some v1, Some v2 -> Some (BoolVal (v1 = v2))
                     | _, _ -> None)

let rec eval_exps (es : exp list) (s : state) : value list option =
  match es with
  | [] -> Some []
  | e :: rest -> (match eval_exp e s, eval_exps rest s with
                  | Some v, Some vs -> Some (v :: vs)
                  | _, _ -> None)

let rec add_args (s : state) (li : ident list) (lv : value list) : state =
  match li, lv with
  | i :: irest, v :: vrest -> add_args (update s i (Val v)) irest vrest
  | _, _ -> s

type stack = (state * ident) list

type config = cmd * stack * state

let rec step_cmd (c : cmd) (k : stack) (s : state) : config option =
  match c with
  | Assign (x, e) -> (match eval_exp e s with
                      | Some v -> Some (Skip, k, update s x (Val v))
                      | None -> None)
  | Seq (Skip, c2) -> Some (c2, k, s)
  | Seq (c1, c2) -> (match step_cmd c1 k s with
                     | Some (c1', k', s') -> Some (Seq (c1', c2), k', s')
                     | None -> None)
  | Skip -> None
  | IfC (e, c1, c2) -> (match eval_exp e s with
                        | Some (BoolVal true) -> Some (c1, k, s)
                        | Some (BoolVal false) -> Some (c2, k, s)
                        | _ -> None)
  | While (e, c) -> Some (IfC (e, Seq (c, While (e, c)), Skip), k, s)
  | Call (x, f, es) -> (match eval_exps es s with
                        | Some vs -> (match lookup s f with
                                      | Some (Fun (params, c)) -> Some (c, (s, x) :: k, add_args s params vs)
                                      | _ -> None)
                        | None -> None)
  | Return e -> (match eval_exp e s, k with
                 | Some v, (s', x) :: k' -> Some (Skip, k', update s' x (Val v))
                 | _, _ -> None)
(* ((𝑒,𝜎)⇓𝑣    (𝑣=𝑖_𝑗))/((switch(𝑒){ case 𝑖_1:𝑐_1…case 𝑖_𝑛:𝑐_𝑛  default:𝑐 },𝑘,𝜎)→(𝑐_𝑗,𝑘,𝜎) )
*)
  | Switch (e, cl, def) -> (match eval_exp e s with
                            | Some (IntVal v) -> (match find_case cl v with
                                                  | Some res -> Some (res, k, s)
                                                  | None -> Some (def, k, s))
                            | _ -> None)

let rec run_config (con : config) : config =
  let (c, k, s) = con in
  match step_cmd c k s with
  | Some con' -> run_config con'
  | None -> con

let run_prog (c : cmd) s =
  run_config (c, [], s)

let state0 = update empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))

let state1 = update (update state0 "x" (Val (IntVal 1)))
  "y" (Val (IntVal 2))
  
let config1 = (Return (Add (Var "x", Var "y")), [(state0, "x")], state1)

let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_s "y";; (* should return None *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup res_s "y";; (* should return None *)
