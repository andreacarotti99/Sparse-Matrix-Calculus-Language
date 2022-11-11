open List

type ident = string

type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp
         | Vector of int list

(* switch(E){ case <#>: C … case <#>: C default: C } *)
type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | IfC of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp
           (*| Switch of exp * (int * cmd) list * cmd *)
           | CreateCOO of ident * exp * exp * exp

type coodecl = {varname : ident; row : exp; cols : exp; data : exp}

(* type checker *)
type typ = IntTy | BoolTy | VectorTy | COO of coodecl

type context = ident -> typ option
let empty_context = fun x -> None
let lookup (gamma : context) (x : ident) : typ option = gamma x

let lookup_coo (gamma : context) (x : ident) : coodecl option =
  match lookup gamma x with 
    Some (COO cd) -> Some cd 



(* ------------------------------------------------------------------------------------------------------------------------------------------------ *)
(* coo support functions *)

(*
matrix used as example:
1 7 0 0 
0 2 8 0 
5 0 3 9 
0 6 0 4

COO format of the matrix:
rows = [0; 0; 1; 1; 2; 2; 2; 3; 3]
cols = [0; 1; 1; 2; 0; 2; 3; 1; 3]
data = [1; 7; 2; 8; 5; 3; 9; 6; 4]
*)

(*returns the size of the list*)
let get_size (l : int list) = List.length l

(*given the size of an array and the number of columns of the matrix returns the column position in the matrix of the element in the array*)
let get_col (size : int) (num_cols : int) : int = (num_cols - (size mod num_cols)) mod num_cols

(*given the size of an array, the number of columns of the matrix, and the number of rows returns the row position in the matrix of the element in the array*)
let get_row (size : int) (num_rows : int) (num_cols : int) : int = 
  if (size mod num_cols = 0) then (num_rows - (size / num_cols) ) 
  else (num_rows - (size / num_cols) ) - 1

(*
given an array (representing a 4x4 matrix) like: 
l1 = [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]
returns the array without zeros:
l1 = [1; 7; 2; 8; 5; 3; 9; 6; 4]
*)
let rec get_data_array content =
  match content with
  | [] -> []
  | head :: tail -> (match head with
    | 0 -> get_data_array tail
    | _ -> head :: (get_data_array tail) )


(*
given an array (representing a 4x4 matrix) like: 
l1 = [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]
returns the columns of the non-zero elements in an array
l1 = [0; 1; 1; 2; 0; 2; 3; 1; 3]
*)
let rec get_cols_array content num_cols = 
  match content with
    | [] -> []
    | head :: tail -> (match head with
      | 0 -> get_cols_array tail num_cols
      | _ -> (get_col (get_size content) num_cols) :: (get_cols_array tail num_cols) )

(*
given an array (representing a 4x4 matrix) like: 
l1 = [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]
returns the rows of the non-zero elements in an array
l1 = [0; 0; 1; 1; 2; 2; 2; 3; 3]
*)
let rec get_rows_array content num_rows num_cols = 
  match content with
    | [] -> []
    | head :: tail -> (match head with
      | 0 -> get_rows_array tail num_rows num_cols
      | _ -> (get_row (get_size content) num_rows num_cols) :: (get_rows_array tail num_rows num_cols))
      

(* ------------------------------------------------------------------------------------------------------------------------------------------------ *)

let rec type_of (gamma : context) (e : exp) : typ option =
  match e with
  | Num i -> Some IntTy
  | Add (e1, e2) | Sub (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _, _ -> None)
  | Var x -> lookup gamma x
  | Bool _ -> Some BoolTy
  | And (e1, e2) | Or (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some BoolTy, Some BoolTy -> Some BoolTy
       | _, _ -> None)
  | Eq (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some t1, Some t2 -> if t1 = t2 then Some BoolTy else None
       | _, _ -> None)
  
let rec typecheck_cmd (gamma : context) (c : cmd) : bool =
  match c with
  | Assign (i, e) ->
      (match lookup gamma i, type_of gamma e with
       | Some t1, Some t2 -> t1 = t2
       | _, _ -> false)
  | Seq (c1, c2) -> typecheck_cmd gamma c1 && typecheck_cmd gamma c2
  | Skip -> true
  | IfC (e, c1, c2) -> type_of gamma e = Some BoolTy && typecheck_cmd gamma c1 && typecheck_cmd gamma c2
  | While (e, c) -> type_of gamma e = Some BoolTy && typecheck_cmd gamma c



(* semantics *)
type value = IntVal of int | BoolVal of bool | VectorVal of int list 

type entry = Val of value | Fun of ident list * cmd | COO of coodecl

type state = ident -> entry option
let empty_state = fun x -> None
let lookup (s : state) (x : ident) : entry option = s x
let update (s : state) (x : ident) (e : entry) : state = fun y -> if y = x then Some e else s y

let update_coo (gamma : context) (x : ident) (c : coodecl) : context = update gamma x (COO coo)


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
  | CreateCOO (x, num_row, num_col, content) -> 
      (match eval_exp content s with
          | Some v -> Some (Skip, k, update s x (COO (parse_coo num_row num_col content)) )
          | None -> None)



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
