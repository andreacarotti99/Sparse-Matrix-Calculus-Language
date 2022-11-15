open List

type ident = string

type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp
         | Vector of int list
          
type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | IfC of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp
           | CreateCOO of ident * exp * exp * exp
           | MatSumCOO of ident * exp * exp
           | MatSubCOO of ident * exp * exp
           | MatMulCOO of ident * exp * exp

type coodecl = {rows : exp; cols : exp; data : exp}
type value = IntVal of int | BoolVal of bool | VectorVal of int list | COOVal of coodecl
type entry = Val of value | Fun of ident list * cmd | COO of coodecl | Vector of int list
type typ = IntTy | BoolTy | VectorTy | COO of coodecl

(* context *)
type context = ident -> typ option
let empty_context = fun x -> None
let lookup_context (gamma : context) (x : ident) : typ option = gamma x
let update_context (gamma : context) (x : ident) (e : typ) = fun y -> if y = x then Some e else gamma y

(* state *)
type state = ident -> entry option
let empty_state = fun x -> None
let lookup_state (s : state) (x : ident) : entry option = s x
let update_state (s : state) (x : ident) (e : entry) : state = fun y -> if y = x then Some e else s y
let lookup_coo (gamma : context) (x : ident) : coodecl option =
  match lookup_context gamma x with 
    | Some (COO cd) -> Some cd 
    | _ -> None
let update_coo (gamma : context) (x : ident) (coo : coodecl) : context = update_context gamma x (COO coo)

type stack = (state * ident) list

type config = cmd * stack * state

(* ------------------------------------------------------------------------------------------------------------------------------------------------ *)
(* coo support functions *)

(*
matrix used as example:
1 7 0 0   2   16
0 2 8 0   2   20
5 0 3 9 * 2 = 34
0 6 0 4   2   20

COO format of the matrix:
rows = [0; 0; 1; 1; 2; 2; 2; 3; 3]
cols = [0; 1; 1; 2; 0; 2; 3; 1; 3]
data = [1; 7; 2; 8; 5; 3; 9; 6; 4]

Transposted
rows = [0; 0; 1; 1; 1; 2; 2]
cols = [0; 2; 0; 1; 3; 2; 3]
data = []
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
let rec get_data_array (content:int list): int list =
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
let rec get_cols_array (content:int list) (num_cols:int): int list = 
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
let rec get_rows_array (content:int list) (num_rows: int) (num_cols:int): int list = 
  match content with
    | [] -> []
    | head :: tail -> (match head with
      | 0 -> get_rows_array tail num_rows num_cols
      | _ -> (get_row (get_size content) num_rows num_cols) :: (get_rows_array tail num_rows num_cols))
      

let parse_coo (num_rows:int) (num_cols:int) (v:int list): coodecl =
  {rows = Vector(get_rows_array v num_rows num_cols); cols = Vector(get_cols_array v num_cols); data = Vector(get_data_array v)}

let get_rows_coo (c: coodecl): int list = 
  match c.rows with 
    | Vector v -> v
    | _ -> []

let get_cols_coo (c: coodecl): int list = 
  match c.cols with 
    | Vector v -> v
    | _ -> []
  
let get_data_coo (c: coodecl): int list = 
  match c.data with 
    | Vector v -> v
    | _ -> []

let rec sum_op_helper (coo1: coodecl) (coo2 : coodecl) (ret : coodecl): coodecl option = 
    match get_rows_coo coo1, get_rows_coo coo2, get_cols_coo coo1, get_cols_coo coo2 with
    | [], [],[],[] -> Some {
                          rows = Vector(rev (get_rows_coo ret)); 
                          cols = Vector(rev (get_cols_coo ret)); 
                          data = Vector(rev (get_data_coo ret))}
    | [], rows2,[] , cols2-> Some {
                          rows = Vector((rev (get_rows_coo ret)) @ rows2); 
                          cols = Vector((rev (get_cols_coo ret)) @ cols2); 
                          data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo2))}
    | rows1, [],cols1 , []-> Some {
                          rows = Vector((rev (get_rows_coo ret)) @ rows1); 
                          cols = Vector((rev (get_cols_coo ret)) @ cols1); 
                          data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo1))}
    | r1 :: rest1_row, r2 :: rest2_row,   c1 :: rest1_col,   c2 :: rest2_col ->
        if      (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) + (hd (get_data_coo coo2)) != 0) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} ) ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector(((hd (get_data_coo coo1)) + (hd (get_data_coo coo2)))::(get_data_coo ret))})
        else if (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) + (hd (get_data_coo coo2)) = 0) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} ) ret
        else if ((r1 = r2 && c1 < c2)||(r1<r2)) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) coo2 ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo1))::(get_data_coo ret))})
        else if ((r1 = r2 && c1 > c2)|| r2<r1) then sum_op_helper coo1 ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} )({rows = Vector(r2::(get_rows_coo ret)); cols = Vector(c2::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo2))::(get_data_coo ret))})

        else None
    | _, _, _, _ -> None

let rec sub_op_helper (coo1: coodecl) (coo2 : coodecl) (ret : coodecl): coodecl option = 
    match get_rows_coo coo1, get_rows_coo coo2, get_cols_coo coo1, get_cols_coo coo2 with
    | [], [],[],[] -> Some {rows = Vector(rev (get_rows_coo ret)); cols = Vector(rev (get_cols_coo ret)); data = Vector(rev (get_data_coo ret))}
    | [], rows2,[] , cols2-> Some {rows = Vector((rev (get_rows_coo ret))@ rows2); cols = Vector((rev (get_cols_coo ret)) @ cols2); data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo2))}
    | rows1, [],cols1 , []-> Some {rows = Vector((rev (get_rows_coo ret))@ rows1); cols = Vector((rev (get_cols_coo ret)) @ cols1); data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo1))}
    | r1 :: rest1_row, r2 :: rest2_row,   c1 :: rest1_col,   c2 :: rest2_col->
        if      (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) - (hd (get_data_coo coo2)) != 0) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} ) ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector(((hd (get_data_coo coo1)) - (hd (get_data_coo coo2)))::(get_data_coo ret))})
        else if (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) - (hd (get_data_coo coo2)) = 0) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} ) ret
        else if ((r1 = r2 && c1 < c2)||(r1<r2)) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1))}) coo2 ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo1))::(get_data_coo ret))})
        else if ((r1 = r2 && c1 > c2)|| r2<r1) then sub_op_helper coo1 ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2))} )({rows = Vector(r2::(get_rows_coo ret)); cols = Vector(c2::(get_cols_coo ret)); data = Vector((-(hd (get_data_coo coo2)))::(get_data_coo ret))})

        else None
    | _, _, _, _ -> None

let sum_op (c1: coodecl) (c2 : coodecl): coodecl option = 
    sum_op_helper c1 c2 {
        rows = Vector([]); 
        cols = Vector([]); 
        data = Vector([])}

let sub_op (c1: coodecl) (c2 : coodecl): coodecl option = 
    sub_op_helper c1 c2 {rows = Vector([]); cols = Vector([]); data = Vector([])}

let get_val_from_idx (l : int list) (index : int) : int =
  match nth_opt l index with
  | Some v -> v
  | None -> -1

let empty_list (length : int) : int list = List.init length (fun x->0)

let replace list idx elem  = List.mapi (fun i x -> if i = idx then elem else x) list 

(* 
Multiplication: Ax = y where A is a COO matrix and x is a generic Vector
C implementation:
coo_Ax(rows, cols, data, nnz, x, y) //nnz is the number of non-zeroes in the matrix
  for (i = 0; i < nnz; i++) {
    y[rows[i]] += data[i] * x[cols[i]] 
  }
*)
let rec coo_Ax_mul_helper (c: coodecl) (x: int list) (y : int list) : int list option = 
  match get_rows_coo c, get_cols_coo c, get_data_coo c, y with
  | r_head :: r_rest, c_head :: c_rest, d_head :: d_rest, y_head :: y_rest -> 
        coo_Ax_mul_helper  ({rows = Vector(r_rest); cols = Vector(c_rest); data = Vector(d_rest)}) x (replace y r_head (((get_val_from_idx y r_head) + (d_head * (get_val_from_idx x c_head)))))
  | [], [], [], _ -> Some y
  | _ , _, _ , _ -> None
   
let coo_Ax_mul (c1: coodecl) (x: int list) : int list option = 
  let y = empty_list (get_size x)
  in
  coo_Ax_mul_helper c1 x y


let rec get_col_order (col_list: int list) (res: int list) (searched_pos: int) (i: int) : int list =
  if List.length col_list = List.length res then res
  else if (length col_list) <= i then get_col_order col_list (res) (searched_pos+1) (0) 
  else if List.nth col_list i = searched_pos then get_col_order col_list (res @ [i]) searched_pos (i +1)
  else get_col_order col_list (res) searched_pos (i +1)

(*pass new positions for its elements*)
let rec re_order_vector (new_order: int list) (v1: int list) (res: int list): int list =
  match new_order with
  | [] -> res
  | head::tail -> re_order_vector tail v1 (res @ [(nth v1 head)])

let rec get_matrix_transpose (c: coodecl): coodecl =
  ({rows=Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_rows_coo c) [])); cols = Vector(sort (fun a b -> a - b) (get_cols_coo c)); data = Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_data_coo c) []))})

  
 
  (* ------------------------------------------------------------------------------------------------------------------------------------------------ *)

let rec type_of (gamma : context) (e : exp) : typ option =
  match e with
  | Num i -> Some IntTy
  | Add (e1, e2) | Sub (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some IntTy, Some IntTy -> Some IntTy
       | _, _ -> None)
  | Var x -> lookup_context gamma x
  | Bool _ -> Some BoolTy
  | And (e1, e2) | Or (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some BoolTy, Some BoolTy -> Some BoolTy
       | _, _ -> None)
  | Eq (e1, e2) ->
      (match type_of gamma e1, type_of gamma e2 with
       | Some t1, Some t2 -> if t1 = t2 then Some BoolTy else None
       | _, _ -> None)
  | Vector v -> Some VectorTy
  
let rec typecheck_cmd (gamma : context) (c : cmd) : bool =
  match c with
  | Assign (i, e) ->
      (match lookup_context gamma i, type_of gamma e with
       | Some t1, Some t2 -> t1 = t2
       | _, _ -> false)
  | Seq (c1, c2) -> typecheck_cmd gamma c1 && typecheck_cmd gamma c2
  | Skip -> true
  | IfC (e, c1, c2) -> type_of gamma e = Some BoolTy && typecheck_cmd gamma c1 && typecheck_cmd gamma c2
  | While (e, c) -> type_of gamma e = Some BoolTy && typecheck_cmd gamma c
  | CreateCOO(x, num_row, num_col, content) ->(match lookup_coo gamma x, type_of gamma num_row, type_of gamma num_col,type_of gamma content with
                                              | Some _, Some IntTy, Some IntTy, Some VectorTy->true
                                              |_,_,_,_ ->false
  )



let rec eval_exp (e : exp) (s : state) : value option =
  match e with
  | Var x -> (match lookup_state s x with Some (Val v) -> Some v 
              | Some( COO c) -> Some(COOVal c)
              | _ -> None)
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
  | Vector v -> Some(VectorVal v)

let rec eval_exps (es : exp list) (s : state) : value list option =
  match es with
  | [] -> Some []
  | e :: rest -> (match eval_exp e s, eval_exps rest s with
                  | Some v, Some vs -> Some (v :: vs)
                  | _, _ -> None)

let rec add_args (s : state) (li : ident list) (lv : value list) : state =
  match li, lv with
  | i :: irest, v :: vrest -> add_args (update_state s i (Val v)) irest vrest
  | _, _ -> s



let rec step_cmd (c : cmd) (k : stack) (s : state) : config option =
  match c with
  | Assign (x, e) -> (match eval_exp e s with
                      | Some v -> Some (Skip, k, update_state s x (Val v))
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
                        | Some vs -> (match lookup_state s f with
                                      | Some (Fun (params, c)) -> Some (c, (s, x) :: k, add_args s params vs)
                                      | _ -> None)
                        | None -> None)
  | Return e -> (match eval_exp e s, k with
                 | Some v, (s', x) :: k' -> Some (Skip, k', update_state s' x (Val v))
                 | _, _ -> None)
  | CreateCOO (x, num_row, num_col, content) -> 
      (match eval_exp content s,eval_exp num_row s,eval_exp num_col s with
          | Some( VectorVal v ), Some(IntVal r),Some(IntVal c)-> Some (Skip, k, update_state s x (COO (parse_coo r c v)) )
          | _ -> None)
  | MatSumCOO (x, v1, v2) -> 
      (match eval_exp v1 s,eval_exp v2 s with
          | Some( COOVal c1), Some(COOVal c2)->( match (sum_op c1 c2) with
                                                |Some res -> Some (Skip, k, update_state s x (COO res) )
                                                |_ ->None)
          | _ -> None)
  | MatSubCOO (x, v1, v2) -> 
      (match eval_exp v1 s,eval_exp v2 s with
          | Some( COOVal c1), Some(COOVal c2)->( match (sub_op c1 c2) with
                                                |Some res -> Some (Skip, k, update_state s x (COO res) )
                                                |_ ->None)
          | _ -> None)
  | MatMulCOO (x, v1, v2) ->
    (match eval_exp v1 s, eval_exp v2 s with
      | Some (COOVal c1), Some (VectorVal vec) -> (match (coo_Ax_mul c1 vec) with
                                                | Some res -> Some (Skip, k, update_state s x (Vector res))
                                                | _ -> None )
      | _ -> None)

let rec run_config (con : config) : config =
  let (c, k, s) = con in
  match step_cmd c k s with
  | Some con' -> run_config con'
  | None -> con

let run_prog (c : cmd) s =
  run_config (c, [], s)



(* --------------------------------------------------------------- TEST --------------------------------------------------------------------------------- *)
let r = [0; 0; 1; 1; 2; 2; 2; 3; 3]
let c = [0; 1; 1; 2; 0; 2; 3; 1; 3]
let d = [1; 7; 2; 8; 5; 3; 9; 6; 4]
let r3 = [0; 0; 1; 2]
let c3 = [0; 1; 1; 0]

let d3 = [2; 5; 1; 8]
let decl : coodecl = {rows = Vector r; cols = Vector c; data = Vector d}

let decl3: coodecl = { rows = Vector r3; cols = Vector c3; data = Vector d3}
let x = [2; 2; 2; 2]
let test_multiplication_Ax_COO = coo_Ax_mul decl x (* should return [16; 20; 34; 20] *)
let x2 = [2; 1; 0; 1]
let test_multiplication_Ax_COO_2 = coo_Ax_mul decl x2 (* should return [9; 2; 19; 10] *)


let test_col_order = get_col_order (get_cols_coo decl) [] 0 0
let test_transpose = get_matrix_transpose decl3


(*
let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))

let state1 = update_state (update_state state0 "x" (Val (IntVal 1)))
  "y" (Val (IntVal 2))
  
let config1 =( Seq(Seq(CreateCOO("z",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),CreateCOO("s",Num 4, Num 4, Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4])),MatSubCOO("c",Var "z",Var "s")), [(state0, "x")], state1)
let config2 = (CreateCOO("z",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]), [(state0, "x")], state1)
let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup_state res_s "z";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "s";; 
lookup_state res_s "c";; 

let (res_c, res_k, res_s) = run_config config2;;
lookup_state res_s "z";; (* should return Some (Val (IntVal 3)) *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup_state res_s "x";;  should return Some (Val (IntVal 3))
lookup_state res_s "y";; should return None *)
 