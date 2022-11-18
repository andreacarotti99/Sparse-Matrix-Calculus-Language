open List

type ident = string

type exp = Var of ident | Num of int | Add of exp * exp | Sub of exp * exp
         | Bool of bool | And of exp * exp | Or of exp * exp
         | Eq of exp * exp
         | Vector of (int list)
          
type cmd = Assign of ident * exp | Seq of cmd * cmd | Skip
           | IfC of exp * cmd * cmd | While of exp * cmd
           | Call of ident * ident * exp list | Return of exp
           | CreateCOO of ident * exp * exp * exp
           | MatSumCOO of ident * exp * exp
           | MatSubCOO of ident * exp * exp
           | MatMulCOO of ident * exp * exp

type coodecl = {rows : exp; cols : exp; data : exp; n_rows: int; n_cols: int}
type value = IntVal of int | BoolVal of bool | VectorVal of (int list) | COOVal of coodecl
type entry = Val of value | Fun of ident list * cmd 
type typ = IntTy | BoolTy | VectorTy | COOTy | FunTy of typ * (typ list)
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
  {rows = Vector(get_rows_array v num_rows num_cols); cols = Vector(get_cols_array v num_cols); data = Vector(get_data_array v); n_rows = num_rows; n_cols = num_cols}

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
                          data = Vector(rev (get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | [], rows2,[] , cols2-> Some {
                          rows = Vector((rev (get_rows_coo ret)) @ rows2); 
                          cols = Vector((rev (get_cols_coo ret)) @ cols2); 
                          data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo2)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | rows1, [],cols1 , []-> Some {
                          rows = Vector((rev (get_rows_coo ret)) @ rows1); 
                          cols = Vector((rev (get_cols_coo ret)) @ cols1); 
                          data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo1)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | r1 :: rest1_row, r2 :: rest2_row,   c1 :: rest1_col,   c2 :: rest2_col ->
        if      (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) + (hd (get_data_coo coo2)) != 0) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2)); n_rows = coo2.n_rows; n_cols = coo2.n_cols} ) ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector(((hd (get_data_coo coo1)) + (hd (get_data_coo coo2)))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})
        else if (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) + (hd (get_data_coo coo2)) = 0) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2)); n_rows = coo2.n_rows; n_cols = coo2.n_cols} ) ret
        else if ((r1 = r2 && c1 < c2)||(r1<r2)) then sum_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) coo2 ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo1))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})
        else if ((r1 = r2 && c1 > c2)|| r2<r1) then sum_op_helper coo1 ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2));  n_rows = coo2.n_rows; n_cols = coo2.n_cols} )({rows = Vector(r2::(get_rows_coo ret)); cols = Vector(c2::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo2))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})

        else None
    | _, _, _, _ -> None

let rec sub_op_helper (coo1: coodecl) (coo2 : coodecl) (ret : coodecl): coodecl option = 
    match get_rows_coo coo1, get_rows_coo coo2, get_cols_coo coo1, get_cols_coo coo2 with
    | [], [],[],[] -> Some {rows = Vector(rev (get_rows_coo ret)); cols = Vector(rev (get_cols_coo ret)); data = Vector(rev (get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | [], rows2,[] , cols2-> Some {rows = Vector((rev (get_rows_coo ret))@ rows2); cols = Vector((rev (get_cols_coo ret)) @ cols2); data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo2)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | rows1, [],cols1 , []-> Some {rows = Vector((rev (get_rows_coo ret))@ rows1); cols = Vector((rev (get_cols_coo ret)) @ cols1); data = Vector((rev (get_data_coo ret)) @ (get_data_coo coo1)); n_rows = ret.n_rows; n_cols = ret.n_cols}
    | r1 :: rest1_row, r2 :: rest2_row,   c1 :: rest1_col,   c2 :: rest2_col->
        if      (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) - (hd (get_data_coo coo2)) != 0) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2)); n_rows = coo2.n_rows; n_cols = coo2.n_cols} ) ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector(((hd (get_data_coo coo1)) - (hd (get_data_coo coo2)))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})
        else if (r1 = r2 && c1 = c2 && (hd (get_data_coo coo1)) - (hd (get_data_coo coo2)) = 0) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2)); n_rows = coo2.n_rows; n_cols = coo2.n_cols} ) ret
        else if ((r1 = r2 && c1 < c2)||(r1<r2)) then sub_op_helper ({rows = Vector(rest1_row); cols = Vector(rest1_col); data = Vector(tl (get_data_coo coo1)); n_rows = coo1.n_rows; n_cols = coo1.n_cols}) coo2 ({rows = Vector(r1::(get_rows_coo ret)); cols = Vector(c1::(get_cols_coo ret)); data = Vector((hd (get_data_coo coo1))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})
        else if ((r1 = r2 && c1 > c2)|| r2<r1) then sub_op_helper coo1 ({rows = Vector(rest2_row); cols = Vector(rest2_col); data = Vector(tl (get_data_coo coo2)); n_rows = coo2.n_rows; n_cols = coo2.n_cols} )({rows = Vector(r2::(get_rows_coo ret)); cols = Vector(c2::(get_cols_coo ret)); data = Vector((-(hd (get_data_coo coo2)))::(get_data_coo ret)); n_rows = ret.n_rows; n_cols = ret.n_cols})

        else None
    | _, _, _, _ -> None

let sum_op (c1: coodecl) (c2 : coodecl): coodecl option = 
  if (c1.n_cols = c2.n_cols && c1.n_rows = c2.n_rows) then sum_op_helper c1 c2 {rows = Vector([]); cols = Vector([]); data = Vector([]); n_rows = c1.n_rows; n_cols = c2.n_cols}
  else None

let sub_op (c1: coodecl) (c2 : coodecl): coodecl option = 
  if (c1.n_cols = c2.n_cols && c1.n_rows = c2.n_rows) then sub_op_helper c1 c2 {rows = Vector([]); cols = Vector([]); data = Vector([]); n_rows = c1.n_rows; n_cols = c2.n_cols}
  else None
    

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
        coo_Ax_mul_helper  ({rows = Vector(r_rest); cols = Vector(c_rest); data = Vector(d_rest); n_rows = c.n_rows; n_cols = c.n_cols}) x (replace y r_head (((get_val_from_idx y r_head) + (d_head * (get_val_from_idx x c_head)))))
  | [], [], [], _ -> Some y
  | _ , _, _ , _ -> None
   
let coo_Ax_mul (c1: coodecl) (x: int list) : int list option = 
  if c1.n_cols = (get_size x) then (
    let y = empty_list (get_size x)
    in
    coo_Ax_mul_helper c1 x y
  )else None


let rec trace_coo_fun (a : int list) (b : int list) (c : int list): int = 
  match a, b, c with
  | h_a :: tail_a, h_b :: tail_b, h_c :: tail_c -> if h_a = h_b then (trace_coo_fun tail_a tail_b tail_c) + h_c else (trace_coo_fun tail_a tail_b tail_c)
  |[], [], [] -> 0 
  | _ -> 0

let trace_coo (c: coodecl): int = trace_coo_fun (get_rows_coo c) (get_cols_coo c) (get_data_coo c)

let rec get_col_order (col_list: int list) (res: int list) (searched_pos: int) (i: int) : int list =
  if get_size col_list = get_size res then res
  else if (length col_list) <= i then get_col_order col_list (res) (searched_pos+1) (0) 
  else if List.nth col_list i = searched_pos then get_col_order col_list (res @ [i]) searched_pos (i +1)
  else get_col_order col_list (res) searched_pos (i +1)

(*pass new positions for its elements*)
let rec re_order_vector (new_order: int list) (v1: int list) (res: int list): int list =
  match new_order with
  | [] -> res
  | head::tail -> re_order_vector tail v1 (res @ [(nth v1 head)])

let rec get_coo_transpose (c: coodecl): coodecl =
  ({cols=Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_rows_coo c) [])); rows = Vector(sort (fun a b -> a - b) (get_cols_coo c)); data = Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_data_coo c) [])); n_rows = c.n_cols; n_cols = c.n_rows})


(************************************************************************************* COO MATRIX MUL **********************************************************************************************************)

let rec coo_partial_matrix_mul_helper (c1: coodecl) (c2: coodecl) (ret: coodecl) (full_c1 : coodecl) (full_c2 : coodecl): coodecl option = 
  match get_rows_coo c1, get_cols_coo c1, get_data_coo c1 with
    | r1_h :: r1_tail, c1_h :: c1_tail, d1_h :: d1_tail -> (
     match get_rows_coo c2, get_cols_coo c2, get_data_coo c2 with
        | r2_h :: r2_tail, c2_h :: c2_tail, d2_h :: d2_tail -> ( 
          if (c1_h = c2_h) then ( coo_partial_matrix_mul_helper c1 {rows = Vector(r2_tail); cols = Vector(c2_tail); data = Vector(d2_tail);n_rows = ret.n_rows;n_cols = ret.n_cols}
             {rows = Vector( r1_h           :: (get_rows_coo ret) ); 
              cols = Vector( r2_h           :: (get_cols_coo ret) ); 
              data = Vector( (d1_h * d2_h)  :: (get_data_coo ret) );
              n_rows = ret.n_rows;
              n_cols = ret.n_cols}) full_c1 full_c2
          else 
            coo_partial_matrix_mul_helper c1 ({rows = Vector(r2_tail); cols = Vector(c2_tail); data = Vector(d2_tail);n_rows = ret.n_rows;n_cols = ret.n_cols}) ret full_c1 full_c2
        )
        | _ , [], [] -> coo_partial_matrix_mul_helper ({rows = Vector(r1_tail); cols = Vector(c1_tail); data = Vector(d1_tail);n_rows = ret.n_rows;n_cols = ret.n_cols}) full_c2 ret full_c1 full_c2
        | _ -> None
    )
    | [], _, _ -> Some {rows = Vector(get_rows_coo ret); cols = Vector(get_cols_coo ret); data = Vector(get_data_coo ret);n_rows = ret.n_rows;n_cols = ret.n_cols}
    | _ -> None


let max_number_list lst = List.fold_left max 0 lst

let coo_partial_matrix_mul (c1: coodecl) (c2: coodecl) : coodecl option = 
  if (c1.n_cols = c2.n_rows) then
    let c2' = get_coo_transpose c2 in
    (*let longer_coo = coo_partial_matrix_mul_helper c1 c2' {rows = Vector([]); cols = Vector([]); data = Vector([])} c1 c2'*) 
    coo_partial_matrix_mul_helper c1 c2' {rows = Vector([]); cols = Vector([]); data = Vector([]); n_rows =c1.n_rows; n_cols = c1.n_cols} c1 c2'   
  else None



let rec tail l =
  match l with
  | [] -> []
  | h :: tl -> tl

let rec head_int l =
  match l with
  | [] -> -1
  | h :: tl -> h

let get_tail l =
  match l with
  | [] -> []
  | h :: tl -> tl

let combine_lists a b = List.combine a b

let rec mergelist (l1 : int list)(l2 : int list)(l3 : int list) = List.combine (List.combine l1 l2) l3

let supersort l = 
  List.sort 
  (fun ((k1, v1),a) ((k2, v2),b) -> 
    if k1 = k2 then Stdlib.compare v1 v2 
    else Stdlib.compare k1 k2) l

let rec retransform l ret : coodecl = match l with
    | ((a,b),c) :: rest -> (retransform rest ({rows = Vector ((a :: (get_rows_coo ret))); cols = Vector ((b :: get_cols_coo ret)); data = Vector ((c :: get_data_coo ret));n_rows = ret.n_rows;n_cols = ret.n_cols}))
    | [] -> ({rows = Vector (get_rows_coo ret); cols = Vector (get_cols_coo ret); data = Vector (get_data_coo ret);n_rows = ret.n_rows;n_cols = ret.n_cols})

let fix_coo_extended (rows_list : int list) (cols_list : int list) (data_list : int list) (rows : int) (cols : int) : coodecl =
  let merged = mergelist rows_list cols_list data_list in
  let sorted = supersort merged in
  let reordered = retransform sorted {rows = Vector ([]); cols = Vector (([])); data = Vector ([]);n_rows = rows;n_cols = cols} in  (*reordered is of type coodecl*)
  let r = (get_rows_coo reordered) in
  let c = (get_cols_coo reordered) in 
  let d = (get_data_coo reordered) in
  {rows = Vector r; cols = Vector c; data = Vector d;n_rows = rows;n_cols = cols}

let is_empty l = match l with
  | [] -> true
  | _ -> false

let rec compress (r_out : int list) (c_out : int list) (d_out : int list) (ret : coodecl) : coodecl =
  match r_out, c_out, d_out with
  | r_h :: r_rest2, c_h :: c_rest2, d_h :: d_rest2 -> (
    match r_rest2, c_rest2, d_rest2 with
    | r_m :: r_rest, c_m :: c_rest, d_m :: d_rest -> (
    if (r_h = r_m && c_h = c_m) then (
        if        ( (head_int (get_rows_coo ret)) != r_h || (head_int (get_cols_coo ret)) != c_h) then compress (r_m :: r_rest) (c_m :: c_rest) (d_m :: d_rest) {rows = Vector (r_h :: (get_rows_coo ret)); cols = Vector (c_h :: (get_cols_coo ret)); data = Vector ((d_h + d_m) :: (get_data_coo ret));n_rows = ret.n_rows;n_cols = ret.n_cols} 
        else      compress (r_m :: r_rest) (c_m :: c_rest) (d_m :: d_rest) {rows = Vector ((get_rows_coo ret)); cols = Vector ((get_cols_coo ret)); data = Vector ((head_int (get_data_coo ret) + d_m) :: (get_tail (get_data_coo ret)));n_rows = ret.n_rows;n_cols = ret.n_cols} 
    )
    else
      if (is_empty (get_rows_coo ret)) then compress (r_m :: r_rest) (c_m :: c_rest) (d_m :: d_rest) {rows = Vector (r_h :: r_m :: (get_rows_coo ret)); cols = Vector (c_m :: c_h :: (get_cols_coo ret)); data = Vector (d_m :: d_h :: (get_data_coo ret));n_rows = ret.n_rows;n_cols = ret.n_cols} 
      else compress (r_m :: r_rest) (c_m :: c_rest) (d_m :: d_rest) {rows = Vector (r_m :: (get_rows_coo ret)); cols = Vector (c_m :: (get_cols_coo ret)); data = Vector (d_m :: (get_data_coo ret));n_rows = ret.n_rows;n_cols = ret.n_cols} 
    )
    | _ -> ret
  )
  | [], [], [] -> ret
  | _ -> ret
  
let coo_complete_matrix_mul (c1: coodecl) (c2 : coodecl) : coodecl option = 
  let partial = coo_partial_matrix_mul c1 c2 in
  match partial with
  | Some a -> (
    let fixed = fix_coo_extended (get_rows_coo a) (get_cols_coo a) (get_data_coo a) a.n_rows a.n_cols in
    match compress (get_rows_coo fixed) (get_cols_coo fixed) (get_data_coo fixed) {rows = Vector ([]); cols = Vector (([])); data = Vector ([]);n_rows = fixed.n_rows;n_cols = fixed.n_cols} with
    | x -> Some x
  )
  | _ -> None


(**************************************************************************************************************************************************************************************************************)

(*
let rec get_matrix_transpose (c: coodecl): coodecl =
  ({rows=Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_rows_coo c) [])); cols = Vector(sort (fun a b -> a - b) (get_cols_coo c)); data = Vector((re_order_vector (get_col_order (get_cols_coo c) [] 0 0) (get_data_coo c) []))})
  
*)

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
  
(*type_of a given list*)
let rec type_of_list (gamma: context) (es: exp list): typ list option = 
  match es with
  |[] -> Some []
  | e :: rest -> (match type_of gamma e, type_of_list gamma rest with
                  | Some v, Some vs -> Some (v :: vs)
                  | _, _ -> None)

(* checks if in list they have same type*)
let rec typecheck_lists (gamma : context) (t1 : typ list) (t2 : typ list) : bool =
  match t1, t2 with
  | [], [] -> true
  | e1 :: e1_rest, e2 :: e2_rest -> e1 = e2 && typecheck_lists gamma e1_rest e2_rest
  | _, _ -> false


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
  | CreateCOO(x, num_row, num_col, content) ->(match lookup_context gamma x, type_of gamma num_row, type_of gamma num_col,type_of gamma content with
                                              | Some COOTy, Some IntTy, Some IntTy, Some VectorTy->true
                                              |_,_,_,_ ->false
  )
  | MatSumCOO(x,m1,m2) | MatSubCOO(x,m1,m2) | MatMulCOO(x,m1,m2)-> (match lookup_context gamma x, type_of gamma m1, type_of gamma m2 with
                            | Some COOTy, Some COOTy, Some COOTy->true
                            |_,_,_ ->false
  )
  | Call(x, f, es) -> (match lookup_context gamma x,lookup_context gamma f  with
                    | Some type_x, Some type_f-> (match type_f with
                                                  |FunTy(ret_ty, args_ty) -> (if ret_ty != type_x then false
                                                                              else match (type_of_list gamma es) with
                                                                                |Some es_ty ->typecheck_lists gamma args_ty es_ty
                                                                                |_ -> false
                                                                              )
                                                  |_ -> false

                    )
                    | _,_ -> false
                    )
  | Return e ->
      (match lookup_context gamma "__ret",type_of gamma e with
       | Some ret_ty, Some es_ty -> ret_ty = es_ty
       | _,_ -> false)



let rec eval_exp (e : exp) (s : state) : value option =
  match e with
  | Var x -> (match lookup_state s x with Some (Val v) -> Some v 
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
          | Some( VectorVal v ), Some(IntVal r),Some(IntVal c)-> Some (Skip, k, update_state s x (Val(COOVal (parse_coo r c v))) )
          | _ -> None)
  | MatSumCOO (x, v1, v2) -> 
      (match eval_exp v1 s,eval_exp v2 s with
          | Some( COOVal c1), Some(COOVal c2)->( match (sum_op c1 c2) with
                                                |Some res -> Some (Skip, k, update_state s x (Val(COOVal res)) )
                                                |_ ->None)
          | _ -> None)
  | MatSubCOO (x, v1, v2) -> 
      (match eval_exp v1 s,eval_exp v2 s with
          | Some( COOVal c1), Some(COOVal c2)->( match (sub_op c1 c2) with
                                                |Some res -> Some (Skip, k, update_state s x (Val(COOVal res)) )
                                                |_ ->None)
          | _ -> None)
  | MatMulCOO (x, v1, v2) ->
    (match eval_exp v1 s, eval_exp v2 s with
      | Some (COOVal c1), Some (VectorVal vec) -> (match (coo_Ax_mul c1 vec) with
                                                | Some res -> Some (Skip, k, update_state s x (Val(VectorVal res)))
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
let r2 = [0; 0; 1; 1]
let c2 = [1; 2; 0; 2]
let d2 = [10; 12; 1; 2]
let r3 = [0; 0; 1; 2]
let c3 = [0; 1; 1; 0]
let d3 = [2; 5; 1; 8]

let decl : coodecl = {rows = Vector r; cols = Vector c; data = Vector d; n_rows = 4; n_cols = 4}
let empty_decl : coodecl = {rows = Vector []; cols = Vector []; data = Vector []; n_rows = 4; n_cols = 4}
let decl2 : coodecl = {rows = Vector r2; cols = Vector c2; data = Vector d2; n_rows = 2; n_cols = 3}
let decl3: coodecl = { rows = Vector r3; cols = Vector c3; data = Vector d3; n_rows = 3; n_cols = 2}


let ra = [3; 0; 0; 0; 2; 3; 3]
let ca = [3; 1; 2; 3; 2; 0; 1]
let da = [6; 10; 4; 2; 3; 4; 2]
let rb = [3; 0; 1; 2; 2; 3; 3]
let cb = [3; 2; 3; 1; 2; 0; 1]
let db = [6; 8; 3; 2; 8; 2; 7]
let decla : coodecl = {rows = Vector ra; cols = Vector ca; data = Vector da; n_rows = 4; n_cols = 4}
let declb: coodecl = { rows = Vector rb; cols = Vector cb; data = Vector db; n_rows = 4; n_cols = 4}
let test_coo_partial_matrix_mul = coo_partial_matrix_mul decl decl
(*should return:
[3; 3; 3; 3; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1; 0; 0; 0; 0];
[3; 1; 2; 1; 3; 1; 3; 2; 0; 1; 0; 3; 2; 0; 2; 1; 2; 1; 1; 0];
[16; 24; 48; 12; 36; 54; 27; 9; 15; 35; 5; 72; 24; 40; 16; 4; 56; 14; 7; 1]
*)
let test_coo_complete_matrix_mul = coo_complete_matrix_mul decl decl

let x = [2; 1; 3; 2]
let y = [1;3;2;4]
let w = combine_lists x y

let test_multiplication_Ax_COO = coo_Ax_mul decl x (* should return [16; 20; 34; 20] *)
let x2 = [2; 1; 0; 1]
 let test_multiplication_Ax_COO_2 = coo_Ax_mul decl x2 (*should return [9; 2; 19; 10]
let test_col_order = get_col_order ((get_cols_coo decl) [] 0 0)
let test_transpose = get_coo_transpose decl
let test_col_order = get_col_order ((get_cols_coo decl) [] 0 0)*)

(*
let x = [3; 3; 3; 3; 2; 2; 2; 2; 2; 2; 2; 1; 1; 1; 1; 1; 0; 0; 0; 0]
let y = [3; 1; 2; 1; 3; 1; 3; 2; 0; 1; 0; 3; 2; 0; 2; 1; 2; 1; 1; 0]
let z = [16; 24; 48; 12; 36; 54; 27; 9; 15; 35; 5; 72; 24; 40; 16; 4; 56; 14; 7; 1]
*)

(*
expected
let x = [3; 3; 3; 2; 2; 2; 2; 1; 1; 1; 1; 0; 0; 0]
let y = [3; 1; 2; 3; 1; 2; 0; 3; 2; 0; 1; 2; 1; 0]
let z = [16; 24+12; 48; 12+54; 36+15; 27; 9+35; 5; 72+40; 24; 16; 4; 56; 14+7; 1]
*)

(*let test_compress_longer_coo = compress_longer_coo x y z *)

let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))
let r2 = [0; 0; 1; 1]
let c2 = [1; 2; 0; 2]
let d2 = [10; 12; 1; 2]
let r3 = [0; 0; 1; 2]
let c3 = [0; 1; 1; 0]
let d3 = [2; 5; 1; 8]
let decl2 : coodecl = {rows = Vector r2; cols = Vector c2; data = Vector d2; n_rows = 2; n_cols = 3}
let decl3 : coodecl = {rows = Vector r3; cols = Vector c3; data = Vector d3; n_rows = 3; n_cols = 2}
(*let test_coo_partial_matrix_mul = coo_partial_matrix_mul decl2 decl3 empty_decl*)
(*
let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))
let state1 = update_state (update_state state0 "x" (Val (IntVal 1))) "y" (Val (IntVal 2))
let config1 =( Seq(Seq(CreateCOO("z",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),CreateCOO("s",Num 4, Num 4, Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4])),MatSubCOO("c",Var "z",Var "s")), [(state0, "x")], state1)
let config2 = (CreateCOO("z",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]), [(state0, "x")], state1)

let config3 = (Seq(Seq(Assign("k", Vector x2), CreateCOO("l",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4])), MatMulCOO("r", Var "l", Var "k")), [(state0, "x")], state1)

let test_coo_trace = trace_coo decl 

let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup_state res_s "z";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "s";; 
lookup_state res_s "c";; 

let (res_c, res_k, res_s) = run_config config2;;
lookup_state res_s "z";; (* should return Some (Val (IntVal 3)) *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup_state res_s "x";;  should return Some (Val (IntVal 3))
lookup_state res_s "y";; should return None 

*)
 