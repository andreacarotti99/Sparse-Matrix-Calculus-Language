(* Expression typechecking tests *)
(* Tests function type_of that does all the work in the typechecking of expressions *)
let ctx = update_context empty_context "d1" COOTy
(* Tests for Add, Sub, and Mul *)
let add_0 = type_of empty_context (Add (Num 1, Num 2)) (* Shoul return Some IntTy *)
let add_1 = type_of empty_context (Add (Num 2, Vector [1;3;4])) (* Shoul return None *)
let add_2 = type_of empty_context (Add (Var "d", Num 3)) (* Shoul return None *)

(* Tests for Or and And *)
let and_0 = type_of empty_context (And(Bool true, Eq(Bool true, Bool false))) (* Shoul return Some BoolTy *)
let and_1 = type_of empty_context (And (Vector ([2;3;5]), Vector ([2;3;5]))) (* Shoul return None *)

let ctx = update_context ctx "d2" COOTy

(* Tests for Eq *)
let eq_0 = type_of empty_context (Eq(Num (-3), Num 2))(* Shoul return Some BoolTy *)
let eq_1 = type_of empty_context (Eq (Bool true, Eq(Bool true, Bool false))) (* Shoul return Some BoolTy *)
let eq_2 = type_of empty_context (Eq(Vector [1;3;4], Num 2)) (* Shoul return None*)
let eq_3 = type_of ctx (Eq(Var "d2", Var "d1")) (* Shoul return BoolTy *)
let eq_4 = type_of ctx (Eq(Var "d3", Var "x")) (* Should return None *)


let ctx = update_context ctx "v" VectorTy

(* Tests for Var *)

let var_0 = type_of ctx (Var "d1") (* Shoul return Some COOTy *)
let var_1 = type_of ctx (Var "x") (* Shoul return None *)
let var_2 = type_of ctx (Var "v") (* Shoul return Some VectorTy *)


(* Test for Trace *)

let tr_0 = type_of ctx (TraceCOO (Var "d1")) (* Shoul return Some IntTy *)
let tr_1 = type_of ctx (TraceCOO (Var "x")) (* Shoul return None *)
let tr_2 = type_of ctx (TraceCOO (Var "v")) (* Shoul return None *)

(*Test for Sparsity*)

let sp_0 = type_of ctx (SparsityCOO (Var "d1")) (* Shoul return Some FloatTy *)
let sp_1 = type_of ctx (SparsityCOO (Var "x")) (* Shoul return None *)
let sp_2 = type_of ctx (SparsityCOO (Var "v")) (* Shoul return None *)

(*Test for Density*)

let den_0 = type_of ctx (DensityCOO (Var "d1")) (* Shoul return Some FloatTy *)
let den_1 = type_of ctx (DensityCOO (Var "x")) (* Shoul return None *)
let den_2 = type_of ctx (DensityCOO (Var "v")) (* Shoul return None *)

