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

(* Test for Assign *)
let ctx = update_context ctx "x" IntTy
let asgn_0 = typecheck_cmd ctx (Assign("v", Vector ([1;2;3]))) (* Should return true*)
let asgn_1 = typecheck_cmd ctx (Assign("d1", Var "d2")) (* Should return true*)
let asgn_2 = typecheck_cmd ctx (Assign("d1", Num 7)) (* Should return false*)

(*Test for CreateCOO*)
let ctx = update_context ctx "d3" COOTy
let crcoo_0 = typecheck_cmd ctx (CreateCOO("d1",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return true*)
let crcoo_1 = typecheck_cmd ctx (CreateCOO("d1",Var "x", Num 4, Var "v")) (* Should return true*)
let crcoo_2 = typecheck_cmd ctx (CreateCOO("x",Num 4, Num 4, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return false*)
let crcoo_3 = typecheck_cmd ctx (CreateCOO("d1",Num 4,Bool true, Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return false*)
let crcoo_3 = typecheck_cmd ctx (CreateCOO("d1",Num 4,Num 2, Num 3 )) (* Should return false*)

(*Test MatSumCOO*)

let sumcoo_0 = typecheck_cmd ctx (MatSumCOO("d1",Var "d1", Var "d2")) (* Should return true*)
let sumcoo_1 = typecheck_cmd ctx (MatSumCOO("d3",Var "d1", Var "d2")) (* Should return true*)
let sumcoo_2 = typecheck_cmd ctx (MatSumCOO("v",Var "d1", Var "d2")) (* Should return false*)
let sumcoo_3 = typecheck_cmd ctx (MatSumCOO("d1",Num 3, Var "d2")) (* Should return false*)
let sumcoo_4 = typecheck_cmd ctx (MatSumCOO("d1",Var "d1", Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return false*)

(*Test MatSubCOO*)

let subcoo_0 = typecheck_cmd ctx (MatSubCOO("d1",Var "d1", Var "d2")) (* Should return true*)
let subcoo_1 = typecheck_cmd ctx (MatSubCOO("d3",Var "d1", Var "d2")) (* Should return true*)
let subcoo_2 = typecheck_cmd ctx (MatSubCOO("v",Var "d1", Var "d2")) (* Should return false*)
let subcoo_3 = typecheck_cmd ctx (MatSumCOO("d1",Num 3, Var "d2")) (* Should return false*)
let subcoo_4 = typecheck_cmd ctx (MatSubCOO("d1",Var "d1", Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return false*)

(*Test MatMulCOO*)

let mulcoo_0 = typecheck_cmd ctx (MatMulCOO("d1",Var "d1", Var "d2")) (* Should return true*)
let mulcoo_1 = typecheck_cmd ctx (MatMulCOO("d3",Var "d1", Var "d2")) (* Should return true*)
let mulcoo_2 = typecheck_cmd ctx (MatMulCOO("v",Var "d1", Var "d2")) (* Should return false*)
let mulcoo_3 = typecheck_cmd ctx (MatMulCOO("d1",Num 3, Var "d2")) (* Should return false*)
let mulcoo_4 = typecheck_cmd ctx (MatMulCOO("v",Var "d1", Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4] )) (* Should return true*)
