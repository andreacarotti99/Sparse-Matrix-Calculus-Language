(* Tests function eval_exp that evaluates all the expressions *)
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


let eval_state = update_state (update_state empty_state "x" (Val(IntVal(4)))) "d1" (Val(COOVal decl))

(* Tests for Add *)
let add_0 = eval_exp (Add(Num 1, Num 2)) empty_state (* Should return Some IntVal 3 *)
let add_1 = eval_exp (Add(Num 1, Vector [1;3;4])) empty_state (* Should return None *)
let add_2 = eval_exp (Add(Var "d1", Num 1)) empty_state (* Should return None *)
let add_3 = eval_exp (Add(Bool true, Vector [1;3;4])) empty_state (* Should return None *)
let add_4 = eval_exp (Add(Num 1, Var "x")) eval_state (* Should return Some IntVal 5 *)



(* Tests for Sub *)


let sub_0 = eval_exp (Sub(Num 2, Num 1)) empty_state (* Should return Some IntVal 1 *)
let sub_1 = eval_exp (Sub(Num 1, Vector [1;3;4])) empty_state (* Should return None *)
let sub_2 = eval_exp (Sub(Var "d1", Num 1)) empty_state (* Should return None *)
let sub_3 = eval_exp (Sub(Bool true, Vector [1;3;4])) empty_state (* Should return None *)
let sub_4 = eval_exp (Sub(Num 1, Var "x")) eval_state (* Should return Some IntVal -3 *)

let eval_state = update_state eval_state "d2" (Val(COOVal decl))

(* Tests for Eq *)
let eq_0 = eval_exp (Eq(Num (-3), Num 2)) empty_state (* Should return Some BoolVal false *)
let eq_1 = eval_exp (Eq(Num 2, Num 2)) empty_state (* Should return Some BoolVal true *)
let eq_2 = eval_exp (Eq(Vector [1;3;4], Num 2)) empty_state (* Should return Some BoolVal false *)
let eq_3 = eval_exp (Eq(Var "d2", Var "d1")) eval_state (* Should return Some (BoolVal true)*)
let eq_4 = eval_exp (Eq(Var "d3", Var "x")) eval_state (* Should return None*)


(* Tests for And *)
let eval_state = update_state eval_state "y" (Val(BoolVal(true)))

let and_0 = eval_exp (And(Bool true, Bool false)) empty_state (* Should return Some BoolVal false *)
let and_1 = eval_exp (And(Bool true, Bool true)) empty_state (* Should return Some BoolVal true *)
let and_2 = eval_exp (And(Bool true, Eq (Vector [1;3;4], Vector [2;3;4]))) empty_state (* Should return Some BoolVal false *)
let and_3 = eval_exp (And(Num 1, Bool false)) eval_state (* Should return None *)
let and_4 = eval_exp (And(Bool true, Var "y")) eval_state (* Should return Some BoolVal true *)



(* Tests for Or *)

let or_0 = eval_exp (Or(Bool false, Bool false)) empty_state (* Should return Some BoolVal false *)
let or_1 = eval_exp (Or(Bool true, Bool false)) empty_state (* Should return Some BoolVal true *)
let or_2 = eval_exp (Or(Bool false, Eq(Vector [1;3;4], Vector [2;3;4]))) empty_state (* Should return Some BoolVal false *)
let or_3 = eval_exp (Or(Num 1, Bool false)) eval_state (* Should return None *)
let or_4 = eval_exp (Or(Bool false, Var "y")) eval_state (* Should return Some BoolVal true *)



(*Test for TraceCOO *)
let eval_state = update_state eval_state "d3" (Val(COOVal(decl2)))
let tr_0 = eval_exp (TraceCOO (Var "d1")) eval_state (*Should retturn Some Intval 10*)
let tr_1 = eval_exp(TraceCOO (Var "d3")) eval_state (*Should retturn Some Intval 3*)
let tr_2 = eval_exp (TraceCOO (Num 3)) eval_state (*Should retturn None*)
let tr_3 = eval_exp(TraceCOO (Bool true)) eval_state (*Should retturn None*)
let tr_3 = eval_exp(TraceCOO (Var "x")) eval_state (*Should retturn None*)


(*Test for DensityCOO*)

let den_0 = eval_exp (DensityCOO (Var "d1")) eval_state (*Should retturn Some FloatVal 0.5625*)
let den_1 = eval_exp(DensityCOO (Var "d3")) eval_state (*Should retturn Some FloatVal 0.66666666666666663*)
let den_2 = eval_exp (DensityCOO (Num 3)) eval_state (*Should retturn None*)
let den_3 = eval_exp(DensityCOO (Bool true)) eval_state (*Should retturn None*)
let den_3 = eval_exp(DensityCOO (Var "x")) eval_state (*Should retturn None*)


(*Test for SparsityCOO*)

let spar_0 = eval_exp (SparsityCOO (Var "d1")) eval_state (*Should retturn Some FloatVal 0.4375*)
let spar_1 = eval_exp(SparsityCOO (Var "d3")) eval_state (*Should retturn Some FloatVal 0.333333333333333315*)
let spar_2 = eval_exp (SparsityCOO (Num 3)) eval_state (*Should retturn None*)
let spar_3 = eval_exp(SparsityCOO (Bool true)) eval_state (*Should retturn None*)
let spar_3 = eval_exp(SparsityCOO (Var "x")) eval_state (*Should retturn None*)


