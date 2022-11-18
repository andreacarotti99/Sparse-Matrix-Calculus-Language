let r2 = [0; 0; 1; 1]
let c2 = [1; 2; 0; 2]
let data2 = [10; 12; 1; 2]
let r3 = [0; 0; 1; 2]
let c3 = [0; 1; 1; 0]
let data3 = [2; 5; 1; 8]
let gamma : context = empty_context
let gamma : context = update_context (update_context gamma "d1" COOTy) "v1" VectorTy
let gamma : context = update_context (update_context gamma "d2" COOTy) "v2" VectorTy
let gamma : context = update_context (update_context gamma "d3" COOTy) "v3" VectorTy
let gamma : context = update_context (update_context gamma "d4" COOTy) "v4" VectorTy
let gamma : context = update_context (update_context gamma "d5" COOTy) "v5" VectorTy

(*Typecheck cmd*)

let op1 = Assign("v1", Vector ([1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]))
let op2 = Assign("v2", Vector([-9; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]))
let op3 = Assign ("v3", Vector([0; 10; 12; 1; 0; 2]))
let op4 = Assign("v4", Vector([2; 5; 0; 1; 8; 0]))
let op5 = Assign("v5", Vector([0; 0; 1; 0; 3; 5]))


let d1 = CreateCOO("d1", Num 4, Num 4, Var "v1")

let d2 = CreateCOO("d2", Num 4, Num 4, Var "v2")


let test_cmd_a = typecheck_cmd gamma d1 (* Should return true*)
let test_cmd_b = typecheck_cmd gamma d2 (* Should return true*)

let op6 = TraceCOO(Var "d1")
let op7 = DensityCOO(Var "d5")
let op8 = SparsityCOO(Var "d1")
let gamma = update_context (update_context gamma "d6" COOTy) "d7" COOTy
let decl1 = {rows = Vector r2; cols = Vector c2; data = Vector data2; n_rows = 2; n_cols = 3}
let decl2 = { rows = Vector r3; cols = Vector c3; data = Vector data3; n_rows = 3; n_cols = 2}
let state0 = update_state (update_state empty_state "d5" (Val(COOVal decl1))) "d6" (Val(COOVal decl2))

let d3 = Seq(MatSumCOO("d3", Var "d1", Var "d2"),MatSubCOO("d4",Var "d1", Var "d2"))
let d5 = MatMulCOO("d7", Var "d5", Var "d6")

let test_cmd_d3 = typecheck_cmd gamma d3 (* Should return true*)
let test_cmd_d5 = typecheck_cmd gamma d5 (* Should return true*)

let prog1 = Seq(Seq(op1, op2),Seq(Seq(d1,d2),Seq(d3,d5)))

let (res_c, res_k, res_s) = run_prog prog1 state0;;

lookup_state res_s "d3";;
lookup_state res_s "d4";;
lookup_state res_s "d7";;
