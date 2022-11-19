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
let opA = Assign ("v6", Vector ([2; 2; 2; 2]))
let opB = Assign ("v7", Vector ([2; 1; 2; 1]))


let d1 = CreateCOO("d1", Num 4, Num 4, Var "v1")

let d2 = CreateCOO("d2", Num 4, Num 4, Var "v2")


let test_cmd_a = typecheck_cmd gamma d1 (* Should return true*)
let test_cmd_b = typecheck_cmd gamma d2 (* Should return true*)

let traceDecl = TraceCOO(Var "d1")
let densDecl = DensityCOO(Var "d5")
let sparsDecl = SparsityCOO(Var "d1")
let trace1 = Assign ("t1", traceDecl)
let dens1 = Assign ("w1", densDecl)
let spars1 = Assign ("s1", sparsDecl)


let gamma = update_context (update_context gamma "d6" COOTy) "d7" COOTy
let decl1 = {rows = Vector r2; cols = Vector c2; data = Vector data2; n_rows = 2; n_cols = 3}
let decl2 = { rows = Vector r3; cols = Vector c3; data = Vector data3; n_rows = 3; n_cols = 2}
let state0 = update_state (update_state empty_state "d5" (Val(COOVal decl1))) "d6" (Val(COOVal decl2))

let d3 = Seq(MatSumCOO("d3", Var "d1", Var "d2"),MatSubCOO("d4",Var "d1", Var "d2"))
let d5 = MatMulCOO("d7", Var "d5", Var "d6")
let d8 = MatMulCOO("d8", Var "d1", Var "v6")
let d9 = MatMulCOO("d9", Var "d1", Var "d1")


let test_cmd_d3 = typecheck_cmd gamma d3 (* Should return true*)
let test_cmd_d5 = typecheck_cmd gamma d5 (* Should return true*)


let prog1 = Seq(Seq(Seq(Seq(opA, opB) , Seq(Seq(op1, op2),Seq(Seq(d1,d2),Seq(d3,d5)))), Seq(d8, d9)), Seq (Seq(dens1, spars1), Seq(trace1, Skip)))


(*
// ...added to state d5 and d6 COO matrices 

v6 = [2; 2; 2; 2]
v7 = [2; 1; 2; 1]
v1 = [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]
v2 = [-9; 7; 3; 0;0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]
d1 = CreateCOO(4, 4, v1)
d2 = CreateCOO(4, 4, v2)
d3 = MatSumCOO(d1, d2)
d4 = MatSubCOO(d1, d2)
d7 = MatMulCOO(d5, d6)
d8 = MatMulCOO(d1, v6)
d9 = MatMul(d1, d1)
t1 = Trace(d1)
w1 = Density(d5)
s1 = Sparsity(d1)

*)


let (res_c, res_k, res_s) = run_prog prog1 state0;;

lookup_state res_s "v6";;
lookup_state res_s "v7";;
lookup_state res_s "v1";;
lookup_state res_s "v2";;
lookup_state res_s "d1";;
lookup_state res_s "d2";;
lookup_state res_s "d3";;
lookup_state res_s "d4";;
lookup_state res_s "d7";;
lookup_state res_s "d8";;
lookup_state res_s "d9";;
lookup_state res_s "t1";;
lookup_state res_s "w1";;
lookup_state res_s "s1";;

