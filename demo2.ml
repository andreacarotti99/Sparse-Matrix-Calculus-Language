let big_vec = [
  0; 1; 2; 1; 2; 0; 0; 2; 1; 3; 3; 1; 2; 2; 0; 1; 2; 3; 2; 1; 3; 1; 2; 1; 1; 2; 3; 2; 0; 0; 1; 3; 0; 0; 2; 2; 3;
  1; 0; 1; 0; 0; 0; 1; 1; 2; 1; 1; 3; 1; 3; 2; 2; 2; 2; 3; 2; 1; 0; 1; 0; 2; 1; 2; 0; 2; 3; 0; 1; 2; 2; 0; 2; 2;
  3; 3; 2; 2; 1; 3; 0; 3; 2; 0; 1; 0; 1; 0; 0; 2; 3; 2; 2; 3; 1; 1; 3; 1; 0; 2
]
let big_x = [
  1; 0; 2; 2; 1; 2; 3; 1; 1; 0
]

let gamma : context = empty_context
let op1 = Assign("big_vec", Vector big_vec)
let op2 = CreateCOO("A", Num 10, Num 10, Var "big_vec")
let op3 = Assign("x", Vector big_x)
let op4 = MatMulCOO("y", Var "A", Var "x")

let prog2 = 
  Seq(
    Seq(op1, op2), 
    Seq(op3, op4)
    )


let (res_c, res_k, res_s) = run_prog prog2 empty_state;;

lookup_state res_s "y";; (*should return [11 24 25 17 14 26 20 28 8 26] - tested with numpy *)
