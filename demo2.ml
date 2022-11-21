(*
All the code have been tested with Numpy, the result can be checked here:
https://colab.research.google.com/drive/1g8-n-qXGzQVnhnQB8ve4fr5QzpK1amxb?usp=sharing
*)

let big_vec_A = [
  0; 1; 2; 1; 2; 0; 0; 2; 1; 3;
  3; 1; 2; 2; 0; 1; 2; 3; 2; 1; 
  3; 1; 2; 1; 1; 2; 3; 2; 0; 0; 
  1; 3; 0; 0; 2; 2; 3; 1; 0; 1; 
  0; 0; 0; 1; 1; 2; 1; 1; 3; 1; 
  3; 2; 2; 2; 2; 3; 2; 1; 0; 1; 
  0; 2; 1; 2; 0; 2; 3; 0; 1; 2; 
  2; 0; 2; 2; 3; 3; 2; 2; 1; 3; 
  0; 3; 2; 0; 1; 0; 1; 0; 0; 2; 
  3; 2; 2; 3; 1; 1; 3; 1; 0; 2
]
let big_vec_B = [ (*still an array*)
  2; 6; 1; 3; 5; 2; 5; 6; 9; 1;
  0; 4; 4; 3; 0; 1; 0; 0; 0; 2;
  0; 1; 0; 2; 0; 4; 1; 0; 0; 0;
  1; 2; 0; 0; 1; 3; 1; 0; 0; 7;
  0; 0; 0; 0; 4; 0; 2; 5; 8; 1;
  0; 0; 3; 1; 2; 3; 2; 1; 0; 1;
  0; 6; 1; 7; 4; 0; 0; 0; 1; 4;
  0; 0; 1; 5; 0; 0; 0; 4; 6; 6;
  0; 0; 4; 5; 1; 4; 0; 0; 0; 1;
  7; 6; 5; 1; 0; 0; 0; 0; 4; 3
]
let big_x = [
  1; 0; 2; 2; 1; 2; 3; 1; 1; 0
]

let gamma : context = empty_context
let op1 = Assign("big_vec_A", Vector big_vec_A)
let op2 = CreateCOO("A", Num 10, Num 10, Var "big_vec_A")
let op3 = Assign("x", Vector big_x)
let op4 = MatMulCOO("y", Var "A", Var "x")
let op5 = Assign("big_vec_B", Vector big_vec_B)
let op6 = CreateCOO("B", Num 10, Num 10, Var "big_vec_B")
let op7 = MatMulCOO("C", Var "A", Var "B")
let op8 = MatSumCOO("D", Var "A", Var "B")
let op9 = MatSubCOO("E", Var "A", Var "B")
let op10 = Assign("trace_A", TraceCOO(Var "A"))
let op11 = Assign("density_A", DensityCOO(Var "A"))
let op12 = Assign("sparsity_A", SparsityCOO(Var "A"))

let demo2 = 
  Seq(  
    Seq(
      Seq(
        Seq(op1, op2), 
        Seq(op3, op4)
        ),
      Seq(
        Seq(op5, op6),
        Seq(op7, op8)
      )
    ),
    Seq(
      Seq(
        Seq(op9,op10),
        Seq(op11,op12)
      ),
      Skip
    )
  )


let (res_c, res_k, res_s) = run_prog demo2 empty_state;;

lookup_state res_s "y";;
lookup_state res_s "C";;
lookup_state res_s "D";;
lookup_state res_s "E";;
lookup_state res_s "trace_A";;
lookup_state res_s "density_A";;
lookup_state res_s "sparsity_A";;


