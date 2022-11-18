(*Test for cmd*)

(* Assign test *)

let state0 = empty_state ;;
let config_asgn_0 = (Assign ("x", Sub(Num 4, Num 3)), [(state0, "__exit")], empty_state)
let (asgn0_c, asgn0_k, asgn0_s) = run_config config_asgn_0;;
lookup_state asgn0_s "x";; (*Should return Some Val Intval 1*)

let config_asgn_1 = (Assign ("x", Vector [2;3;4;5]), [(state0, "__exit")], empty_state)
let (asgn1_c, asgn1_k, asgn1_s) = run_config config_asgn_1;;
lookup_state asgn1_s "x";; (*Should return Some Val VectorVal [2,3,4,5]*)

(* Seq test*)

let m0 = [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4];;
let m1 = [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4];;

let config_seq_0 = step_cmd (Seq(Skip, CreateCOO("l",Num 4, Num 4, Vector m0))) [(state0, "__exit")] empty_state
 (*Should return   Some
   (CreateCOO ("l", Num 4, Num 4,
     Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),
    [(<fun>, "y")], <fun>)*)

let config_seq_1 = step_cmd (Seq(Seq(CreateCOO("z",Num 4, Num 4, Vector m0),CreateCOO("s",Num 4, Num 4, Vector m1)),MatSubCOO("c",Var "z",Var "s"))) [(state0, "__exit")] empty_state

 (*Should return   Some
   (Seq
     (Seq (Skip,
       CreateCOO ("s", Num 4, Num 4,
        Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4])),
     MatSubCOO ("c", Var "z", Var "s")),
    [(<fun>, "y")], <fun>)*)

(*IfC test*)

let config_ifc_0 = step_cmd (IfC(Eq(Num 3, Num 3), CreateCOO("l",Num 4, Num 4, Vector m0), CreateCOO("s",Num 4, Num 4, Vector m1))) [(state0, "__exit")] empty_state
  (*Should return    Some (CreateCOO ("l", Num 4, Num 4,
     Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),
    [(<fun>, "y")], <fun>)*)
let config_ifc_1 = step_cmd (IfC(Eq(Num 4, Num 3), CreateCOO("l",Num 4, Num 4, Vector m0), CreateCOO("s",Num 4, Num 4, Vector m1))) [(state0, "__exit")] empty_state
(*Should return Some (CreateCOO ("s", Num 4, Num 4,
     Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),
    [(<fun>, "y")], <fun>)*)

(*While test*)

let config_w_0 = step_cmd (While(Eq(Num 3, Num 3), CreateCOO("l",Num 4, Num 4, Vector m0))) [(state0, "__exit")] empty_state
  (*Should return    Some (IfC (Eq (Num 4, Num 3),
     Seq
      (CreateCOO ("s", Num 4, Num 4,
        Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),
      While (Eq (Num 4, Num 3),
       CreateCOO ("s", Num 4, Num 4,
        Vector [1; 7; 0; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]))),
     Skip),
    [(<fun>, "y")], <fun>)*)
let config_w_1 = step_cmd (While(Eq(Num 4, Num 3), CreateCOO("s",Num 4, Num 4, Vector m1))) [(state0, "__exit")] empty_state
(*Should return Some (IfC (Eq (Num 4, Num 3),
     Seq
      (CreateCOO ("s", Num 4, Num 4,
        Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]),
      While (Eq (Num 4, Num 3),
       CreateCOO ("s", Num 4, Num 4,
        Vector [1; 7; 3; 0; 0; 2; 8; 0; 5; 0; 3; 9; 0; 6; 0; 4]))),
     Skip),
    [(<fun>, "y")], <fun>)*)

(*Test Call and Return*)

let state0 = update_state empty_state "f" (Fun (["x"; "y"], Return (Add (Var "x", Var "y"))))
let state1 = update_state (update_state state0 "x" (Val (IntVal 9)))
    "y" (Val (IntVal 6))
    
let config1 = (Return (Sub (Var "x", Var "y")), [(state0, "x")], state1)
let prog1 = Call ("x", "f", [Num 1; Num 2])

let (res_c, res_k, res_s) = run_config config1;;
lookup_state res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "y";; (* should return None *)

let (res_c, res_k, res_s) = run_prog prog1 state0;;
lookup_state res_s "x";; (* should return Some (Val (IntVal 3)) *)
lookup_state res_s "y";; (* should return None *)

(*Test CreateCOO*)

let config_crCOO_0 = (CreateCOO("l",Num 4, Num 4, Vector m0), [(state0, "__exit")], empty_state)
let (crCOO0_c, crCOO0_k, crCOO0_s) = run_config config_crCOO_0;;
lookup_state crCOO0_s "l";; 
(*Should return Some
 (Val
   (COOVal
     {rows = Vector [0; 0; 1; 1; 2; 2; 2; 3; 3];
      cols = Vector [0; 1; 1; 2; 0; 2; 3; 1; 3];
      data = Vector [1; 7; 2; 8; 5; 3; 9; 6; 4]; n_rows = 4; n_cols = 4}))*)

(*Test MatSumCOO*)

let r1 = [0; 0; 1; 1]
let c1 = [1; 2; 0; 2]
let d1 = [10; 12; 1; 2]
let r2 = [0; 0; 1; 2]
let c2 = [0; 1; 1; 0]
let d2 = [2; 5; 1; 8]
let decl1 : coodecl = {rows = Vector r1; cols = Vector c1; data = Vector d; n_rows = 3; n_cols = 3}
let empty_decl : coodecl = {rows = Vector []; cols = Vector []; data = Vector []; n_rows = 0; n_cols = 0}
let decl2 : coodecl = {rows = Vector r2; cols = Vector c2; data = Vector d2; n_rows = 3; n_cols = 3}
let state2 = update_state (update_state empty_state "l" (Val(COOVal decl1))) "n" (Val(COOVal decl2))
let config_sumCOO_0 = (MatSumCOO ("sum", Var "l", Var "n"), [(state2, "x")], state2)
let (sumCOO0_c, sumCOO0_k, sumCOO0_s) = run_config config_sumCOO_0;;
lookup_state sumCOO0_s "sum";;
      (* Should return   Some
   (Val
     (COOVal
       {rows = Vector [0; 0; 0; 1; 1; 1; 2];
        cols = Vector [0; 1; 2; 0; 1; 2; 0];
        data = Vector [2; 6; 7; 2; 1; 8; 8]; n_rows = 3; n_cols = 3}))*)

(*Test MatSubCOO*)
let config_subCOO_0 = (MatSumCOO ("sub", Var "l", Var "n"), [(state2, "x")], state2)
let (subCOO0_c, subCOO0_k, subCOO0_s) = run_config config_subCOO_0;;
lookup_state subCOO0_s "sub";;
      (* Should return Some
 (Val
   (COOVal
     {rows = Vector [0; 0; 0; 1; 1; 1; 2]; cols = Vector [0; 1; 2; 0; 1; 2; 0];
      data = Vector [2; 6; 7; 2; 1; 8; 8]; n_rows = 3; n_cols = 3}))*)

