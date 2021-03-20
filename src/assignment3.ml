open Ast
open Eval

type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l

(**********************************)
(* Problem 1: Tree In-order Fold  *)
(**********************************)

let rec fold_inorder f acc t =
  acc


(*****************************************)
(* Problem 2: Tree Level-order Traversal *)
(*****************************************)

let levelOrder t =
  []

(***************************************)
(* Problem 3: Tail-recursive Tree Sum  *)
(***************************************)

let rec sum_tree t =
  match t with
  | Leaf -> 0
  | Node (l, x, r) -> sum_tree l + x + sum_tree r

let sumtailrec t = 0

(******************************)
(* Problem 4: Imp Interperter *)
(**** Your code in eval.ml ****)
(******************************)

(* Parse a file of Imp source code *)
let load (filename : string) : Ast.com =
  let ch =
    try open_in filename
    with Sys_error s -> failwith ("Cannot open file: " ^ s) in
  let parse : com =
    try Parser.main Lexer.token (Lexing.from_channel ch)
    with e ->
      let msg = Printexc.to_string e
      and stack = Printexc.get_backtrace () in
      Printf.eprintf "there was an error: %s%s\n" msg stack;
      close_in ch; failwith "Cannot parse program" in
  close_in ch;
  parse

(* Interpret a parsed AST with the eval_command function defined in eval.ml *)
let eval (parsed_ast : Ast.com) : environment =
  let env = [] in
  eval_command parsed_ast env


(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for Problem 1 *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for Problem 2 *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for Problem 3 *)
  let _ =
    try
      let tree =
        let rec loop tree i =
          if i = 1000 then tree else loop (insert tree (Random.int 1000)) (i+1) in
        loop Leaf 0 in
      assert (sumtailrec tree = sum_tree tree)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for Problem 4 *)
  let _ =
    try
      let parsed_ast = load ("programs/aexp-add.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- x => 10\n\
         - y => 15\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/aexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- w => -13\n\
         - x => 1\n\
         - y => 2\n\
         - z => 3\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/bexp-combined.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- res1 => 1\n\
         - res10 => 0\n\
         - res11 => 0\n\
         - res12 => 0\n\
         - res13 => 1\n\
         - res14 => 1\n\
         - res15 => 1\n\
         - res16 => 0\n\
         - res2 => 0\n\
         - res3 => 1\n\
         - res4 => 0\n\
         - res5 => 0\n\
         - res6 => 1\n\
         - res7 => 0\n\
         - res8 => 0\n\
         - res9 => 1\n\
         - w => 5\n\
         - x => 3\n\
         - y => 5\n\
         - z => -3\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/cond.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n1 => 255\n\
         - n2 => -5\n\
         - res1 => 1\n\
         - res2 => 255\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/fact.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f => 120\n\
         - n => 1\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/fib.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- f0 => 5\n\
         - f1 => 8\n\
         - k => 6\n\
         - n => 5\n\
         - res => 8\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/for.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- i => 101\n\
         - n => 101\n\
         - sum => 5151\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/palindrome.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 135\n\
         - res => 1\n\
         - res2 => 0\n\
         - reverse => 123454321\n\
         - reverse2 => 531\n\
         - temp => 0\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  let _ =
    try
      let parsed_ast = load ("programs/while.imp") in
      let result = print_env_str(eval (parsed_ast)) in
      assert(result =
        "- n => 0\n\
         - sum => 5050\n");
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  Printf.printf ("%d out of 12 programming questions are incorrect.\n") (!error_count)

let _ = main()
