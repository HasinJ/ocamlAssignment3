open Ast

exception TypeError
exception UndefinedVar
exception DivByZeroError

(* Remove shadowed bindings *)
let prune_env (env : environment) : environment =
  let binds = List.sort_uniq compare (List.map (fun (id, _) -> id) env) in
  List.map (fun e -> (e, List.assoc e env)) binds

(* Env print function to stdout *)
let print_env_std (env : environment): unit =
  List.iter (fun (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      Printf.printf "- %s => %s\n" var vs) (prune_env env)

(* Env print function to string *)
let print_env_str (env : environment): string =
  List.fold_left (fun acc (var, value) ->
      let vs = match value with
        | Int_Val(i) -> string_of_int i
        | Bool_Val(b) -> string_of_bool b in
      acc ^ (Printf.sprintf "- %s => %s\n" var vs)) "" (prune_env env)



(***********************)
(****** Your Code ******)
(***********************)

(* evaluate an expression in an environment *)
let rec eval_expr (e : exp) (env : environment) : value =
    Int_Val 0


(* evaluate a command in an environment *)
let rec eval_command (c : com) (env : environment) : environment =
    []
