(*
    (P49) 4.2.2 Exercise : Change the definition of the eval function in the
    arith implementation to the big-step style introduced in Exercise 3.5.17.

    Read the definition of big-step evaluation (described in textbook 3.5.17).
    Refer to Chapter 4 and the code implementation of small-step evaluation and
    multi-step evaluation below to complete the implementation of big-step evaluation.
*)

open Format

(* Data type definitions *)
type t =
  | True
  | False
  | If of t * t * t
  | Zero
  | Succ of t
  | Pred of t
  | IsZero of t

exception NoRule

let rec isNumber t =
  match t with Zero -> true | Succ t1 -> isNumber t1 | _ -> false
(* (Succ (Succ (Succ (Succ Zero)))) *)

let isValue t =
  match t with
  | True -> true
  | False -> true
  | t when isNumber t -> true
  | _ -> false

(* small-step evaluator *)
let rec eval t =
  match t with
  | If (True, t2, _) -> t2
  | If (False, _, t3) -> t3
  | If (t1, t2, t3) -> If (eval t1, t2, t3)
  | Succ t' -> Succ (eval t')
  | Pred Zero -> Zero
  | Pred (Succ t') -> if isNumber t' then t' else Pred (eval (Succ t'))
  | Pred t' -> Pred (eval t')
  | IsZero Zero -> True
  | IsZero (Succ t') -> if isNumber t' then False else IsZero (eval (Succ t'))
  | IsZero t' -> IsZero (eval t')
  | _ -> raise NoRule

(* multi-step evaluation *)
let rec evalAll t =
  try
    (* https://www2.lib.uchicago.edu/keith/ocaml-class/definitions.html *)
    let t' = eval t in
    evalAll t'
  with NoRule -> t

(* printer function *)
let rec pr t =
  match t with
  | True -> printf "True"
  | False -> printf "False"
  | If (t1, t2, t3) ->
      printf "If(";
      pr t1;
      printf ", ";
      pr t2;
      printf ", ";
      pr t3;
      printf ")"
  | Zero -> printf "Zero"
  | Succ t' ->
      printf "Succ(";
      pr t';
      printf ")"
  | Pred t' ->
      printf "Pred(";
      pr t';
      printf ")"
  | IsZero t' ->
      printf "IsZero(";
      pr t';
      printf ")"

(*
    Exercise: complete the code for big-step evaluator.
    As an example, the code implementation of XX rule in big-step evaluator
    has been given. Please complete the code implementation of other rules
    and verify whether your implementation is correct in unit test.
*)

let rec evalBig t =
  match t with
  (* Todo: B-Value *)
  | v when isValue v -> v
  (* Todo: B-IfTrue *)
  | If (True, t2, _) -> evalBig t2
  (* Todo: B-IfFalse *)
  | If (False, _, t3) -> evalBig t3
  (* Todo: B-Succ *)
  | Succ t' -> (
      let v = evalBig t' in
      match v with nv1 when isNumber nv1 -> Succ nv1 | _ -> raise NoRule)
  (* Todo: B-PredZero *)
  | Pred t' -> (
      let v = evalBig t' in
      match v with
      | Zero -> Zero
      | Succ nv1 when isNumber nv1 -> nv1
      | _ -> raise NoRule)
  (* Todo: B-PredSucc *)
  (* | Pred (Succ t') -> (
      let v = evalBig t' in
      match v with nv1 when isNumber nv1 -> nv1 | _ -> raise NoRule) *)
  (* Todo: B-IszeroZero *)
  | IsZero Zero -> True
  (* sample: B-IszeroSucc *)
  | IsZero t' -> (
      let v = evalBig t' in
      match v with Succ nv1 when isNumber nv1 -> False | _ -> raise NoRule)
  | _ -> raise NoRule

(* ======================= unit test ========================== *)
let e = Pred (Succ (Pred Zero))
let d = evalBig e
let _ = pr e
let _ = printf "\n"
let _ = pr d
let _ = printf "\n---"
let _ = printf "\n"

(*
    exercise: finish the remaining test cases and give the results.
    Todo: e = IsZero Zero
    Todo: e = IsZero (Pred (Succ (Succ Zero)))
    Todo: e = If ( False, False, Pred (Succ (Pred Zero)))
*)

(* IsZero Zero *)
let e = IsZero Zero
let d = evalBig e
let _ = pr e
let _ = printf "\n"
let _ = pr d
let _ = printf "\n---"
let _ = printf "\n"

(* IsZero (Pred (Succ (Succ Zero))) *)
let e = IsZero (Pred (Succ (Succ Zero)))
let d = evalBig e
let _ = pr e
let _ = printf "\n"
let _ = pr d
let _ = printf "\n---"
let _ = printf "\n"

(* If ( False, False, Pred (Succ (Pred Zero))) *)
let e = If (False, False, Pred (Succ (Pred Zero)))
let d = evalBig e
let _ = pr e
let _ = printf "\n"
let _ = pr d
let _ = printf "\n---"
let _ = printf "\n"

(* test *)
let rec isNumberDeduceStep t =
  pr t;
  printf "\n";
  match t with Zero -> true | Succ t1 -> isNumberDeduceStep t1 | _ -> false

let res = isNumberDeduceStep (Succ (Succ (Succ (Succ Zero))))

let _ =
  printf "isNumberDeduceStep: (Succ (Succ (Succ (Succ Zero)))) = %s\n"
    (if res then "true" else "false")
