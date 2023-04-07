(* Problem 01 *)

let rec last (xs: 'a list): 'a option =
  match xs with
  | []  -> None
  | [x] -> Some x
  | _ :: rest -> last rest

(* Problem 02 *)

let rec last_two (xs: 'a list): ('a * 'a) option =
  match xs with
  | [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: rest -> last_two rest

(* Problem 03 *)

let rec at (xs: 'a list) (n: int): 'a option =
  if n < 1 then None else
  match xs with
  | x :: _ when n == 1 -> Some x
  | _ :: rest when n > 1 -> at rest (n-1)
  | _ -> None
  
(* Problem 04 *)
let rec length (xs: 'a list) : int =
  match xs with 
  | [] -> 0
  | _ :: rest -> 1 + length rest

let rec len_tr (xs: 'a list) (acc: int) : int =
  match xs with
  | [] -> acc
  | _ :: rest -> len_tr rest (acc + 1)

let len (xs: 'a list) : int =
  len_tr xs 0 

(* Problem 07 *)
type 'a node = 
  | One of 'a 
  | Many of 'a node list

let flatten (n: 'a node) : 'a list =
  match n with
  | One  x         -> [x]
  | Many (x :: xs) -> (flatten x) @ (map flatten xs)

let rec map (f: 'a -> 'b) (xs: 'a list): 'b list =
  match xs with
  | [] -> []
  | x :: rest -> (f x) :: map f rest

