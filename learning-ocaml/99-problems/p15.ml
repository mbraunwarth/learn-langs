(* Replicate the elements of a list a given number of times *)
let rec replicate (xs: 'a list) (n: int): 'a list =
  if n <= 0 then []
  else if n = 1 then xs
  else
    match xs with
    | [] -> []
    | x :: rest -> more x n @ replicate rest n

let rec more (x: 'a) (n: int): 'a list = 
  if n <= 0 
  then []
  else x :: more x (n-1)

