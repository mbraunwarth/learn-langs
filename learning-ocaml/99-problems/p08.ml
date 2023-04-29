(* Eliminate _consecutive_ duplicates of list elements *)
let compress (xs: 'a list): 'a list = 
  let rec compress' (xs: 'a list) (acc: 'a list): 'a list =
    match xs with
    | [] -> acc
    | x :: [] -> acc @ [x]
    | x :: y :: rest when x = y ->
        compress' (y :: rest) acc
    | x :: y :: rest ->
        compress' (y :: rest) (acc @ [x])
  in compress' xs []


