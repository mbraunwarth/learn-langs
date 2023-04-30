(* Pack consecutive duplicates of list elements into sublists *)
let pack (xs: 'a list): ('a list list) = 
  let rec pack' xs acc =
    match xs with
    | [] -> [[]]
    | x :: y :: rest when x = y ->
        pack' (y :: rest) (x :: acc)
    | x :: y :: rest ->
        (x :: acc) :: (pack' (y :: rest) [])
    | x :: [] -> 
        match acc with
        | [] -> [xs]
        | y :: _ when x = y -> [x :: acc]
        | _ -> acc :: [[x]]
  in pack' xs []
