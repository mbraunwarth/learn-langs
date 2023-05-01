(* Duplicate the elements of a list *)
let rec duplicate (xs: 'a list): 'a list =
  match xs with
  | [] -> []
  | x :: rest -> x :: (x :: duplicate rest)

