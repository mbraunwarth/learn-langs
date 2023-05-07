(* Drop every Nth element of a list *)

let drop list n =
  let rec aux acc list m =
    match list with
    | [] -> acc
    | x :: rest -> 
      if m = 1 then aux acc rest n
      else aux (x :: acc) rest (m-1)
  in List.rev (aux [] list n)

