let rec last (l: 'a list) : 'a option =
  match l with
  | [] -> None
  | x :: [] -> Some x
  | x :: xs -> last xs
