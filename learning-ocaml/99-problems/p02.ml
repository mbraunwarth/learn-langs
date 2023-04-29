let rec last_two (l: 'a list) : ('a * 'a) option =
  match l with
  | x :: y :: [] -> Some (x, y)
  | x :: [] -> None
  | [] -> None
  | x :: xs -> last_two xs
