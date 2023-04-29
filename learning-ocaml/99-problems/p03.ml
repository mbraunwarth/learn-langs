let rec nth (l: 'a list) (n: int) : 'a option =
  match l with
  | [] -> None
  | x :: xs -> 
      if n == 0 then Some x
      else nth xs (n-1)

