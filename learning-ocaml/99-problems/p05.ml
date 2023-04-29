let rec rev (xs: 'a list): 'a list = 
  match xs with
  | [] -> []
  | x :: rest -> (rev rest) @ [x]
