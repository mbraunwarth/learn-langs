let is_palindrome (xs: 'a list): bool = 
  let rec rev xs' =
    match xs' with
    | [] -> []
    | x :: rest -> rev rest @ [x]
  in 
  rev xs = xs
