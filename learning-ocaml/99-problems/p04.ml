let length xs = 
  let rec length' xs' acc =
    match xs' with 
    | [] -> acc
    | _ :: rest -> length' rest (acc+1)
  in length' xs 0
