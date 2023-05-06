(* Modified Run-Length Encoding *)

type 'a rle = 
  | One of 'a 
  | Many of int * 'a

let encode (xs: 'a list): 'a rle list = 
  let rec aux xs acc =
    let elem = match acc with
               | (1, a) -> One a
               | (n, a) -> Many (n, a)
    in
    match xs with
    | [] -> [elem]
    | x :: rest -> 
        if snd acc = x 
          then aux rest (acc |> fst |> succ, x)
          else elem :: aux rest (1, x)
  in aux (List.tl xs) (1, List.hd xs)
