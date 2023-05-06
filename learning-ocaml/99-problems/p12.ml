(* Decode a Run-Length Encoded List *)

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
;;

let replicate n x = 
  let rec aux acc n x =
    if n = 0 then acc else aux (x :: acc) (n-1) x
  in aux [] n x
;;

let decode (list: 'a rle list): 'a list =
  (* func :: 'a rle -> 'a list *)
  let func = function
    | One x -> [x]
    | Many (n, x) -> replicate n x
  in List.map func list |> List.concat
;;

let () =
  let list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] in
  let encoding = encode list in
  Printf.printf "%b\n" (decode encoding = list)
