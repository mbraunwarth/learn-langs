(* Run-Length Encoding 

 Example:
   # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   - : (int * string) list = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)

let encode (xs: 'a list): (int * 'a) list = 
  let rec aux xs acc =
    match xs with
    | [] -> [acc]
    | x :: rest -> 
        if snd acc = x 
          then aux rest (acc |> fst |> succ, x)
          else acc :: aux rest (1, x)
  in aux (List.tl xs) (1, List.hd xs)
