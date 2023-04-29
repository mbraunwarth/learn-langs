(* Flatten a nested list structure *)

(* nested list structure *)
type 'a node =
  | One of 'a
  | Many of 'a node list


(* TODO solve without stdlib functions *)
let rec flatten (nodes: 'a node): 'a list =
  match nodes with
  | One x -> [x]
  | Many xs -> xs |> List.map flatten |> List.concat
