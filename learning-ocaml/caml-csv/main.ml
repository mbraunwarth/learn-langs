(* parse csv files to data structure in memory *)

(* 
 * CSV File Structure
 * header1,header2,...,headerN
 * field1_1,field1_2,...,filed1_n
 * ....
 * filedN_1,fieldN_2,...,fieldN_N
 *)

type csv_data = 
  { size   : int * int;
    header : string list;
    fields : string list list;
  }

let read_file_to_string (fname: string) : string =
  let ic = open_in fname in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  content
;;

let csv_printer out data =
  let (x, y) = data.size in
  let h = String.concat "; " data.header in
  Printf.fprintf out "{ size: (%d, %d), header: [%s] }" x y h
;;

let read_csv (fname: string) : csv_data =
  let content = 
    fname
    |> read_file_to_string
    |> String.split_on_char '\n' 
    |> List.filter (fun x -> String.length x > 0)
    |> List.map (String.split_on_char ',')
  in
  let hdr = List.hd content in
  let fls = List.tl content in
  { size = (List.length hdr, List.length fls);
    header = hdr;
    fields = fls;
  }
;;

let () =
  "sample.csv" 
  |> read_csv
  |> Printf.printf "%a\n" csv_printer
;;
