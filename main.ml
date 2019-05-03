open Core;;

(* dune build main.exe && ./_build/default/main.exe *)

let trimLine (s: string) = 
  int_of_string ( String.strip s )

let fileOpen input = 
  List.fold2_exn 
    (In_channel.read_lines (input ^ "_w.txt")) 
    (In_channel.read_lines (input ^ "_v.txt"))
    ~init:(Array.create ~len:0 (0, 0)) ~f:(fun init weight value -> 
      Array.append init (Array.create ~len:1 ( trimLine weight,  trimLine value ))) 

let fileWeight input =
  In_channel.read_all (input ^ "_c.txt") |> trimLine

let getWeight bag i = let ( w, _ ) = bag.(i) in w 
let getValue  bag i = let ( _, v ) = bag.(i) in v 

let functionRunner f bag weight =
  let t = Unix.gettimeofday () in
  f bag weight;
  Printf.printf "Time Taken: %fs\n" (Unix.gettimeofday () -. t);
;;


let btbag ~table ~bag = 
  let x = Array.length table - 1 in 
  let y = Array.length table.(0) - 1 in 
  let rec backtrack x y = 
    if table.(x).(y) = 0 then []
    else if y - getWeight bag (x - 1) < 0 then [1]
    else match ( table.(x - 1).(y) > table.(x - 1).(y - getWeight bag (x - 1)) + getValue bag (x - 1) ) with
    | true -> backtrack (x - 1) y
    | false -> x::backtrack (x - 1) (y - getWeight bag (x - 1))
  in backtrack x y ;;


let rec recurse lst = 
  match lst with 
  | [] -> Printf.printf "\n"
  | hd::tl -> Printf.printf "%d " hd; recurse tl
  (* | hd::tl -> print_endline (string_of_int hd); recurse tl *)
;;

let t1a bag weight = 
  let matrix = Array.make_matrix ~dimx:((Array.length bag) + 1) ~dimy:(weight + 1) 0 in 
  for i = 1 to (Array.length matrix) - 1 do 
    for j = 0 to (Array.length matrix.(i)) - 1 do
      match (j < (getWeight bag (i - 1))) with 
      | true  -> matrix.(i).(j) <- matrix.(i - 1).(j)
      | false -> 
          matrix.(i).(j) <- Pervasives.max 
          ( matrix.(i - 1).(j - (getWeight bag (i - 1))) + getValue bag (i - 1) )
          ( matrix.(i - 1).(j) )
    done;
  done;
  Printf.printf "Optimal subset: ";
  recurse (btbag ~table:matrix ~bag:bag);
  print_endline ("Optimal Value " ^ string_of_int (Array.last (Array.last matrix)));
  (* Array.last (Array.last matrix) *)
;;

let t1b bag weight = 
  let matrix = Array.make_matrix ~dimx:((Array.length bag) + 1) ~dimy:(weight + 1) (-1) in
  Array.map_inplace matrix.(0) ~f:(fun _ -> 0);
  let rec knapsack i j = 
    match matrix.(i).(j) < 0 with 
    | true -> 
      (match j < (getWeight bag (i - 1)) with 
        | true -> matrix.(i).(j) <- knapsack (i - 1) j; matrix.(i).(j)
        | false -> matrix.(i).(j) <- Pervasives.max 
          (knapsack (i - 1) j) ((getValue bag (i - 1)) + knapsack (i - 1) (j - (getWeight bag (i - 1)))) ;
          matrix.(i).(j))
    | false -> matrix.(i).(j)
  in knapsack (Array.length bag) weight;
;;

let foo bag weight =
  let a = Array.mapi bag 
    ~f:(fun idx (w, v) -> (
      float_of_int v /. float_of_int w,
      getValue bag idx,
      getWeight bag idx,
      idx
    )) in 
  Array.sort a ~compare:(fun (x, _, _, _) (y, _, _, _) -> if x = y then 0 else if x > y then -1 else 1);
  let rec greedy idx currentValue weightLeft = 
    let ( _ , value, weight, _ ) = a.(idx) in 
    match (idx = Array.length bag, weightLeft >= 0, weight >= weightLeft) with 
    | ( _, _, true ) -> currentValue 
    | ( _, false, _ ) -> 0
    | ( false, true, _ ) -> greedy (idx + 1) (currentValue + value) (weightLeft - weight)
    | ( true, true, _ ) -> currentValue
  in greedy 0 0 weight;
;;


let main = 
  let input = ref "" in 
  input := In_channel.(input_line_exn stdin);
  let weight = fileWeight !input in 
  let backpack = fileOpen !input in 
  print_endline (string_of_int (foo backpack weight))
  (* functionRunner t1a backpack weight; *)
  (* foo backpack weight;; *)
  ;;



(* Traditional Dynamic Programming Optimal value: 1458
Traditional Dynamic Programming Optimal subset: {1, 3, 5, 7, 8, 9, 14, 15}
Traditional Dynamic Programming Time Taken: <corresponding output here> *)


(* module Htbl = struct 
  let size = 10
  let getTbl = Array.init size ~f:(fun _ -> Avltree.empty )
end  *)

(* let main =  *)
  (* let _ = Avltree.empty (1, 1) in
  print_endline "done";
  print_endline (string_of_int Htbl.size); *)
  (* let f = Avltree.empty in 
  let _ = Avltree.add f ~replace:false ~compare:(fun _ _ -> 1) ~key:5 ~data:10 in 
  let _ = Array.init 10 ~f:(fun _ -> Avltree.empty) in
  print_endline "done";; *)

(* W      |   v
1           1
4           3   
5           4
7           5

Vi / Wi *)