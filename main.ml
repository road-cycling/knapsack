open Core;;
open Core_kernel;;
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

let functionRunner f bag weight print  =
  let t = Unix.gettimeofday () in
  f bag weight;
  Printf.printf "%s Time Taken: %fs\n\n" print (Unix.gettimeofday () -. t);
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
  print_endline ("Traditional Dynamic Programming Optimal Value " ^ string_of_int (Array.last (Array.last matrix)));
  Printf.printf "Traditional Dynamic Programming Optimal subset: ";
  recurse (btbag ~table:matrix ~bag:bag);
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

let t2a bag weight =
  let a = Array.mapi bag 
    ~f:(fun idx (w, v) -> (
      float_of_int v /. float_of_int w,
      getValue bag idx,
      getWeight bag idx,
      idx
    )) in 
  Array.sort a ~compare:(fun (x, _, _, _) (y, _, _, _) -> if x = y then 0 else if x > y then -1 else 1);
  let rec greedy idx currentValue weightLeft = 
    let ( _ , value, weight, i ) = a.(idx) in 
    match (idx = Array.length bag, weightLeft >= 0, weight >= weightLeft) with 
    | ( _, _, true ) | ( _, false, _ ) | ( true, true, _ ) -> [] 
    | ( false, true, _ ) -> (value, i + 1)::greedy (idx + 1) (currentValue + value) (weightLeft - weight)
  in greedy 0 0 weight;
;;

(* //Heap Approach *)
let t2b bag weight =
  let arr = Array.mapi bag 
    ~f:(fun idx (w, v) -> (
      float_of_int v /. float_of_int w,
      getValue bag idx,
      getWeight bag idx,
      idx
    )) in 
  let a = Heap.of_array arr 
    ~cmp:(fun (x, _, _, _) (y, _, _, _) -> if x = y then 0 else if x > y then -1 else 1) in
  let rec greedy weightLeft = 
    match (Heap.pop a) with 
    | Some (_, value, weight, i) -> 
        (match weight >= weightLeft with 
        | false -> (value, i + 1)::greedy (weightLeft - weight)
        | true -> [])
    | None -> []
  in greedy weight;
;;

let reconstructFromIndices bag weight = 
  let ( totalValue, lst ) =  List.fold_right (t2a bag weight) 
    ~f:(fun (value, idx) (accumValue, oIdx) -> (value + accumValue, idx::oIdx)) 
    ~init:(0, []) in
  print_endline ("Greedy Approach Optimal value: " ^ (string_of_int totalValue));
  Printf.printf "Greedy Approach Optimal subset: { ";
  List.iteri lst ~f:(fun _ v -> Printf.printf "%d " v);
  Printf.printf "}\n";
;;

let reconstructFromIndices1 bag weight = 
  let ( totalValue, lst ) =  List.fold_right (t2b bag weight) 
    ~f:(fun (value, idx) (accumValue, oIdx) -> (value + accumValue, idx::oIdx)) 
    ~init:(0, []) in
  print_endline ("Heap-based Greedy Approach Optimal value: " ^ (string_of_int totalValue));
  Printf.printf "Heap-based Greedy Approach Optimal subset: { ";
  List.iteri lst ~f:(fun _ v -> Printf.printf "%d " v);
  Printf.printf "}\n";

;;

let main = 
  let input = ref "" in 
  input := In_channel.(input_line_exn stdin);
  let weight = fileWeight !input in 
  let backpack = fileOpen !input in 
  functionRunner t1a backpack weight "Traditional Dynamic Programming";

  functionRunner reconstructFromIndices  backpack weight "Greedy Approach";
  functionRunner reconstructFromIndices1 backpack weight "Heap Based Greedy Approach"; 
  (* functionRunner t1b backpack weight; *)

  
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