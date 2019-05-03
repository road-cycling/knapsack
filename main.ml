open Core;;
open Core_kernel;;
open Htbl;;
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

let t1b bag weight matrix= 
  let rec knapsack i j = 
    match Htbl.find (i, j) matrix < 0 with 
    | true -> 
      (match j < (getWeight bag (i - 1)) with 
        | true -> 
          Htbl.insert (i, j) (knapsack (i - 1) j) matrix; 
          Htbl.find (i, j) matrix;
        | false -> Htbl.insert (i, j) (Pervasives.max (knapsack (i - 1) j) ((getValue bag (i - 1)) + knapsack (i - 1) (j - (getWeight bag (i - 1))))) matrix ;
          Htbl.find (i, j) matrix)
    | false -> Htbl.find (i, j) matrix
  in knapsack (Array.length bag) weight;
;;

let bthtable ~htable ~bag weight = 
  let x = Array.length bag in 
  let y = weight in 
  let rec backtrack x y = 
    if Htbl.find (x, y) htable = 0 then []
    else if y - getWeight bag (x - 1) < 0 then [1]
    else match ( Htbl.find (x - 1, y) htable > 
    Htbl.find (x - 1, y - getWeight bag (x - 1)) htable + getValue bag (x - 1) ) with
    | true -> backtrack (x - 1) y
    | false -> x::backtrack (x - 1) (y - getWeight bag (x - 1))
  in backtrack x y ;;


let t1brunner bag weight = 
  let matrix = Htbl.getTbl in 
  print_endline ("Space-efficient Dynamic Programming Value: " ^ (string_of_int (t1b bag weight matrix)));
  Printf.printf "Space-efficient Dynamic Programming Optimal Subset: ";
  recurse (bthtable ~htable:matrix ~bag:bag weight);
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
  functionRunner t1brunner backpack weight "Space-efficient Dynamic Programming";
  functionRunner reconstructFromIndices  backpack weight "Greedy Approach";
  functionRunner reconstructFromIndices1 backpack weight "Heap Based Greedy Approach"; 








(* let timerRunner f bag weight =
  let t = Unix.gettimeofday () in
  f bag weight;
  Unix.gettimeofday () -. t;
;;

let getMeasurements weight backpack =   
  ( Array.length backpack * weight, 10 + 2 * weight, Array.length backpack, Array.length backpack )

let log2 v = 
    int_of_float (Float.round_up (log (float_of_int v)))

let task3 input = 
  let weight = fileWeight input in 
  let backpack = fileOpen input in
  let (a_space, b_space, c_space, d_space) = getMeasurements weight backpack in 
  let a_time = timerRunner t1a backpack weight in
  let b_time = timerRunner t1brunner backpack weight in 
  let c_time = timerRunner reconstructFromIndices  backpack weight in 
  let d_time = timerRunner reconstructFromIndices1 backpack weight in 
  ( (log2 a_space, a_time), (log2 b_space, b_time), (log2 c_space, c_time), (log2 d_space, d_time) )



let main3 =
  for i = 0 to 8 do 
    let ( (s_1, t_1), (s_2, t_2), (s_3, t_3), (s_4, t_4) ) = task3 ("p0" ^ string_of_int i) in 
    Printf.printf "I: %d \n" i;
    Printf.printf "%d, %f\n %d, %f\n %d, %f\n %d, %f\n" s_1 t_1 s_2 t_2 s_3 t_3 s_4 t_4;
  done;;
  print_endline "Finished";; *)
