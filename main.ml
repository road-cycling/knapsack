open Core;;

(* dune build main.exe && ./_build/default/main.exe *)

let trimLine (s: string) = 
  int_of_string ( String.strip s )

let fileOpen input = 
  List.fold2_exn 
    (In_channel.read_lines (input ^ "_w.txt")) 
    (In_channel.read_lines (input ^ "_v.txt"))
    ~init:(Array.create ~len:0 (0, 0)) ~f:(fun init weight text -> 
      Array.append init (Array.create ~len:1 ( trimLine weight,  trimLine text))) 

let fileWeight input =
  In_channel.read_all (input ^ "_c.txt") 
  |> trimLine

let getHighestValue array =
  (Array.fold_right array ~f:(fun ( _, v ) init -> Pervasives.max v init ) ~init:0) + 1

let getWeight bag i =
  let ( w, _ ) = bag.(i) in 
  w 

let getValue bag i = 
  let ( _, v ) = bag.(i) in 
  v 

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
  Array.last (Array.last matrix)

;;

let t1b bag weight = 
  let rec knapsack i currentVal weightLeft = 
    match (i = Array.length bag, weightLeft >= 0) with
    | ( true, true )  -> currentVal
    | ( false, true ) -> Pervasives.max ( knapsack (i + 1) currentVal weightLeft ) ( knapsack (i + 1) (currentVal + (getValue bag i)) (weightLeft - (getWeight bag i)) )
    | ( _ , false ) -> 0
  in knapsack 0 0 weight;

;;

let main = 
  let input = ref "" in 
  input := In_channel.(input_line_exn stdin);
  let weight = fileWeight !input in 
  let backpack = fileOpen !input in 
  print_endline (string_of_int (t1a backpack weight));
  print_endline (string_of_int (t1b backpack weight));
  ;;
  (* let bag = fileOpen in 
  Array.sort bag ~compare:(fun (w1, _) (w2, _) -> Pervasives.compare w1 w2);
  (* Array.iteri ~f:(fun _ (weight, value) -> 
  print_endline ((string_of_int weight) ^ " " ^ (string_of_int value)) 
  ) bag;; *)
  print_endline (string_of_int (t1a bag));; *)


(* 
    for i = 0 to (Array.length matrix) - 1 do 
    for j = 0 to (Array.length matrix.(i)) - 1 do 
      (* print_endline (" " ^ (string_of_int matrix.(i).(j)) ^ " "); *)
      Printf.printf " %d " matrix.(i).(j)
    done;
    Printf.printf "\n"
  done; *)