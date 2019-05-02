open Core;;

(* dune build main.exe && ./_build/default/main.exe *)

let trimLine (s: string) = 
  int_of_string ( String.strip s )

let fileOpen input = 
  List.fold2_exn 
    (In_channel.read_lines (input ^ "_w.txt")) 
    (In_channel.read_lines (input ^ "_v.txt"))
    ~init:(Array.create ~len:0 (0, 0)) ~f:(fun init weight text -> 
      Array.append init (Array.create ~len:1 ( trimLine weight,  trimLine text ))) 

let fileWeight input =
  In_channel.read_all (input ^ "_c.txt") |> trimLine

let getWeight bag i = let ( w, _ ) = bag.(i) in w 
let getValue  bag i = let ( _, v ) = bag.(i) in v 

let functionRunner f bag weight =
  let t = Unix.gettimeofday () in
  f bag weight;
  Printf.printf "Execution time: %fs\n" (Unix.gettimeofday () -. t);
;;

let btbag ~table ~bag = 
  let x = Array.length table - 1 in 
  let y = Array.length table.(0) - 1 in 
  let rec backtrack x y = 
    if table.(x).(y) = 0 then []
    else if y - getWeight bag (x - 1) < 0 then [getValue bag (x - 1)]
    else match ( table.(x - 1).(y) > table.(x - 1).(y - getWeight bag (x - 1)) + getValue bag (x - 1) ) with
    | true -> backtrack (x - 1) y
    | false -> (getValue bag (x - 1))::backtrack (x - 1) (y - getWeight bag (x - 1))
  in backtrack x y ;;


let rec recurse lst = 
  match lst with 
  | [] -> print_endline "Done"
  | hd::tl -> print_endline (string_of_int hd); recurse tl
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

let bt ~table ~weight ~value = 
  let x = Array.length table - 1 in 
  let y = Array.length table.(0) - 1 in 
  let rec backtrack x y = 
    if table.(x).(y) = 0 then []
    else match ( table.(x - 1).(y) > table.(x - 1).(y - weight.(x - 1)) + value.(x - 1) ) with
    | true -> backtrack (x - 1) y
    | false -> (value.(x - 1))::backtrack (x - 1) (y - weight.(x - 1))
  in backtrack x y ;;


let main = 
  let input = ref "" in 
  input := In_channel.(input_line_exn stdin);
  let weight = fileWeight !input in 
  let backpack = fileOpen !input in 
  functionRunner t1a backpack weight;
  ;;



