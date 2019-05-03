open Core_kernel;;


module Htbl = struct 
  let size = 30
  let getKey k_x k_y = ( ( k_x + k_y ) * ( k_x + k_y + 1 ) / 2 ) + k_y

  let getTbl = Array.init size 
    ~f:(fun _ -> (Avltree.empty : (int, int * int * int) Avltree.t))

  let rec insertHelper idx k table x y v = 
    let inserted = ref false in
    let newTree = Avltree.add table.(idx mod size) ~replace:false ~compare:Pervasives.compare ~added:inserted ~key:k ~data:(x, y, v) in 
    match !inserted with 
    | true -> table.(idx mod size) <- newTree 
    | false -> insertHelper (idx + 1) k table x y v

  let insert (k_x, k_y) v table = 
    insertHelper (k_x + k_y mod size) (getKey k_x k_y) table k_x k_y v

  let rec findBucket idx k table x y = 
    match (Avltree.find table.(idx mod size) ~compare:Pervasives.compare k) with 
    | Some (k_x, k_y, v) -> if k_x = x && k_y = y then Some (v) else findBucket (idx + 1) k table x y 
    | None -> None

  let findHelper (k_x, k_y) table = 
    match findBucket (k_x + k_y mod size) (getKey k_x k_y) table k_x k_y with 
    | Some v -> v 
    | None -> -1


  let find (k_x, k_y) table = 
      match k_x with 
      | 0 -> 0
      | _ -> findHelper (k_x, k_y) table

end 