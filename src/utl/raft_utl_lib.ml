let group_of_n l n = 
  let rec  aux g l = function
    | 0 -> (List.rev g)::(aux [] l n) 
    | i -> 
      match l with 
      | [] -> [g] 
      | hd :: tl -> 
        aux (hd :: g) tl (i - 1) 
  in
  aux [] l n 
