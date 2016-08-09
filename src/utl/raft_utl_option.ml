
let string_of_option f = function
  | None   -> "None"
  | Some x -> f x 
