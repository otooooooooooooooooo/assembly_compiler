module CommonFunctions = struct

(**
Checks whether
an el is in list l
*)
let rec is_in l el =
    match l with
    |[] -> false
    |x::xs -> el = x || is_in xs el


end
