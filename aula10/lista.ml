let rec cr n l =
  if n <= 0 then l 
  else cr (n-1) (0::l) ;;

let criar n = cr n [] ;;

let rec tam_rec l ac =
  match l with 
  | [] -> ac 
  | h::t -> tam_rec t (ac + 1) ;;

let tam l = tam_rec l 0 ;;