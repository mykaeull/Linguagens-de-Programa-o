(* 
===CHAMADAS INGENUAS===

let rec criar n =
  if n <= 0 then []
  else 0::(criar (n-1)) ;;

let rec tam l =
  match l with 
  | [] -> 0
  | h::t -> 1 + (tam t);;

let rec imprimir_lista l =
  match l with
  | [] -> ()
  | h::t -> (Printf.printf "%d\n" h; imprimir_lista t) ;;

*)

(* 
===CHAMADAS DE CAUDA===    
let criar n =
  let rec cr n l =
    if n <= 0 then l 
    else cr (n-1) (0::l)
  in 
  cr n [] ;;

let tam l = 
  let rec tam_rec l ac =
    match l with 
    | [] -> ac 
    | h::t -> tam_rec t (ac + 1)
  in 
  tam_rec l 0 ;;
*)

let main () =
  print_string "Tamanho da lista a ser criada: ";
  let n = read_int () in 
  (* let l = criar n in *)
  let l = Lista.criar n in 
  print_endline "Lista criada";
  (* Printf.printf "Tamanho: %d\n" (tam l) *)
  Printf.printf "Tamanho: %d\n" (Lista.tam l) ;;
  (*(imprimir_lista l) ;;*)

main () ;;

module type UnidadeDeTempo =
  sig      (* Seria o conteúdo do arquivo mli do módulo *)
    type t 
    val de_int : int -> t 
    val para_int : t -> int 
  end ;;

module Minuto : UnidadeDeTempo =
  struct   (* Seria o conteúdo do arquivo ml do módulo *)
  type t = int 
  let de_int i = i 
  let para_int m = m 
  end ;;

let m = Minuto.de_int 3 in
Printf.printf "%d\n" (Minuto.para_int m) ;;