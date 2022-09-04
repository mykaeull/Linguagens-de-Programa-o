let rec selecionar criterio l =
  match l with
  | [] -> []
  | h::t -> if (criterio h) then h :: (selecionar criterio t) else (selecionar criterio t);;

let remover_da_lista n l =
  selecionar (fun x -> x <> n) l;;

print_endline (if (selecionar (fun n -> n mod 2 = 0) [-2;-1;0;1;2;3;4;5;6]) = [-2; 0; 2; 4; 6] then "Ok" else "Algo errado");;
print_endline (if (remover_da_lista 1 [1; 2; 3; 1; 2; 1]) = [2; 3; 2] then "Ok" else "Algo errado");;
(*print_endline (if (separar_em_listas [(1, "Um"); (2, "Dois"); (3, "Três")]) = ([1; 2; 3], ["Um"; "Dois"; "Três"]) then "Ok" else "Algo errado");;*)