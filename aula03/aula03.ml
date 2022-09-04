let rec substituir l a b =
  match l with
  | [] -> []
  | h::t -> if h = a then b :: (substituir t a b) else h :: (substituir t a b);;

print_endline (if (substituir [1;2;3;1;2;3;3;3;2;2;1;1] 2 0) = [1;0;3;1;0;3;3;3;0;0;1;1] then "Ok" else "Algo errado");;

let rec mapear l f =
  match l with
  | [] -> []
  | h::t -> (f h) :: mapear t f;;

let l1 = [1; 2; 3; 4];;

let l2 = mapear l1 (fun i -> i*i);;

let l3 = mapear l2 string_of_int ;;

let l4 = mapear l3 (fun s -> s^s) ;;

let l5 = mapear l4 String.length ;;

print_endline
  begin
  if l2 = [1; 4; 9; 16] &&
     l3 = ["1"; "4"; "9"; "16"] &&
     l4 = ["11"; "44"; "99"; "1616"] &&
     l5 = [2; 2; 2; 4]
  then "Conforme esperado."
  else "Algo errado!"
  end

(*
let l = [1; 2; 3; 4; 5];;

let rec imprimir_lista l =
  match l with
  | [] -> print_endline ""
  | h::t -> (if t <> [] then Printf.printf "%d::" h else Printf.printf "%d" h); imprimir_lista t;;

let rec tam l =
  match l with
  | [] -> 0
  | _::t -> 1 + tam t;;

imprimir_lista l;;

let rec criar_lista_natural () =
  begin
  print_string "Digite um natural (negativo para parar): ";
  let i = read_int () in
  if i < 0 then
    []
  else
    i :: (criar_lista_natural ())
  end

let l2 = criar_lista_natural ();;
Printf.printf "tamanho da lista: %d\n" (tam l2);;
*)