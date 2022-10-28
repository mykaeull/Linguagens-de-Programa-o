type tNoh = { elem: int; mutable ant: tNoh ref option; mutable prox: tNoh ref option } ;;

type tLista = { mutable inicio: tNoh ref option } ;;

let criar_lista_vazia () =
  { inicio = None } ;;

let inserir lista elem =
  let novo = { elem; ant = None; prox = lista.inicio } in 
  begin
  match lista.inicio with
  | None -> ()
  | Some r -> (!r).ant <- Some (ref novo)
  end;
  lista.inicio <- Some (ref novo) ;;

let iterar f lista =
  let rec it atual =
    match atual with
    | None -> ()
    | Some r -> f ((!r).elem); it ((!r).prox)
  in 
  it lista.inicio ;;

let procurar lista elem =
  let rec proc atual elem =
    match atual with 
    | None -> None (* print_endline "Elemento não existe na lista." *)
    | Some r -> 
      if (!r).elem == elem then 
        Some r(* print_endline "Elemento existe na lista." *)
      else
        proc ((!r).prox) elem
  in
  proc lista.inicio elem ;; 

let main () =
  let l = criar_lista_vazia () in 
  inserir l 1;
  inserir l 2;
  inserir l 3;
  iterar (Printf.printf "%d\n") l;
  (*
  let teste = procurar l 3 in 
  if teste == None then 
    print_endline "Elemento não existe na lista."
  else
    print_endline "Elemento existe na lista." ;;
    (* Printf.printf "%d\n" (!teste).elem ;; *))
  *)
  match procurar l 2 with
  | None   -> print_endline "2 não encontrado!"
  | Some r -> Printf.printf "Encontrado: %d\n" (!r).elem;
              let novo = { elem = 4; ant = None; prox = None } in
              r := novo;
              iterar (Printf.printf "%d\n") l ;;

main () ;;