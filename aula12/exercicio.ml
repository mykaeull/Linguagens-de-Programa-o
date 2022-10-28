(* EXERCICIO 02 *)

let redim v n novo_tam ini =
  if novo_tam < n then raise (Failure "não foi possível efetuar a copia pois o novo vetor é maior do que o atual.")
  else 
    let novo_v = Array.make novo_tam ini in 
    for i = 0 to Array.length v - 1 do
      novo_v.(i) <- v.(i)
    done;
    (novo_v) ;;

let imprimir v nome =
  let sep () = print_endline "----------" in 
  sep ();
  for i = 0 to Array.length v - 1 do 
    Printf.printf "%s[%d]: %d\n" nome i v.(i);
  done;
  sep () ;;

let main () =
  let v = [|1;2;3;4;5|] in 
  let novo_v = redim v (Array.length v) 3 0 in
  imprimir novo_v "novo_v";
  print_string "Número a inserir (ou -1 para parar): ";
  let n = ref (read_int ()) in
  let i = ref (1) in 
  let atual = ref ([|n|]) in
  let dobrado = redim !atual (Array.length !atual) ((!i*2)) 0 in
  while !n <> -1 do
    print_string "Número a inserir (ou -1 para parar): ";
    n := read_int ();
    i := !i + 1;
    atual := redim !atual (Array.length !atual) (Array.length dobrado) n
  done;  

main () ;;