(*
===VETORES===  
let imprimir v nome =
  let rec imp i =
    if i >= Array.length v then ()
    else (Printf.printf "%s[%d]: %d\n" nome i v.(i); imp (i+1)) 
  in
  let sep () = print_endline "--------------" in 
  sep ();
  imp 0;
  sep () ;;
*)

(*  
===LAÇO FOR===
let imprimir v nome =
  let sep () = print_endline "----------" in 
  sep ();
  for i = 0 to Array.length v - 1 do 
    Printf.printf "%s[%d]: %d\n" nome i v.(i);
  done;
  sep () ;;
*)

(*  
let main () =
  let v = [|1;2;3;4;5|] in
  imprimir v "v";
  v.(0) <- -1;
  v.(1) <- -2;
  v.(2) <- -3;
  v.(3) <- -4;
  v.(4) <- -5;
  imprimir v "v";
  print_string "n (tamanho do vetor): ";
  let n = read_int () in 
  let w = Array.make n 0 in 
  imprimir w "w" ;;
*)

(*
===REFERENCIAS E WHILE===
let ler_int_positivo mensagem =
  print_string mensagem;
  let ri = ref (read_int ()) in 
  while !ri <= 0 do 
    print_endline "O número precisa ser positivo.";
    print_string mensagem;
    ri := read_int ()
  done;
  !ri ;;

let imprimir v nome =
  let sep () = print_endline "----------" in 
  sep ();
  for i = 0 to Array.length v - 1 do 
    Printf.printf "%s[%d]: %d\n" nome i v.(i);
  done;
  sep () ;;

let main () =
  let n = ler_int_positivo "n (tamanho do vetor): " in 
  let w = Array.make n 0 in 
  imprimir w "w" ;;

main ();;
*)

type reg = { nome: string; mutable idade: int } ;;

let imprimir r =
  Printf.printf "%s -> %d\n" r.nome r.idade ;;

let main () =
  let r = { nome = "Fulano"; idade = 20 } in 
  imprimir r;
  r.idade <- r.idade + 1;
  imprimir r ;;

main () ;;