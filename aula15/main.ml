(*
===OPERADORES PERSONALIZADOS===
let (==>) a b =
  if a then b else true ;;

let (<=>) a b =
  (a ==> b) && (b ==> a) ;;

let imptabverd op nomeop =
  let imp b =
    if b then 
      print_string "true"
    else
      print_string "false"
  in
  let linha a b =
    imp a;
    Printf.printf " %s " nomeop;
    imp b;
    Printf.printf " : ";
    imp (op a b);
    print_char '\n'
  in 
  Printf.printf "\nTabela verdade de %s:\n" nomeop;
  linha true true;
  linha true false;
  linha false true;
  linha false false ;;
  
let main () =
  imptabverd (==>) "==>";
  imptabverd (<=>) "<=>" ;;

main () ;;
*)

(*
===OPERADORES REGISTROS===
type ('t, 'u) tPar = { mutable prim: 't; mutable seg: 'u } ;;

let (=.) reg valor = reg.prim <- valor ;;

let (=..) reg valor = reg.seg <- valor ;;

let main () =
  let imp p =
    Printf.printf "{ %d ; %s }\n" p.prim p.seg 
  in
  let p = { prim = 0; seg = "Zero" } in 
  imp p;
  p =. 1;
  imp p;
  p =.. "Um";
  imp p;
  p =. 2;
  imp p;
  p =.. "Dois";
  imp p ;;

main () ;;
*)

(*
===REFERENCIAS SAO REGISTROS===
let imprimir ({ contents }: int ref) =
  Printf.printf "%d\n" contents ;;

let main () =
  let r = ref 0 in 
  imprimir r;
  r := 1;
  imprimir r;
  r.contents <- 2;
  imprimir r ;;

main () ;;
*)

(*
===MODULOS DE 1a ORDEM===
module type TipoMod =
  sig 
  val intf: int -> int 
  end;;
module MId : TipoMod =
  struct
  let intf x = x
  end
module MInc : TipoMod =
  struct 
  let intf x = x + 1
  end

let aplicarEImprimir (module M: TipoMod) i =
  Printf.printf "%d\n" (M.intf i) ;;

let main () =
  print_string "Id (0) ou Inc (1)? ";
  let escolha = read_int () in 
  if escolha = 0 then 
    aplicarEImprimir (module MId: TipoMod) 0
  else
    aplicarEImprimir (module MInc) 0
  ;;

main () ;;
*)

(*
===MODULOS COM ESTADO===
module type ImpCadastro =
  sig 
  val sep: string
  val impcad: string list -> string -> unit
  end;;

module ImpTracos : ImpCadastro =
  struct
  let sep = "-------------------------------"

  let impcad cadastro nomearq =
    let arq = open_out nomearq in 
    List.iter (fun s -> output_string arq (s ^ "\n" ^ sep ^ "\n")) cadastro;
    close_out arq ;;
  end

let main () =
  let cad = "Fulano"::"Beltrano"::"Cicrano"::[] in 
  ImpTracos.impcad cad "cadastro.txt" ;;

main () ;;
*)

module type ImpCadastro =
  sig 
  val sep: string
  val impcad: string list -> string -> unit
  end;;

module ImpTracos : ImpCadastro =
  struct
  let sep = "-------------------------------"

  let impcad cadastro nomearq =
    let arq = open_out nomearq in 
    List.iter (fun s -> output_string arq (s ^ "\n" ^ sep ^ "\n")) cadastro;
    close_out arq ;;
  end

module ImpBarras : ImpCadastro =
  struct 
  let sep = "////////////////////////////////"

  let impcad cadastro nomearq =
    let arq = open_out nomearq in 
    List.iter (fun s -> output_string arq (s ^ "\n" ^ sep ^ "\n")) cadastro;
    close_out arq ;;
  end

let main () =
  let cad = "Fulano"::"Beltrano"::"Cicrano"::[] in 
  print_string "Traços (T) ou barras (B)? ";
  match read_line () with 
  | "T" -> ImpTracos.impcad cad "cadastro.txt"
  | "B" -> ImpBarras.impcad cad "cadastro.txt"
  | _ -> print_endline "Opção inválida" ;;

main () ;;
