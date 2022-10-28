(*
===CRIAR OBJETOS VIA FUNCAO===
let criar_contador () =
  object
  val mutable c = 0
  method inc () = c <- c + 1
  method get () = c 
  end ;;

let imp cont nome =
  Printf.printf "%s: %d\n" nome (cont#get()) ;;

let o = criar_contador () in 
let p = criar_contador () in 
imp o "o";
imp p "p";
o#inc ();
p#inc ();
imp o "o";
imp p "p";
o#inc ();
p#inc ();
imp o "o";
imp p "p";
Printf.printf "o = p? %s\n" (string_of_bool (o = p)) ;;
*)

(*
===PILHA VIA OBJETO===
let criar_pilha () =
  object
  val mutable l = []
  
  method empilhar e = l <- e::l 

  method desempilhar () =
    match l with
    | [] -> None
    | h::t -> l <- t; Some h

  method vazia () = (l = [])

  method topo () =
    match l with 
    | [] -> None
    | h::_ -> Some h
  end ;;

let main () =
  let p = criar_pilha () in 
  let rec menu () =
    print_string "#: Emp, D: Des, V: Vaz, T: Topo, S: Sair: ";
    match read_line () with 
    | "S" -> ()
    | "T" -> begin
             match p#topo() with
             | None -> print_endline "Vazia, não há topo"
             | Some t -> Printf.printf "Topo: %d\n" t 
             end;
             menu ()
    | "V" -> Printf.printf "Vazia? %s\n" (string_of_bool (p#vazia()));
             menu ()
    | "D" -> begin 
             match p#desempilhar() with 
             | None -> print_endline "Vazia, não há o que desempilhar"
             | Some t -> Printf.printf "Desempilhado: %d\n" t 
             end;
             menu ()
    | op -> begin 
            match int_of_string_opt op with
            | None -> print_endline "Opção inválida"
            | Some n -> p#empilhar n 
            end;
            menu ()
  in
  menu () ;;
  
main () ;;
*)

type tForma = < area: float > ;;

type tQuadrado = < area: float; lado: float; set_lado: float -> unit > ;;

let criar_quadrado lado : tQuadrado =
  object
  val mutable l: float = lado 

  method area = l *. l
  method lado = l 
  method set_lado novo = l <- novo 
  end ;;

type tCirculo = < area: float; raio: float; set_raio: float -> unit > ;;

let criar_circulo raio : tCirculo =
  object 
  val mutable r: float = raio 

  method area = Float.pi *. r *. r 
  method raio = r 
  method set_raio novo = r <- novo
  end ;;

let adicionar (f: float -> tForma) (l: tForma list) (valor: float) =
  (f valor) :: l ;;

let main () =
  let l0 = [] in 
  let l1 = adicionar (criar_quadrado :> float -> tForma) l0 10. in 
  let l2 = adicionar (criar_circulo :> float -> tForma) l1 10. in
  List.iter (fun f -> Printf.printf "Area: %g\n" f#area) l2 ;;
  
main () ;;