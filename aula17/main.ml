(*
===METODOS RECURSIVOS===
let obj = 
  object (self)

  method fat i = if i = 0 then 1 else i * (self#fat (i-1))
  method eh_par i = if i = 0 then true else self#eh_impar (i-1)
  method eh_impar i = if i = 0 then false else self#eh_par (i-1)
  end ;;

for i = 0 to 5 do 
  Printf.printf "%d! -> %d\n" i (obj#fat i);
  Printf.printf "%d é par? -> %s\n" i (string_of_bool (obj#eh_par i));
  Printf.printf "%d é impar? -> %s\n" i (string_of_bool (obj#eh_impar i));
done
*)

class ['t] tPilha inicial =
  object 
  val mutable l: 't list = inicial

  method empilhar elem = l <- elem::l 

  method desempilhar = match l with 
                       | [] -> None 
                       | h::t -> l <- t; Some h 
  
  method vazia = (l = [])
  end ;;

class ['t] tPilhaConsultavel inicial msg_desemp (farg: unit -> unit) =
  object 
  inherit ['t] tPilha inicial as super 

  method desempilhar = print_endline msg_desemp; super#desempilhar

  method topo = match l with
                | [] -> None 
                | h::t -> Some h 

  method metodof = farg ()
  end ;;

let oi () = print_endline "oi" in 
let p = new tPilhaConsultavel [] "Desempilhando..." oi in 
p#metodof;
p#empilhar 1;
p#empilhar 2;
p#empilhar 3;
while p#vazia = false do
  begin
  match p#topo with
  | None -> print_endline "Absurdo!"
  | Some t -> Printf.printf "Topo: %d\n" t 
  end;
  match p#desempilhar with 
  | None -> print_endline "Absurdo!"
  | Some e -> Printf.printf "Desempilhar: %d\n" e 
done ;;