(*
===INICIALIZANDO CLASSES===
class tPessoa nome_pessoa =
  object (self)
  method nome = nome_pessoa
  
  initializer 
    Printf.printf "Criando a pessoa %s\n" self#nome 
  end ;;

let _ = new tPessoa "Fulano" ;;
*)

(*
===TIPOS DE SELF===
let criar_pessoa nome_pessoa =
  object (self: 't)

  method nome = nome_pessoa 

  method tem_mesmo_nome (obj: 't) = (self#nome = obj#nome)

  initializer 
    Printf.printf "Criando a pessoa %s\n" self#nome 
  end ;;

let f = criar_pessoa "Fulano" in 
let b = criar_pessoa "Beltrano" in 
Printf.printf "Têm o mesmo nome? -> %s\n" (string_of_bool (f#tem_mesmo_nome b)) ;;
*)

(*
===TIPOS DE CLASSES E METODOS PRIVADOS===
class tClasse : 
  object 
  method qqcoisa: unit 
  end 
  =
  object 
  method qqcoisa = print_endline "qqcoisa"

  method private priv = print_endline "Privado"

  end ;;

class tSub =
  object 
  inherit tClasse as super 

  method qqcoisadiferente = print_endline "qqcoisa, mas diferente"

  method qqcoisa = super#qqcoisa

  method algomais = print_endline "algo mais"

  (* method chama_priv = super#priv *)

  end ;;

let sub = new tSub in 
sub#qqcoisa;
sub#qqcoisadiferente;
sub#algomais ;;
(* sub#chama_priv ;; *)
*)

(*
===HERENÇA MULTIPLA===
class tA arg =
  object 
  method mA = print_endline ("mA " ^ arg)

  method comum = print_endline "comum A"
  end ;;

class tB arg =
  object 
    method mB = print_endline ("mB " ^ arg)

    method comum = print_endline "comum B"
  end ;;

class tSub argA argB =
  object 
  inherit tA argA as superA
  inherit tB argB as superB

  method comum = print_endline "comum Sub"
  method mSub = 
    print_endline "------------";
    superA#comum;
    superB#comum;
    print_endline "mSub";
    print_endline "------------";
  end ;;

let o = new tSub "1" "2" in 
o#mA;
o#mB;
o#mSub;
o#comum ;;
*)

module A =
  struct 
  let mA () = print_endline "mA"
  end ;;

module B =
  struct 
  include A

  let mB () = print_endline "mB"
  end ;;

B.mB ();
B.mA () ;;