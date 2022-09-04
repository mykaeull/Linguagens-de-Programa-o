type arvore = Vz | Raiz of arvore * int * arvore ;;

let rec inserir elem arv =
  match arv with
  | Vz -> Raiz(Vz, elem, Vz)
  | Raiz(esq,r,dir) -> if elem < r then
                        Raiz(inserir elem esq, r, dir) else
                        Raiz(esq, r, inserir elem dir)

let imprimir arv =                       
  let rec imp arv =
    match arv with
    | Vz -> print_string "*"
    | Raiz(Vz, r, Vz) -> print_int r
    | Raiz(esq, r, dir) -> (imp esq; Printf.printf " %d " r; imp dir) in
  (imp arv);
  print_endline "";;

let arv = Raiz(Raiz(Raiz(Vz, 3, Vz), 5, Raiz(Vz, 6, Vz)), 9, Raiz(Raiz(Vz, 6, Vz), 7, Raiz(Vz, 8, Vz))) in

let arv2 = inserir 2 arv in

(imprimir arv);;