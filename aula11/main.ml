(* 
===ARQUIVO SPLIT===   

let ler_notas nome =
  let rec ler arq =
    try
      let linha = input_line arq in 
      match String.split_on_char ',' linha with
      | [s1;s2;s3] -> 
        let n1 = s1 in
        let n2 = s2 in
        let n3 = s3 in
        let (l1, l2, l3) = ler arq in 
        (n1::l1, n2::l2, n3::l3)
      | _ -> raise (Failure "ler_notas: formato inválido")
    with
    | End_of_file -> ([], [], [])
  in 
  try
    let arq = open_in nome in 
    let resp = ler arq in 
    close_in arq;
    Some resp
  with
  | Sys_error _ -> None ;;

let ler_notas_rc nome =
  let rec ler arq (l1, l2, l3) =
    try
      let linha = input_line arq in 
      match String.split_on_char ',' linha with
      | [s1; s2; s3] -> 
        let n1 = float_of_string s1 in
        let n2 = float_of_string s2 in
        let n3 = float_of_string s3 in
        (ler) arq (n1::l1, n2::l2, n3::l3)
      | _ -> raise (Failure "ler_notas: formato inválido")
    with End_of_file -> (l1, l2, l3)
  in
  try
    let arq = open_in nome in
    let resp = ler arq ([], [], []) in 
    close_in arq;
    Some resp
  with
  | Sys_error _ -> None ;;
type l = List of string * l * l ;;
let l1 = (ler_notas "teste.txt") ;;
(* (let (a, b, c) = l1 in
Printf.printf "%s\n" h ;;) *)
*)

(* 
===MÓDULOS MULT RECURSIVOS===   

module rec
  Par:
    sig
    val eh_par: int -> bool
    end
  =
    struct
    let eh_par n =
      if n = 0 then true
      else Impar.eh_impar (n-1)
    end 
and
  Impar:
    sig
    val eh_impar: int -> bool
    end
  =
    struct
    let eh_impar n =
      if n = 0 then false
      else Par.eh_par (n-1)
    end ;;

let n = 10 in
Printf.printf "%d é par? %s\n" n (string_of_bool (Par.eh_par n)) ;;
*)

module type Data =
  sig
  type t
  val dia: t -> int
  val mes: t -> int
  val ano: t -> int
  val criar: int -> int -> int -> t
  end

module DataTupla : Data =
  struct
  type t = int * int * int
  let dia t =
    let (d,m,a) = t in
    d 
  let mes t =
    let (d,m,a) = t in  
    m 
  let ano t = 
    let (d,m,a) = t in
    a
  let criar d m a = (d,m,a)
  end

module DataRegistro : Data =
  struct
  type t = {dia: int; mes: int; ano: int}
  let dia {dia = d; mes = m; ano = a} = d
  let mes {dia; mes; ano} = mes (* notação abreviada *)
  let ano {dia; mes; ano} = ano
  let criar dia mes ano = {dia;mes;ano}
  end

module type UtilData =
  sig 
  type t
  val anterior: t -> t -> bool
  val string_of_data: t -> string 
  val data_of_string: string -> t option
  val dia_seguinte: t -> t 
  end

module CriarUtilData (D: Data):
  UtilData with type t := D.t
=
  struct
  open D
  let anterior a b =
    ano a < ano b ||
    (ano a = ano b && (mes a < mes b || (mes a = mes b && dia a < dia b)))

  let string_of_data t =
    let dia = string_of_int (dia t) in 
    let mes = string_of_int (mes t) in 
    let ano = string_of_int (ano t) in
    dia ^ "/" ^ mes ^ "/" ^ ano 

  let data_of_string s =
    match String.split_on_char '/' s with 
    | [s1;s2;s3] ->
      let dia = int_of_string s1 in
      let mes = int_of_string s2 in 
      let ano = int_of_string s3 in
      let data = D.criar dia mes ano in 
      Some data
    | _ -> None 
  
  let dia_seguinte t =
    let dia = dia t in
    let mes = mes t in
    let ano = ano t in
    if ano mod 4 == 0 && dia == 28 && mes == 2 then
      let data = D.criar (dia+1) mes ano in 
      (data)
    else if dia == 28 && mes == 2 then
      let data = D.criar 1 (mes+1) ano in 
      (data)
    else if (dia == 30 || dia == 31) && mes == 12 then 
      let data = D.criar 1 1 (ano+1) in 
      (data)
    else if (dia == 30 || dia == 31) then 
      let data = D.criar 1 (mes+1) ano in 
      (data)
    else
      let data = D.criar (dia+1) mes ano in 
      (data) ;;    
  end ;;

module UT = CriarUtilData(DataTupla) ;;

(* let data = DataTupla.criar 10 10 2022 in 
Printf.printf "%d\n" (DataTupla.ano data) ;; *)

let hoje = DataTupla.criar 14 09 2022 in 
let amanha = DataTupla.criar 15 09 2022 in 
let res = UT.anterior hoje amanha in 
let data_string = UT.string_of_data hoje in
let data_tupla = UT.data_of_string data_string in
let data_presente = DataTupla.criar 30 12 2022 in 
let data_futuro = UT.dia_seguinte data_presente in
Printf.printf "hoje < amanha? %s\n" (string_of_bool res);
Printf.printf "data formatada: %s\n" (data_string);
Printf.printf "dia seguinte: %s\n" (UT.string_of_data data_futuro) ;;