(*
type data = { dia: int; mes: int; ano: int };;

let ler_data () =
  print_string "Digite uma data (DD/MM/AAAA): ";
  let s = read_line () in
  { dia = int_of_string (String.sub s 0 2);
    mes = int_of_string (String.sub s 3 2);
    ano = int_of_string (String.sub s 6 4) };;

let d = ler_data () in
Printf.printf "Dia: %02d  Mês: %02d  Ano: %04d\n" d.dia d.mes d.ano;;
*)

type data = { dia: int; mes: int; ano: int };;
type registro = { nome: string; data_nascimento: data };;
let cadastros = [];;
(* { nome = "jennie"; data_nascimento = { dia = 01; mes = 01; ano = 2004} } *)

let rec ler_opcao msg =
  print_string msg;
  let op = read_line () in
  if op <> "L" && op <> "I" && op <> "S" then 
    (print_endline "Opção inválida, tente novamente."; ler_opcao msg) 
  else 
    op;;

let rec ler_data_nascimento msg =
  print_string msg;
  let data_nascimento = read_line () in
  if String.length data_nascimento <> 10 || (data_nascimento.[2] <> '/' || data_nascimento.[5] <> '/') then
    (print_endline "Erro na leitura da data, repetindo:"; ler_data_nascimento msg)
  else
    { dia = int_of_string (String.sub data_nascimento 0 2);
      mes = int_of_string (String.sub data_nascimento 3 2);
      ano = int_of_string (String.sub data_nascimento 6 4) };;

let rec printar_cadastros (l) =
  match l with
  | [] -> print_endline ""
  | h::t -> (print_endline "----------------------------------------"; 
            Printf.printf "Nome: %s\n" h.nome;
            Printf.printf "Nascimento: %d/%d/%d\n" h.data_nascimento.dia h.data_nascimento.mes h.data_nascimento.ano;
            print_endline "----------------------------------------"; 
            printar_cadastros (t));;

let rec append l e =
  match l with
  | [] -> e
  | h::t -> h :: append t e;;

let inserir_registro (cadastros) =
  print_endline "Leitura de novo registro:";
  print_string "Nome: ";
  let nome = read_line () in
  let data_nascimento = ler_data_nascimento "Nascimento (DD/MM/AAAA): " in
  let registro = { nome = nome; data_nascimento = data_nascimento } in
  let cadastros_att = append cadastros [registro] in
  cadastros_att;;

let rec loop_menu flag cadastros =
  print_endline "Menu:";
  print_endline "  L: Listar o cadastro inteiro";
  print_endline "  I: Inserir um registro no cadastro";
  print_endline "  S: Sair";
  let op = ler_opcao "Digite a opção escolhida (L/I/S): " in
  if op <> "S" then 
    if op = "L" then 
      (print_endline "Cadastro atual:";
      (printar_cadastros (cadastros));
      loop_menu op cadastros)
    else 
      (let cadastros_att = (inserir_registro (cadastros)) in
      loop_menu op cadastros_att)
  else 
    print_endline "---";; 

(loop_menu "X" []);;
 