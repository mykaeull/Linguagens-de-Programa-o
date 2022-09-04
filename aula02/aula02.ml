let print = Printf.printf;;

let calculator a op b =
  match op with
  | "+" -> a +. b
  | "-" -> a -. b
  | "*" -> a *. b
  | "/" -> if b = 0. then 
            (print_string "Não é permitido dividir por zero!\n"; 0.) 
          else 
            a /. b
  | "**" -> a ** b
  | _ -> print_string "Operação inválida!\n"; 0.;;

print_string "Digite o primeiro operando: ";;
let a = read_float();;
print_string "Digite a operação (+ - * / **): ";;
let op = read_line();;
print_string "Digite o segundo operando: ";;
let b = read_float();;

let result = calculator a op b;;

print "Resultado: %g\n" result;;