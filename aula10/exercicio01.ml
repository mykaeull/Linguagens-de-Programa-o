let criar_arq nome n =
  let arq = open_out nome in
  (* let texto = "qualquer coisa\n" in *)
  let rec escrever arq texto n_0 n =
    if n_0 < n then (output_string arq texto; escrever arq texto (n_0 + 1) n)
    else ()
  in
  escrever arq "qualquer coisa\n" 0 n;
  close_out arq ;;

(criar_arq "teste.txt" 10) ;;