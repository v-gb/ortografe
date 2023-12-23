let%expect_test _ = (
  print_string (Ortografe.pure_text "https://ceux.com/choix. Coment. Jusque-là. Choix.");
  [%expect "https://ceux.com/choix. Coment. Jusque-l\195\160. Chois. "];
  print_string (Ortografe.pure_text "plouf-européennes");
  [%expect "plouf-européènes"];
)
