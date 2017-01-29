#' @title Funkcja wymieniająca droższe zwierzę na kilka tańszych
#'
#' @description Jeśli nie mamy żadnego egzemplarza któregoś ze zwierząt tañszych niż zwierzę, które chcemy wymienić, to po kolei je kupujemy za sprzedane zwierzę.
#'
#' @param zagroda - wektor reprezentujący liczbê poszczególnych rodzajów zwierząt w zagrodzie
#' @param zwierze - numer zwierzęcia, które chcemy sprzedać
#' @return wektor reprezentujący stan zagrody po dokonianiu odpowiednich wymian
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export
#'


kup_kilka_tanszych  <- function( zagroda, zwierze ) {
  do_zaplaty <- -wartosc(zwierze)

  if (indeks(zwierze) == indeks("krowa")) {
    wektor_kosztu_zwierze <- c( 36, 6, 3 )
  }
  if (indeks(zwierze) == indeks("swinia")) {
    wektor_kosztu_zwierze <- c( 12, 2, 0 )
  }
  if (indeks(zwierze) == indeks("owca")) {
    wektor_kosztu_zwierze <- c( 6, 0, 0 )
  }

  zagroda[indeks(zwierze)] <- zagroda[indeks(zwierze)] + 1
  for (i in c( "krolik", "owca", "swinia" ) ) {
    while (do_zaplaty < 0 &&
           zagroda[indeks(i)] == 0 &&
           zagroda[indeks(zwierze)] > 1 &&
           wartosc(zwierze) > wartosc(i)) {
      zagroda <- wymiana(zagroda, zwierze, i, 0, wektor_kosztu_zwierze[indeks(i)])
      do_zaplaty <- do_zaplaty + wektor_kosztu_zwierze[indeks(i)] * wartosc(i)
    }
  }
  stado <- c( 60, 24, 20, 12, 6, 4, 2 )
  stopifnot(all( zagroda <= stado )) # test poprawnosci
  zagroda
}
