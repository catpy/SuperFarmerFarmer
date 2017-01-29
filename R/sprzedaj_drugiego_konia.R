#' @title Funkcja wymieniająca drugiego konia na inne zwierzęta
#'
#' @description Funcja wymienia drugiego konia po kolei na niezbędne do wygranej mniej wartościowe zwierzęta
#' @param zagroda - wektor reprezentujący liczbę poszczególnych rodzajów zwierząt w zagrodzie
#' @return wektor reprezentujący stan zagrody po dokonianiu wymiany
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export

sprzedaj_drugiego_konia  <- function( zagroda ) {
  stado <- c( 60, 24, 20, 12, 6, 4, 2 )
  zagroda[indeks("kon")] <- zagroda[indeks("kon")] - 1
  do_zaplaty <- -wartosc("kon")
  ilosc <- c( 6, 1, 1, 1 )

  while (do_zaplaty < 0) {
    for (i in c( "krowa", "swinia", "owca", "krolik") ) {
      if (do_zaplaty < 0 &&
          stado[indeks(i)] >= ilosc[indeks(i)] &&
          do_zaplaty + wartosc(i) <= 0) {
        zagroda <- wymiana(zagroda, "kon", i, 0, ilosc[indeks(i)])
        do_zaplaty <- do_zaplaty + wartosc(i)
      }
    }
  }

  zagroda
}
