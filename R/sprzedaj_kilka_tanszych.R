#' @title Funkcja wymieniająca kilka tańszych zwierząt na jedno droższe.
#'
#' @description Jeśli jesteœmy w stanie sobie pozwoliæ na kupowane zwierzê i zachować po tej transakcji co najmniej jeden egzemplarz ze wszytkich wcześniej posiadanych zwierząt, to funkcja kupuje to zwierzę.
#'
#' @param zagroda - wektor reprezentujący liczbę poszczególnych rodzajów zwierząt w zagrodzie
#' @param zwierze - numer zwierzęcia, które chcemy kupić
#' @return wektor reprezentujący stan zagrody po dokonianiu odpowiednich wymian
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export
#'

sprzedaj_kilka_tanszych <- function( zagroda, zwierze ) {

  if ( indeks(zwierze) == indeks("kon") ) {
    wektor_ograniczen <- c( 0, 0, 0, 0, 0, 0, 0 )
  } else {
    wektor_ograniczen <- c( 1, 1, 1, 1, 1, 0, 0 )
  }
  zagroda[indeks(zwierze)] <- zagroda[indeks(zwierze)] + 1
  do_zaplaty <- -wartosc( zwierze )


  for (i in c( "krolik", "owca", "swinia", "maly_pies" ) ) {
    while ( do_zaplaty < 0 &&
            zagroda[indeks(i)] > wektor_ograniczen[indeks(i)] ) {
      zagroda <- wymiana( zagroda, i, zwierze, 1, 0)
      do_zaplaty <- do_zaplaty + wartosc(i)
    }
  }


  zagroda
}
