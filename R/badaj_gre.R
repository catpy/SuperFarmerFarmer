#' @title Funkcja wykonująca symulację zadanej strategii określoną liczbę razy.
#'
#' @description Funcja \code{badaj_gre} symuluje \code{liczba_iteracji} rozgrywek
#' i zwraca wektor czasów rozgrywki.
#'
#' @param strategia - argument określający funkcję strategii gry
#' @param liczba_iteracji - argument okreslajacy dla ilu wywolan gry chcemy otrzymac
#' wynik funkcji. Domyslna wartosc to 10000.
#' @return funkcja zwraca wektor długości \code{liczba_iteracji} czasów kolejnych rozgrywek.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export

badaj_gre <- function( strategia, liczba_iteracji = 1000 ) {
  wyniki <- 1:liczba_iteracji
  for( i in 1:liczba_iteracji ) {
    wyniki[i] <- gra( strategia )
  }
  wyniki
}
