#' @title Funkcja wykonująca pojedynczą symulację zadanej strategii
#'
#' @description Funcja \code{gra} symuluje pojedynczą rozgrywkę i zwraca czas jej trwania.
#'
#' @param strategia - argument określający funkcję strategii gry
#' @return funkcja zwraca czas trwania rozgrywki podny w liczbie rund potrzebnych do uzyskania
#' przez gracza zagrody spełniającej warunek końcowy.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export

gra <- function( strategia ) {
  runda <- 0;
  zagroda <- c( 0, 0, 0, 0, 0, 0, 0 )
  while ( !czy_koniec( zagroda ) ) {
    zagroda <- strategia( zagroda )
    if ( !czy_koniec( zagroda ) ) {
      wynik <- rzuc_kostkami()
      zagroda <- wykonaj_kostki( zagroda, wynik ) # test poprawnosci
      runda <- runda + 1
    }
  }
  runda
}
