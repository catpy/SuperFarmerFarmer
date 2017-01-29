#' @title Funkcja generuje histogram z wyniku funkcji badaj_grę
#'
#' @description Funkcja generuje histogram z wyniku funkcji badaj_grę
#'
#' @param wyniki - argument określający wyniki badania strategii
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#'
#' @import graphics
#' @export

rysuj_histogram <- function( wyniki ) {
  plot( table( wyniki ), type = "h", xlab = "Liczba rund", ylab = "Czestotliwosc" )
}
