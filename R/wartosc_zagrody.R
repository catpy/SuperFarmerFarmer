#' @title Funkcja obliczająca łączną wartość zagrody w królikach.
#'
#' @description Funcja mnoży wektor reprezentujący zwierzeta w zagrodzie przez wektor ich wartoœci w królikach.
#'
#' @param zagroda - wektor reprezentujący liczbę poszczególnych rodzajów zwierząt w zagrodzie
#' @return wartość wszystkich zwierząt w zagrodzie wyrażona w liczbie królików
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export

wartosc_zagrody <- function( zagroda ) {
  stopifnot( length( zagroda ) == 7 )
  sum( zagroda * c( 1, 6, 12, 36, 72, 6, 36 ) )
}
