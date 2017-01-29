#' @title Funkcja zwracająca wartość danego zwierzaka
#'
#' @description Funkcja \code{wartosc} zwraca wartość zwierzeka podanego jako argument.
#'
#' @param zwierze - argument określający typ zwierzaka.
#' @return Funkcja zwraca wartość podanego zwierzaka.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}

wartosc <- function( zwierze ) {

  wartosci = c( 1, 6, 12, 36, 72, 6, 36 )
  wartosci[indeks(zwierze)]
}
