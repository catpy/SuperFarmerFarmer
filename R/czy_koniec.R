#' @title Funkcja prywatna sprawdzająca warunek zakończenia gry.
#'
#' @description Funkcja \code{czy_koniec} zwraca \code{true} wtedy i tylko wtedy, gdy spełniony jest warunek
#' zakończenia rozgrywki i \code{false} w przeciwnym razie.
#'
#' @param zagroda - argument określający obecny stan zagrody gracza.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}

czy_koniec <- function( zagroda ) {
  all( zagroda >= c( 1, 1, 1, 1, 1, 0, 0 ) )
}
