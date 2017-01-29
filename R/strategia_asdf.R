#' @title Podstawowa strategia rozgrywnia partii (niezgodna z zasadami)
#'
#' @description Funcja \code{strategia_asdf} dla dowolnego stanu zagrody zwraca zagrodę w stanie wygrywającym.
#'
#' @param zagroda - argument będący wektorym określającym obecny stan zagrody gracza
#' @return funkcja zwraca nowy wektor \code{zagroda} po wykonaniu przez gracza ruchu.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export


strategia_asdf <- function( zagroda ) {
  zagroda = c( 1, 1, 1, 1, 1, 0, 0 )
  zagroda
}
