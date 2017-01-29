#' @title Funkcja wymieniająca zwierzę na inne
#'
#' @description Funcja dodaje liczbę kupionych do zagrody i odejmuje jej równowartość w sprzedawanych zwierzętach.
#' @param zagroda - wektor reprezentujący liczbę poszczególnych rodzajów zwierząt w zagrodzie
#' @param co_sprzedaj - nazwa zwierzęcia sprzedawanego
#' @param co_kup - numer zwierzęcia kupowanego
#' @param ile_sprzedaj - liczba zwierząt sprzedawanych
#' @param ile_kup - liczba zwierząt kupowanych
#' @return wektor reprezentujący stan zagrody po dokonianiu wymiany
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export


wymiana <- function( zagroda,
                     co_sprzedaj,
                     co_kup,
                     ile_sprzedaj,
                     ile_kup ) {
  if(ile_kup > 0) {
    sprzedaz = indeks(co_sprzedaj)
    kupno = indeks(co_kup)
    zagroda[kupno] <- zagroda[kupno] + ile_kup
    zagroda[sprzedaz] <- zagroda[sprzedaz] - ile_sprzedaj
  }
  zagroda
}
