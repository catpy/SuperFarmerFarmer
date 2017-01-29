#' @title Funkcja wykonująca część strategii dla momentu, kiedy wartość zagrody jest bliska 127
#'
#' @description Dokonuje odpowiednich wymian niezbędnych do uzyskania co najmniej jednego egzemplarza zwierzęcia z każdego gatunku.
#'
#' @param zagroda - wektor reprezentujący liczbę poszczególnych rodzajów zwierząt w zagrodzie
#' @return wektor reprezentujący stan zagrody po dokonianiu odpowiednich wymian
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export


prawie_wygrana <- function(zagroda) {
  if (zagroda[indeks("maly_pies")] == 0 && zagroda[indeks("krolik")] > 9) {
    zagroda <- wymiana(zagroda, "krolik", "maly_pies", 6, 1)
  }
  if (zagroda[indeks("krolik")] == 0 && zagroda[indeks("maly_pies")] > 0) {
    zagroda <- wymiana(zagroda, "maly_pies", "krolik", 1, 6)
  }
  if (zagroda[indeks("owca")] == 0 && zagroda[indeks("krolik")] > 6) {
    zagroda <- wymiana(zagroda, "krolik", "owca", 6, 1)
  }
  if (zagroda[indeks("swinia")] == 0 && zagroda[indeks("krolik")] > 12) {
    zagroda <- wymiana(zagroda, "krolik", "swinia", 12, 1)
  }

  zagroda
}
