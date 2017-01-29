#' @title Funkcja zwracająca indeks zwierzęcia
#'
#' @description Funkcja zwracająca indeks zwierzęcia
#'
#' @param zwierze - parametr okreslający zwierze
#' @return funkcja zwraca uniwersalny indeks danego zwierzęcia
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}


indeks <- function( zwierze ) {

  lista <- c("krolik"    =  1,
             "owca"      =  2,
             "swinia"    =  3,
             "krowa"     =  4,
             "kon"       =  5,
             "maly_pies" =  6,
             "duzy_pies" =  7,
             "wilk"      = -1,
             "lis"       = -2 )
  lista[zwierze];
}
