#' @title Funkcja wykonująca rzut dwiema kostkami określonymi w zasadach gry.
#'
#' @description Bezparametrowa funkcja \code{rzuc_kostkami} zwraca wektor wyników rzutu na dwóch kostkach.
#'
#' @return funkcja zwraca wektor długości dwa z wartościomi określającymi wyniki na obydwu kostkach
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}

rzuc_kostkami <- function( ) {
  kostka1 <- c( "wilk",
                "krowa",
                "swinia",
                "owca",
                "owca",
                "owca",
                "krolik",
                "krolik",
                "krolik",
                "krolik",
                "krolik",
                "krolik" )

  kostka2 <- c( "lis",
                "kon",
                "swinia",
                "swinia",
                "owca",
                "owca",
                "krolik",
                "krolik",
                "krolik",
                "krolik",
                "krolik",
                "krolik" )
  wynik1 <- sample( kostka1, 1 )
  wynik2 <- sample( kostka2, 1 )

  wynik = c( wynik1, wynik2 )
  wynik
}
