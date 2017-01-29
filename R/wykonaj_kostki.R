#' @title Funkcja wprowadzająca zmiany w zagrodzie na podstawie wyniku uzyskanego na kostkach.
#'
#' @description Funkcja zmienia liczebność zagrody gracza na podstawie uzyskanych wyniów na kostkach
#'
#' @param zagroda - argument określający obecny stan zagrody gracza.
#' @param wynik - argument określający wynik losowania na kostkach (wektor długości 2).
#' @return funkcja zwraca nowy stan zagrody gracza
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}

wykonaj_kostki <- function( zagroda, wynik ) {
  wynik1 = indeks(wynik[1])
  wynik2 = indeks(wynik[2])

  if (wynik1 == indeks("wilk")) {
    if ( zagroda[indeks("duzy_pies")] > 0 ) {
      zagroda[indeks("duzy_pies")] <- zagroda[indeks("duzy_pies")] - 1
    }
    else {
      for ( i in c( "krolik", "owca", "swinia", "krowa" ) )
        zagroda[indeks(i)] <- 0
    }
  }


  if ( wynik2 == indeks("lis") ) {
    if ( zagroda[indeks("maly_pies")] > 0 ) {
      zagroda[indeks("maly_pies")] <- zagroda[indeks("maly_pies")] - 1
    } else {
      zagroda[indeks("krolik")] <- 0
    }
  }

  if ( wynik1 == wynik2 ) {
    stopifnot(wynik1 > 0)
    zagroda[wynik1] <- zagroda[wynik1] + ( zagroda[wynik1] + 2 ) %/% 2
  } else {
    if ( wynik1 > 0 )
      zagroda[wynik1] <- zagroda[wynik1] + ( zagroda[wynik1] + 1 ) %/% 2

    if ( wynik2 > 0 )
      zagroda[wynik2] <- zagroda[wynik2] + ( zagroda[wynik2] + 1 ) %/% 2
  }

  zagroda <- pmin(zagroda, c( 60, 24, 20, 12, 6, 4, 2 ))

  zagroda
}
