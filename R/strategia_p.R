#' @title Podstawowa strategia rozgrywnia partii
#'
#' @description Funcja \code{strategia_p} dla dowolnego stanu zagrody zwraca zagrodę w następnym stanie.
#'
#' @param zagroda - argument będący wektorym określającym obecny stan zagrody gracza
#' @return funkcja zwraca nowy wektor \code{zagroda} po wykonaniu przez gracza ruchu.
#' @author
#' Katarzyna Pyrtek, \email{kat.pyrtek@gmail.com}
#' @author
#' Mateusz Siwiec, \email{ms332521@students.mimuw.edu.pl}
#'
#' @export
#'

strategia_p <- function( zagroda ) {
  #1. Do wartości zagrody gracza równej 33, trzymamy same króliki.
  total = wartosc_zagrody( zagroda )
  if ( total <= 33 ) {
    if (zagroda[indeks("swinia")] > 0) {
      zagroda <- wymiana( zagroda, "swinia", "krolik", 1, 12 )
    } else if (zagroda[indeks("owca")] > 0) {
      zagroda <- wymiana( zagroda, "owca", "krolik", 1, 6 )
    }
  }

  #2. Dla wartości od 33 do 46: mały pies i same króliki.

  if ( total > 33 && total <= 46 ) {
    if ( zagroda[indeks("maly_pies")] == 0 &&
         zagroda[indeks("krolik")] > 9 ) {
      zagroda <- wymiana( zagroda, "krolik", "maly_pies", 6, 1 )
    } else if ( zagroda[indeks("swinia")] > 0 ) {
      zagroda <- wymiana( zagroda, "swinia", "krolik", 1, 12 )
    } else if ( zagroda[indeks("owca")] > 0 ) {
      zagroda <- wymiana( zagroda, "owca", "krolik", 1, 6 )
    }
  }

  #3. Od 47 do 71: do 40 królików, mały pies, reszta - świnie i owce.

  if ( total >= 47 && total <= 71 ) {
    if ( zagroda[indeks("maly_pies")] == 0 && zagroda[indeks("krolik")] > 6 )
    {
      zagroda <- wymiana( zagroda, "krolik", "maly_pies", 6, 1 )
    } else if ( zagroda[indeks("swinia")] < (zagroda[indeks("krolik")] - 34) / 12 ) {
      zagroda <- wymiana( zagroda, indeks("krolik"), indeks("swinia"), 12, 1 )
    } else if ( zagroda[indeks("owca")] < (zagroda[indeks("krolik")] - 35) / 6 ) {
      zagroda <- wymiana( zagroda, "krolik", "owca", 6, 1 )
    }
  }

  #Kupujemy pierwszego konia
  if ( total >= 72 && zagroda[indeks("kon")] == 0 )
  {
    zagroda <- sprzedaj_kilka_tanszych(zagroda, "kon")
  }

  #Od 72 postępujemy tak samo, z tym, że w zagrodzie mamy już konia.
  #4. Do wartości zagrody 105 trzymamy same króliki i 1 konia

  if ( zagroda[indeks("kon")] == 1 && total >= 72 && total < 105 ) {
    if ( zagroda[indeks("swinia")] > 0 ) {
      zagroda <- wymiana( zagroda, "swinia", "krolik", 1, 12 )
    } else if (zagroda[indeks("owca")] > 0) {
      zagroda <- wymiana( zagroda, "owca", "krolik", 1, 6 )
    }
  }

  #5. Dla wartości od 105 do 118: 1 koń, mały pies i same króliki.

  if ( zagroda[indeks("kon")] == 1 && total >= 105 && total <= 118 ) {
    if ( zagroda[indeks("maly_pies")] == 0 && zagroda[indeks("krolik")] > 9 ) {
      zagroda <- wymiana( zagroda, "krolik", "maly_pies", 6, 1 )
    } else if ( zagroda[indeks("swinia")] > 0 ) {
      zagroda <- wymiana( zagroda, "swinia", "krolik", 1, 12 )
    } else if (zagroda[indeks("owca")] > 0) {
      zagroda <- wymiana( zagroda, "owca", "krolik", 1, 6 )
    }
  }


  #6. Dla 119 do 127: 1 koń, mały pies, reszta świnie i owce.

  if ( zagroda[indeks("kon")] == 1 && total >= 119 && total < 127 ) {
    zagroda <- prawie_wygrana( zagroda )
  }

  #7. Dla wartości powyżej 127 robimy wymiany i kończymy grę. Ponadto,
  #   jeżeli wartość zagrody przekracza 144 to kupujemy drugiego konia
  #   (pod warunkiem, że brakuje nam co najmniej 2 zwierząt), a w następnej
  #   turze zakupimy brakujące zwierzęta.

  if ( total >= 144 ) {
    if ( zagroda[indeks("kon")] < 2 && sum(zagroda[1:5] == 0) > 1 ) {
      zagroda <- sprzedaj_kilka_tanszych( zagroda, "kon" )
    } else if ( zagroda[indeks("kon")] > 1 ) {
      zagroda <- sprzedaj_drugiego_konia( zagroda )
    }
  }


  if (total >= 127)
  {
    if (( zagroda[indeks("krolik")] == 0 ||
          zagroda[indeks("owca")] == 0 ||
          zagroda[indeks("swinia")] == 0 ) &&
          zagroda[indeks("krowa")] > 1 ) {
      zagroda <- kup_kilka_tanszych( zagroda, "krowa" )
    } else if (( zagroda[indeks("krolik")] == 0 ||
                 zagroda[indeks("owca")] == 0) &&
                 zagroda[indeks("swinia")] > 1 ) {
      zgroda <- kup_kilka_tanszych( zagroda, "swinia" )
    } else if (zagroda[indeks("krolik")] == 0 &&
               zagroda[indeks("owca")] > 1) {
      zagroda <- kup_kilka_tanszych( zagroda, "owca" )
    } else if ( zagroda[indeks("owca")] == 0 &&
                zagroda[indeks("krolik")] > 6 ) {
      zagroda <- sprzedaj_kilka_tanszych( zagroda, "owca" )
    } else if (zagroda[indeks("swinia")] == 0) {
      zagroda <- sprzedaj_kilka_tanszych( zagroda, "swinia" )
    } else if (zagroda[indeks("krowa")] == 0) {
      zagroda <- sprzedaj_kilka_tanszych( zagroda, "krowa" )
    }
  }
  zagroda <- pmin(zagroda, c( 60, 24, 20, 12, 6, 4, 2 ))
  zagroda
}

