###paling densiteitsschatting


#' Berekening van populatiegrootte (N) door iteratieve schatting voor multiple mark-recapture
#'
#' Bereken een nieuwe N en de delta_n op basis van de formules zoals beschreven in het handboek Ricker (1968). Dit is meer specifiek enkel geldig voor de schatting van populatiegrootte waarbij op verschillende momenten kort na elkaar de dieren gevangen worden, en alle nog-niet-gemerkte dieren gemerkt worden zodat ze herkenbaar zijn bij de volgende vangst. De basis voor deze berekening is dat het totaal niet-gemerkte dieren in de populatie gelijk gesteld wordt aan de kans dat een dier tijdens geen enkele vangst gemerkt wordt. Zowel N als delta_n zullen convergeren naar een vaste waarde en dan stopt het iteratieve proces. Deze N wordt de populatieschatting en delta_n wordt gebruikt om de onzekerheid in de schatting te bepalen, namelijk SE(N) = sqrt((N-U)/delta_n). De kans dat een dier in geen enkele vangst gevangen wordt is het product van de kansen dat ze tijdens een bepaalde vangstoccasie gevangen worden. PN(1,2,...,X) = PN(1)PN(2)...PN(X) met PN(i) = (N - catch_i)/N. Voor de berekening van delta_n wordt de geschatte populatie vergeleken met de waarde van de formule waarbij we N-1 invullen waar N voorkomt. Indien de schatting voor N-1 nagenoeg 1 minder is dan de schatting voor N, dan wil dit zeggen dat de formule even goed geldt voor een andere N waarde dan de geschatte waarde, en is er dus een grotere onzekerheid op de schatting, aangezien andere N waarden even goed werken in de formule. De nieuwe inschatting voor N wordt bekomen door een soort interpolatie. Er wordt gekeken hoeveel N * PN afwijkt van N - totalmarks en de nieuwe inschatting voor N is dan N + dit verschil (wat ook negatief kan zijn) gedeeld door de delta_n. De deling door delta_n zorgt dat er veel sneller convergentie komt, anders wordt dat een zeer traag assymptotisch proces waarvoor heel veel iteraties nodig zijn
#'
#' @param N Vorige populatieschatting
#' @param catch de totale vangsten voor iedere meetoccasie
#' @param recapture de hoeveelheid hervangsten voor iedere meetoccasie (de eerste is standaard 0)
#'
#' @return vector die de nieuwe populatieschatting bevat alsook de berekende delta
#' @export
#'
multimark_calcN <- function(N, catch, recapture){

  #Berekening delta_n
  PN <- prod((N - catch) / N)
  PNmin1 <- prod((N - 1 - catch) / (N - 1))
  delta_N <- 1 + (N - 1) * PNmin1  -  N * PN

  #berekening totaal aantal gemarkeerde dieren
  newmarks <- catch - recapture
  totalmarks <- sum(catch - recapture)

  #berekening nieuwe N
  new_N <- N  +  (N * PN - (N - totalmarks)) / delta_N

  #output
  return(c(N = new_N, delta_N = delta_N))
}


#' Schatting populatie N voor multiple capture mark recapture
#'
#' Iteratieve methode om populatie N in te schatten voor meerdere vangstoccasies waar iedere vis die niet nog niet gemerkt was van ee markering voorzien wordt
#'
#' @param data dataset die de kolommen bevat die onder 'colnames' staan
#' @param colnames de kolomnamen die overeenkomen met vangst en hervangst respectievelijk
#' @param max_iter maximum aantal iteraties dat de procedure uitvoert vooraleer op te geven
#' @param multiplier vermeningvuldigingsfactor voor de startschatting die standaard op het aantal markeringen ingesteld staat
#' @param plot toon een plot die de evolutie van de schatting N en de delta_N toont, bij convergentie zal dat met een vlakkel lijn eindigen
#' @param return Welke velden moeten teruggegeven worden. Je kan 1 of meerdere uit location, year, N, seN en delta_N kiezen
#'@importFrom graphics par
#' @return data.frame met 1 rij met de kolommen gespecifieerd in de parameter 'return'
#' @export
#' @examples{
#' df <- data.frame(catch = c(36, 31, 25, 34), recapture = c( 0,  4,  7, 15))
#' PopEstim <-
#'multimark_popsize_iterative(data = df, multiplier = 1, plot = TRUE)
#'PopEstim
#'}
multimark_popsize_iterative <- function(data, colnames = c("catch", "recapture"),
                                          max_iter = 100, multiplier = 2,
                                          plot = TRUE, return = c("N", "seN", "delta_N", "lcl", "ucl")){
  #check inputformaat
  vangst <- data[,colnames[1]]
  hervangst <- data[, colnames[2]]

  #nieuw markeringen
  nieuwgemarkeerd <- (vangst - hervangst)
  totalmarks <- sum(nieuwgemarkeerd)

  #startwaarden voor het iteratieve proces
  N_hat <-  multiplier * totalmarks #arbitraire keuze voor startwaarde geschatte N (N_hat)
  N_new <- N_hat
  delta_N <- NULL
  N_estimates <- N_hat
  counter <- 1

  #het iteratieve proces zelf. telkens wordt de functie calcN opgeroepen om een nieuwe schatting te maken
  while (counter == 1 | (abs(N_hat - N_new) > 0.05 & counter < max_iter)) {
    N_hat <- N_new
    rv <- multimark_calcN(N_hat, vangst, hervangst)
    N_new <- unname(rv["N"])
    delta <- abs(unname(rv["delta_N"]))
    N_estimates <- c(N_estimates, N_new)
    delta_N <- c(delta_N, delta)
    counter <- counter + 1
  }
  #standard error op N, volgens handboek Ricker(1968)
  seN <- sqrt((N_new - totalmarks) / delta)
  if (plot == TRUE) {
    par(mfrow = c(1,2))
    plot(N_estimates, type = "l")
    plot(delta_N, type = "l")
    par(mfrow = c(1,1))
  }

  #output
  rv <- data.frame(N = unname(N_new), seN = unname(seN), delta_N = unname(delta), lcl = N_new - 2 * seN, ucl = N_new + 2 * seN)
  return(rv[,return,drop = FALSE])
}
