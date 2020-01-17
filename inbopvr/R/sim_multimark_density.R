
#' Simuleer een dataset met vangstgegevens om de palingdensiteitsberekening uit te voeren
#'
#' @param Npop De echte populatiegrootte
#' @param S Hoeveel vissen worden er gemiddeld per vangst gevangen. Ofwel een vector met lengte 1, ofwel een vector met lengte \code{Ndates}
#' @param Ndates Hoeveel dagen naëen vis je om de populatieschatting te verbeteren
#' @param Nsim Hoeveel keer moet de routine opnieuw lopen om een zicht te krijgen op de variabiliteit van de schattingen
#' @param verbose indien TRUE toon op het scherm extra informatie over het verloop van het proces zoals aan welke iteratie de procedure zit
#' @param ... argumenten die kunnen doorgegeven worden aan de door de routine opgeroepen functies \code{\link{multimark_calcN}} en\code{\link{multimark_popsize_iterative}}.
#' @return data.frame met per Nsim, per Ndates de vangstgrootte, de recaptures, en de geschatte populatie volgens de methode op basis van de informatie tot de respectievelijke vangstdag
#' @export
#' @importFrom stats rbinom
#' @importFrom dplyr bind_rows
#' @examples
#' schattingen <- sim_multimark_density(Npop = 200, S = 30, Nsim = 100)
#' head(schattingen)
#' hist(schattingen[schattingen$day == 5, "N_estim"], main = "", xlab = "estimation of N")
#' abline(v = 200, col = "red", lwd = 3)
#'
sim_multimark_density <- function(Npop = 200, S = 30, Ndates = 5, Nsim = 200, verbose = FALSE, ...){
  prob = S / rep(Npop, Ndates)
  calcdata <- NULL
  for (k in 1:Nsim) {
    if (verbose) cat("iteratie", k, "/", Nsim, "\n")
    visvangst <- NULL #initialiseer vangst
    totalmarks <- recap <- catch <- found <- N <- NULL #intialiseer variabelen
    for (i in 1:Ndates) {
      visvangst <- cbind(visvangst, rbinom(Npop, size = 1, prob = prob[i])) #voer een vangst uit
      catch[1] = sum(visvangst[,1]) #eerste vangst
      recap[1] = 0 #nog geen markeringen voorheen
    }
    for (i in 2:Ndates) {
      has_prevmark <- rowSums(visvangst[,1:(i - 1), drop = FALSE]) > 0 #geeft TRUE indien de vis al eerder gevangen werd
      is_catched_now <- visvangst[,i] #geeft TRUE (1) als de vis nu gevangen is
      totalmarks[i] <- sum(has_prevmark)    #aantal gemarkeerde vissen voor deze vangst
      catch[i] <- sum(visvangst[,i])       #huidige vangstgrootte
      recap[i] <- sum(has_prevmark & is_catched_now) #aantal hervangsten
      N[i] <- catch[i] / recap[i] * totalmarks[i] #een benadering van N, als startwaarde voor de functie calcN hieronder
    }

    #Voer het optimalisatie-algoritme uit
    calcs <- numeric(length = Ndates)
    for (i in 2:Ndates) {
      calcs[i] <- multimark_calcN(N = Npop, catch = catch[1:i], recapture = recap[1:i])["N"]
    }
    calcdata <- bind_rows(calcdata,
                          data.frame(iteration = k,
                                     day = 1:Ndates,
                                     catch = catch,
                                     recapture = recap,
                                     N_estim = calcs))
  }
  calcdata
}


#Toont gewoon de laatste berekening (die zal mogelijks ver van Npop liggen, maar binnen het betrouwbaarheidsinterval)
# plot(1:Ndates, calcs, type = "b", col = "red"); abline(h = Npop, v = gemeten)
#


