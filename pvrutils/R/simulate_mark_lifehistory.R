#' Simulate a lifehistory for an individual with capture, recapture, resighting and ring recovery (close to Barker model)
#'
#' The model assumes that there is a recapture event each fixed period t, t+1, t+2 And that there are alive ring resightings between the timesteps, and/or dead recovery between the time steps
#'
#' @param ringyear the year in the survey on which the animal is ringed
#' @param endyear the last year of the survey, assuming it starts by time 1 and there is a recapture every year
#' @param ring_age the age on which the animal is ringed
#' @param age_based named vector with names S, p, R, Rprime, F, Fprime that is TRUE when the model is age-based and FALSE when time-based, defaults to FALSE. When age_based is a simple logical constant, it is valid for all parameters
#' @param S constant or vector with the probability an animals that lives at i remains alive at i + 1
#' @param p constant or vector with the probability an animal at risk of capture at i is captured at i
#' @param r constant or vector with that dies in i, i + 1 is found dead and the band reported
#' @param R constant or vector with the probability an animal that survives from i to i + 1 is resighted (alive) some time between i and i + 1
#' @param Rprime constant or vector with the probability an animal that dies over the interval from i to i + 1, without being found dead, is resighted alive in i, i + 1 before it died
#' @param F constant or vector with the probability an animal at risk of capture at i is at risk of capture at i + 1
#' @param Fprime constant or vector with the probability an animal not at risk of capture at i is at risk of capture at i + 1 (NB: this differs from the definition in Barker, 1997) #'
#' @return a lifehistory string for the aimal
#' @export
#'
#' @examples
#'endyear <- 10
#'indivs <- expand.grid(Time = 1:1, Id = 1:50, Age = 0:0)
#'#Burnham model: R, Rprime, Fprime = 0
#'for (i in 1:nrow(indivs)) {
#'  indivs$ch[i] <-
#'    sim_individual(ringyear = indivs$Time[i], endyear = endyear,
#'                   ring_age = indivs$Age[i], age_based = c(S = TRUE, p = TRUE),
#'                   S = 0.7, p = 0.8, r = 0.2, R = 0.0, Rprime = 0.0, F = 1.0, Fprime = 0.0)
#'}
sim_individual <- function(ringyear, endyear, ring_age = 0, age_based = FALSE, S, p, r, R, Rprime, F = 1, Fprime = 0) {
  alive <- c(rep(1, ringyear), rep(NA, endyear - ringyear))
  age <- c(rep(NA, ringyear - 1), ring_age + (0:(endyear - ringyear)))
  times <- 1:endyear

  #> initialize variables

  if (length(age_based == 1) && is.null(names(age_based))){
    if (age_based == TRUE) {
      age_based <- rep(TRUE, 7)
    } else {
      age_based <- rep(FALSE, 7)
    }
    names(age_based) <- c("S", "p", "r", "R", "Rprime", "F", "Fprime")
  }

  prob_alive <- c(rep(1, ringyear),        rep(NA, endyear - ringyear))
  prob_p     <- c(rep(0, ringyear - 1), 1, rep(NA, endyear - ringyear))
  prob_r   <- prob_R <- prob_Rprime  <-  prob_F <- prob_Fprime <- rep(NA, endyear - 1)
  L  <- c(rep(0, (ringyear - 1)), 1, rep(NA, endyear - ringyear))
  D  <- c(rep(0, (ringyear - 1)), rep(NA, endyear - ringyear), 0) #laatste jaar per definitie 0
  FF <- c(rep(0, (ringyear - 1)), rep(NA, endyear - ringyear), 0) #laatste jaar per definitie 0
  FA <- c(rep(0, (ringyear - 1)), rep(NA, endyear - ringyear), 0) #laatste jaar per definitie 0

  if (length(S)  == 1)     S      <- rep(S,      max(c(endyear, age), na.rm = TRUE))
  if (length(p)  == 1)     p      <- rep(p,      max(c(endyear, age), na.rm = TRUE))
  if (length(R) == 1)      R      <- rep(R,      max(c(endyear, age), na.rm = TRUE))
  if (length(Rprime) == 1) Rprime <- rep(Rprime, max(c(endyear, age), na.rm = TRUE))
  if (length(r)  == 1)     r      <- rep(r,      max(c(endyear, age), na.rm = TRUE))
  if (length(F) == 1)      F      <- rep(F,      max(c(endyear, age), na.rm = TRUE))
  if (length(Fprime) == 1) Fprime <- rep(Fprime, max(c(endyear, age), na.rm = TRUE))

  if ("S" %in% names(age_based)){
    if (age_based["S"] == TRUE) {
      S[1:(endyear)]  <- S[age[1:endyear]  + 1]
    } else {
      S[1:(endyear)]  <- S[times]
    }
  }
  if ("p" %in% names(age_based)){
    if (age_based["p"] == TRUE) {
      p[1:(endyear)]  <- p[age[1:endyear]  + 1]
    } else {
      p[1:(endyear)]  <- p[times]
    }
  }
  if ("r" %in% names(age_based)){
    if (age_based["r"] == TRUE) {
      r[1:(endyear)]  <- r[age[1:endyear]  + 1]
    } else {
      r[1:(endyear)]  <- r[times]
    }
  }
  if ("R" %in% names(age_based)){
    if (age_based["R"] == TRUE) {
      R[1:(endyear)]  <- R[age[1:endyear]  + 1]
    } else {
      R[1:(endyear)]  <- R[times]
    }
  }
  if ("Rprime" %in% names(age_based)){
    if (age_based["Rprime"] == TRUE) {
      Rprime[1:(endyear)]  <- Rprime[age[1:endyear]  + 1]
    } else {
      Rprime[1:(endyear)]  <- Rprime[times]
    }
  }
  if ("F" %in% names(age_based)){
    if (age_based["F"] == TRUE) {
      F[1:(endyear)]  <- F[age[1:endyear]  + 1]
    } else {
      F[1:(endyear)]  <- F[times]
    }
  }
  if ("Fprime" %in% names(age_based)){
    if (age_based["Fprime"] == TRUE) {
      Fprime[1:(endyear)]  <- Fprime[age[1:endyear]  + 1]
    } else {
      Fprime[1:(endyear)]  <- Fprime[times]
    }
  }
print(S)
print(p)

  #> Run simulation

  for (t in ringyear:(endyear - 1)) {
    prob_alive[t + 1] <- prob_alive[t] * S[t]
    alive[t + 1] <- rbinom(n = 1, size = 1, prob = alive[t] * S[t])
    if (alive[t + 1]) {
      livedet <- rbinom(n = 1, size = 1, prob = R[t])
      D[t]     <- ifelse(livedet, 2, 0)
      L[t + 1] <- rbinom(n = 1, size = 1, prob = p[t])

    } else {
      livedet <- rbinom(n = 1, size = 1, prob = Rprime[t])
      recover  <- rbinom(n = 1, size = 1, prob = r[t])
      D[t] <- ifelse(livedet,
                     ifelse(recover, 1, 2),
                     ifelse(recover, 1, 0)) * alive[t]
      L[t + 1] <- 0
    }
  }
  #print(cbind(times, age, alive, p_alive, L, D))
  paste(paste(L, D, sep = ""), collapse = "")
}

#
# sim_individual <- function(ringyear, endyear, ring_age = 0, age_based = TRUE, S, p, R, Rprime, F, Fprime) {
#   alive <- c(rep(1, ringyear), rep(NA, endyear - ringyear))
#   age <- c(rep(NA, ringyear - 1), ring_age + (0:(endyear - ringyear)))
#   times <- 1:endyear
#
#   p_alive <- c(rep(1, ringyear),        rep(NA, endyear - ringyear))
#   p_P     <- c(rep(0, ringyear - 1), 1, rep(NA, endyear - ringyear))
#   p_LL    <- p_LD <- p_R  <- rep(NA, endyear - 1)
#   L <- c(rep(0, (ringyear - 1)), 1, rep(NA, endyear - ringyear))
#   D <- c(rep(0, (ringyear - 1)), rep(NA, endyear - ringyear), 0) #laatste jaar per definitie 0
#
#   if (length(S)  == 1) S  <- rep(S,  max(c(endyear, age), na.rm = TRUE))
#   if (length(LL) == 1) LL <- rep(LL, max(c(endyear, age), na.rm = TRUE))
#   if (length(LD) == 1) LD <- rep(LD, max(c(endyear, age), na.rm = TRUE))
#   if (length(R)  == 1) R  <- rep(R,  max(c(endyear, age), na.rm = TRUE))
#   if (length(P)  == 1) P  <- rep(P,  max(c(endyear, age), na.rm = TRUE))
#
#   if (age_based) {
#     S[1:(endyear)]  <- S[age[1:endyear]  + 1]
#     LL[1:(endyear)] <- LL[age[1:endyear] + 1]
#     LD[1:(endyear)] <- LD[age[1:endyear] + 1]
#     R[1:(endyear)]  <- R[age[1:endyear]  + 1]
#     P[1:(endyear)]  <- P[age[1:endyear]  + 1]
#   } else {
#     S[1:(endyear)]  <- S[times]
#     LL[1:(endyear)] <- LL[times]
#     LD[1:(endyear)] <- LD[times]
#     R[1:(endyear)]  <- R[times]
#     P[1:(endyear)]  <- P[times]
#   }
#
#   for (t in ringyear:(endyear - 1)) {
#     p_alive[t + 1] <- p_alive[t] * S[t]
#     alive[t + 1] <- rbinom(n = 1, size = 1, prob = alive[t] * S[t])
#     if (alive[t + 1]) {
#       livedet <- rbinom(n = 1, size = 1, prob = LL[t])
#       D[t]     <- ifelse(livedet, 2, 0)
#       L[t + 1] <- rbinom(n = 1, size = 1, prob = P[t])
#
#     } else {
#       livedet <- rbinom(n = 1, size = 1, prob = LD[t])
#       recover  <- rbinom(n = 1, size = 1, prob = R[t])
#       D[t] <- ifelse(livedet,
#                      ifelse(recover, 1, 2),
#                      ifelse(recover, 1, 0)) * alive[t]
#       L[t + 1] <- 0
#     }
#   }
#   #print(cbind(times, age, alive, p_alive, L, D))
#   paste(paste(L, D, sep = ""), collapse = "")
# }
#


# endyear <- 10
# indivs <- expand.grid(Time = 1:2, Id = 1:1, Age = 0:4)
# for (i in 1:nrow(indivs)) {
#   indivs$ch[i] <-
#     sim_individual(ringyear = indivs$Time[i], endyear = endyear, ring_age = indivs$Age[i],
#                    age_based = c(S = TRUE, p = FALSE),
#                    S = c(0.7, 0.9, 0.4, 0.5, 0.8, rep(0.7, 8)), R = 0.0, Rprime = 0.0, r = 0.0, p = c(8:1/10, rep(0.2, 4)))
# }
# lhistory <- indivs %>% group_by(Age, ch) %>% summarize(freq = n())

