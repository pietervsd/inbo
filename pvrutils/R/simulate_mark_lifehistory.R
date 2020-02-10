#' Simulate a lifehistory for an individual with capture, recapture, resighting and ring recovery (close to Barker model)
#'
#' The model assumes that there is a recapture event each fixed period t, t+1, t+2 And that there are alive ring resightings between the timesteps, and/or dead recovery between the time steps
#'
#' @param ringyear the year in the survey on which the animal is ringed
#' @param endyear the last year of the survey, assuming it starts by time 1 and there is a recapture every year
#' @param ring_age the age on which the animal is ringed
#' @param age_based ig TRUE then the parameters will vary by age, otherwise they wil vary by time
#' @param S constant or vector containing the survival by year or age depending on paramter age_based
#' @param LL constant or vector containing the chance for a sighting between capture evens if the animal survives to the next year
#' @param LD constant or vector containing the chance for a sighting between capture evens if the animal survives to the next year
#' @param R constant or vector containing the chance for a ring recovery if the animals dies between t and t+1
#' @param P constant or vector containing the chance for a recapture at t+1
#'
#' @return a lifehistory string for the aimal
#' @export
#'
#' @examples
#'endyear <- 10
#'indivs <- expand.grid(Time = 1:1, Id = 1:50, Age = 0:0)
#'for (i in 1:nrow(indivs)) {
#'  indivs$ch[i] <-
#'    sim_individual(ringyear = indivs$Time[i], endyear = endyear,
#'                   ring_age = indivs$Age[i], age_based = TRUE,
#'                   S = 0.7, LL = 0.0, LD = 0.0, R = 0.0, P = 0.8)
#'}
sim_individual <- function(ringyear, endyear, ring_age = 0, age_based = TRUE, S, LL, LD, R, P) {
  alive <- c(rep(1, ringyear), rep(NA, endyear - ringyear))
  age <- c(rep(NA, ringyear - 1), ring_age + (0:(endyear - ringyear)))
  times <- 1:endyear

  p_alive <- c(rep(1, ringyear),        rep(NA, endyear - ringyear))
  p_P     <- c(rep(0, ringyear - 1), 1, rep(NA, endyear - ringyear))
  p_LL    <- p_LD <- p_R  <- rep(NA, endyear - 1)
  L <- c(rep(0, (ringyear - 1)), 1, rep(NA, endyear - ringyear))
  D <- c(rep(0, (ringyear - 1)), rep(NA, endyear - ringyear), 0) #laatste jaar per definitie 0

  if (length(S)  == 1) S  <- rep(S,  max(c(endyear, age), na.rm = TRUE))
  if (length(LL) == 1) LL <- rep(LL, max(c(endyear, age), na.rm = TRUE))
  if (length(LD) == 1) LD <- rep(LD, max(c(endyear, age), na.rm = TRUE))
  if (length(R)  == 1) R  <- rep(R,  max(c(endyear, age), na.rm = TRUE))
  if (length(P)  == 1) P  <- rep(P,  max(c(endyear, age), na.rm = TRUE))

  if (age_based) {
    S[1:(endyear)]  <- S[age[1:endyear]  + 1]
    LL[1:(endyear)] <- LL[age[1:endyear] + 1]
    LD[1:(endyear)] <- LD[age[1:endyear] + 1]
    R[1:(endyear)]  <- R[age[1:endyear]  + 1]
    P[1:(endyear)]  <- P[age[1:endyear]  + 1]
  } else {
    S[1:(endyear)]  <- S[times]
    LL[1:(endyear)] <- LL[times]
    LD[1:(endyear)] <- LD[times]
    R[1:(endyear)]  <- R[times]
    P[1:(endyear)]  <- P[times]
  }

  for (t in ringyear:(endyear - 1)) {
    p_alive[t + 1] <- p_alive[t] * S[t]
    alive[t + 1] <- rbinom(n = 1, size = 1, prob = alive[t] * S[t])
    if (alive[t + 1]) {
      livedet <- rbinom(n = 1, size = 1, prob = LL[t])
      D[t]     <- ifelse(livedet, 2, 0)
      L[t + 1] <- rbinom(n = 1, size = 1, prob = P[t])

    } else {
      livedet <- rbinom(n = 1, size = 1, prob = LD[t])
      recover  <- rbinom(n = 1, size = 1, prob = R[t])
      D[t] <- ifelse(livedet,
                     ifelse(recover, 1, 2),
                     ifelse(recover, 1, 0)) * alive[t]
      L[t + 1] <- 0
    }
  }
  #print(cbind(times, age, alive, p_alive, L, D))
  paste(paste(L, D, sep = ""), collapse = "")
}


# endyear <- 10
# indivs <- expand.grid(Time = 1:2, Id = 1:50, Age = 0:1)
# for (i in 1:nrow(indivs)) {
#   indivs$ch[i] <-
#     sim_individual(ringyear = indivs$Time[i], endyear = endyear, ring_age = indivs$Age[i], age_based = TRUE,
#                    S = 0.7, LL = 0.0, LD = 0.0, R = 0.0, P = 0.8)
# }
# lhistory <- indivs %>% group_by(Age, ch) %>% summarize(freq = n())

