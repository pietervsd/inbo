# --- Potential for Conflict Index --- #
# Vaske, J. J., Beaman, J., Barreto, H., & Shelby, L. B. (2010).
# An Extension and Further Validation of the Potential for Conﬂict Index.
# Leisure Sciences, 32(X), 240–254

############################
### >>> qd_pci1
############################

#'Potential conflict index (first variant)
#'
#'questionnaire data analysis: potential conflict index
#'
#' @param x vector with scores of the respondents
#' @param scale_values vector with levels; default: -2:2
#' @param x_is_table if TRUE, x is table with the distribution of the scores
#'
#' @return PCI-score (potential for conflict index)
#' @export
#'
#' @examples{
#'set.seed(201)
#'Xv <- sample(-2:2, size = 100, replace = TRUE) #random responses
#'Yv <- rep(c(-2,2),50) #most extreme difference
#'Zv <- rep(2,100) #minimal difference

#'#qd_pci1
#'qd_pci1(Xv, scale_values = -2:2, x_is_table = FALSE) # 0.4
#'qd_pci1(Yv, scale_values = -2:2, x_is_table = FALSE) # 1
#'qd_pci1(Zv, scale_values = -2:2, x_is_table = FALSE) # 0
#' }
qd_pci1 <- function(x, scale_values = c(-2:2),
                    x_is_table = FALSE){
  
  ### ERROR CONTROL AND PREPARE DATA
  if (scale_values[1] != -scale_values[length(scale_values)])
    stop("index should be symmetric")
  if (x_is_table) {
    if (length(x) != length(scale_values))
      stop("table of x should contain countdata for every scale-value")
  } else {
    x <- table(factor(x, levels = scale_values))
  }
  S <- NULL #To avoid the compilation NOTE
  
  ### PREP DATA
  countdata <- data.frame(N = as.numeric(x),
                          X = abs(scale_values),
                          S = sign(scale_values))
  negatives <- subset(countdata, S == -1)
  positives <- subset(countdata, S == 1)
  neutrals  <- subset(countdata, S == 0)
  
  #CALC DATA
  sum_Xa <- sum(positives$N * positives$X)
  sum_Xu <- sum(negatives$N * negatives$X)
  Xt     <- sum_Xa + sum_Xu
  n      <- sum(positives$N) + sum(negatives$N) + sum(neutrals$N)
  Z      <- n * max(c(min(scale_values), max(scale_values)))
  
  #RETURN RESULT
  (1 - abs((sum_Xa / Xt) - (sum_Xu / Xt))) * Xt/Z
}




###########################
### >>> qd_pci2
###########################


#' Distance matrix for qd_pci2
#'
#'Calculates distance matrix for the function qd_pci2
#' @param x vector with the scores of the respondents
#' @param m m value in the formula (see details)
#' @param p power value in the formula (see details)
#' @details
#' \deqn{Dp_{x,y} = (|r_{x} - r_{y}|) - (m-1))^{p}}
#' \deqn{if sign(r_{x} \neq r_{y}) \\
#' else d_{x,y} = 0}
#' Dp_x,y = (|r_x - r_y| - (m-1))^p
#' @return single value containing pci index
#' @examples{
#' #'set.seed(201)
#'Xv <- sample(-2:2, size = 100, replace = TRUE) #random responses
#'qd_pci2(Xv, scale_values = -2:2, x_is_table = FALSE, m = 1, p = 1) # 0.37
#' }
#' @export


qd_pci2_D <- function(x, m=1, p=1){
  d <- matrix(nrow = length(x), ncol = length(x), data = NA)
  for (i in 1:nrow(d)) {
    for (j in 1:i) {
      if (abs(c(sign(x[i]) - sign(x[j]))) == 2) {
        d[i,j] <- d[j,i] <- (abs(x[i] - x[j]) - (m - 1)) ^ p
      }
      else {
        d[i,j] <- d[j,i] <- 0
      }
    }
  }
  return(d)
}

###----------------

#' Potential conflict index (second variant)
#'
#' Calculates the potential conflict index based on the distance matrix between responses.
#'
#' @param x vector with scores of the respondents
#' @param scale_values vector with levels; default: -2:2
#' @param x_is_table if TRUE, x is table with the distribution of the scores
#' @param m correction; default: m = 1
#' @param p power; default: p = 1
#' @param print flag; if TRUE print results
#'
#' @return PCI-score (potential for conflict index)
#' @export
#'
#' @examples{
#'set.seed(201)
#'Xv <- sample(-2:2, size = 100, replace = TRUE) #random responses
#'Yv <- rep(c(-2,2),50) #most extreme difference
#'Zv <- rep(2,100) #minimal difference
#' #qd_pci2 - using D2 (m=1)
#'qd_pci2(Xv, scale_values = -2:2, x_is_table = FALSE, m = 1, p = 1) # 0.37
#'qd_pci2(Yv, scale_values = -2:2, x_is_table = FALSE, m = 1, p = 1) # 1
#'qd_pci2(Zv, scale_values = -2:2, x_is_table = FALSE, m = 1, p = 1) # 0

#qd_pci2 - using D1 (m=2)
#'qd_pci2(Xv, scale_values = -2:2, x_is_table = FALSE, m = 2, p = 1) # 0.31
#'qd_pci2(Yv, scale_values = -2:2, x_is_table = FALSE, m = 2, p = 1) # 1
#'qd_pci2(Zv, scale_values = -2:2, x_is_table = FALSE, m = 2, p = 1) # 0
#' }
qd_pci2 <- function(x, scale_values = c(-2:2),
                    x_is_table = FALSE, m = 1, p = 1, print = FALSE){
  
  ### ERROR CONTROL AND PREPARE DATA
  
  if (scale_values[1] != -scale_values[length(scale_values)])
    stop("index should be symmetric")
  if (x_is_table) {
    if (length(x) != length(scale_values))
      stop("table of x should contain countdata for every scale-value")
  } else {
    x <- table(factor(x, levels = scale_values))
  }
  
  ### PREP DATA
  
  #Total N
  Ntot <- sum(x)
  
  #call distance function
  d <- qd_pci2_D(scale_values, m = m, p = p)
  
  #matrix with counts
  n <- matrix(nrow = length(x), ncol = length(x), data = rep(x, length(x)))
  
  #Actual Distance
  #n = nk, t(n) = nh
  #d is distance matrix between the scale_value levels
  #d * nk * nh accounts for number of elements in each scale_value level
  #rowsums(d*n*t(n)) calculates the deltax for each level
  #diag(d)*diag(n)^2 actual distance with itself is subtracted
  #sum(...) sums the results for each level
  
  weightedsum <- sum(rowSums(d * n * t(n)) - (diag(d) * diag(n) * diag(n)))
  
  #Maximum Possible Distance
  #dmax = max distance between 2 single elements
  #even N: multiply with Ntot^2 = max distance
  #  if each element is at the extremes
  #odd N: multiply with Ntot^2 - 1
  dmax <- max(d)
  
  delta <- dmax * (Ntot^2 - Ntot %% 2) / 2
  
  #return the normalized sum
  if (print == TRUE) {
    cat("\nqd_pci2 (m =", m, ", p =", p, ",
       levels =", length(scale_values), ")\n")
    cat("------------------------------------\n")
    cat("Total actual distance:", weightedsum, "\n")
    cat("Maximum total distance:", delta, "\n")
    cat("Maximum distance:", dmax, "\n")
    cat("\nqd_pci2:", round(weightedsum / delta, 2),"\n")
  }
  
  return(invisible(weightedsum / delta))
}

#### Simulatie van een gegeven dataset (zoals in excel file Scaled Bubbles)


#' Calculate some base stats on the pci calculation
#'
#' Simulates pci values as a bootstrap based upon an observed sample
#'
#' @param x vector with scores of the respondents
#' @param nsim amount of simulations
#' @param func function to use (as character) "qd_pci1" or "qd_pci2"
#' @param ... extra arguments (like m and p for qd_pci2)
#'
#' @return Vector of statistics
#' @importFrom stats quantile sd median
#' @export
#'
#' @examples{
#' scores <- c(rep(-2,10), rep(-1,10), rep(0,10), rep(1,10), rep(2,10))
#' qd_moments(scores, nsim = 10000, m = 2, p = 1)
#' }
qd_moments <- function(x, nsim = 1000, func = "qd_pci2", ...) {
  pci <- numeric(nsim)
  for (i in 1:nsim) {
    bootsamp <- sample(x, size = length(x), replace = TRUE)
    if (func == "qd_pci1"){
      pci[i] <- qd_pci1(bootsamp, ...)
      if (i == 1) pcireal <- qd_pci1(x)
    } else {
      pci[i] <- qd_pci2(bootsamp, ...)
      if (i == 1) pcireal <- qd_pci2(x, ...)
    }
  }
  quants <- quantile(pci, probs = c(0,0.025,0.25,0.50,0.75,0.975,1))
  data.frame(pci_obs = pcireal, 
             pci_mean = mean(pci),
             pci_min = quants[1],
             pci_025 = quants[2],
             pci_250 = quants[3],
             pci_median = quants[4],
             pci_750 = quants[5],
             pci_975 = quants[6],
             pci_max = quants[7], 
             pci_sd = sd(pci),
             pci_skewness = moments::skewness(pci),
             pci_kurtosis = moments::kurtosis(pci),
             median_pci = median(pci))
}

################################################################

#' Compare 2 pci values
#'
#' Performs a test comparable with the unpaired t-test for 2 groups
#' 
#'I am not convinced this is the right way, but that's how it is in the excel application
#'
#' @param pci1 precalculated pci first group
#' @param pci2 precalculated pci second group
#' @param sd1 precacluated sd for first group on pci
#' @param sd2 precalculated sd for second group on pci
#' @importFrom stats qnorm pnorm
#'
#' @return Vector of statistics
#' @export
#'
#' @examples
#' {
#' qd_pci_2sample_test(pci1 = 0.75, pci2 = 0.50, sd1 = 0.06, sd2 = 0.05)
#' }
qd_pci_2sample_test <- function(pci1=NULL, pci2=NULL, sd1 = NULL, sd2 = NULL) {
  if (is.null(pci1) | is.null(pci2) | is.null(sd1) | is.null(sd2)){
    stop("all pci1, pci2, sd1 and sd2 must be present")
  }
  d <- (pci1 - pci2) / sqrt(sd1^2 + sd2^2) #geen absolute waarde gebruiken zoals in voorbeeld excel, want dat is fout
  pval = ifelse(d < 0, pnorm(d), 1 - pnorm(d))
  rv <- c(statistic = c("d" = d), p.value = pval)
  cat("The normalized distance statistic = ", d, " which corresponds to a 2-sided p-value of ", pval, "\n")
  cat("d is assumed N(0,1) so the one-paired test can be derived from d\n\n")
  rv
}

#######################################################################

bootstrap_compare_pci2 <- function(x, m = 1, p = 1, 
                                   scale_values = -2:2, 
                                   nsamp = 1000) {
  dfExpanded <- 
    x %>%
    group_by(.data$group, .data$score) %>% 
    do({
      aantal <- pull(., .data$count)
      if (aantal > 0)
        data.frame(presence = rep(1, .[["count"]])) 
      else
        data.frame(presence = 0)
    }) %>% 
    filter(.data$presence == 1)
  
  dfPCI <- NULL
  dfCheck <- expand.grid(g1 = unique(dfExpanded$group), 
                         g2 = unique(dfExpanded$group), 
                         smaller = 0, greater = 0) %>% 
    filter(.data$g1 < .data$g2)
  for (i in 1:nsamp){
    dfUsed <- 
      dfExpanded %>%
      slice(sample(1:nrow(dfExpanded), 
                   size = nrow(dfExpanded),
                   replace = TRUE)) %>%
      group_by(.data$group) %>% 
      summarize(pci = qd_pci2(.data$score, 
                              scale_values = scale_values,
                              p = p, 
                              m = m), 
                iter = i)
    for (k in 1:nrow(dfCheck)) {
      g1 <- filter(dfUsed, .data$group == dfCheck[k,"g1"]) %>% pull(.data$pci)
      g2 <- filter(dfUsed, .data$group == dfCheck[k, "g2"]) %>% pull(.data$pci)
      if(g1 <= g2) dfCheck$smaller[k] <- dfCheck$smaller[k] + 1
      if(g1 >= g2) dfCheck$greater[k] <- dfCheck$greater[k] + 1
      
    }
    dfPCI <- bind_rows(dfPCI, dfUsed)
  } 
  dfCheck <- dfCheck %>% mutate(fractieSmaller = .data$smaller / nsamp)
  list(dfPCI, dfCheck)
}

# dfSamples <- expand.grid(score = c(-2,-1,0,1,2), group = 1:4)
# dfSamples$count <- sample(0:50, size = nrow(dfSamples), replace = TRUE)
# dfSamples %>% group_by(group)  %>% summarize(pci = qd_pci2(count, x_is_table = TRUE, scale_values = -2:2))
# 
# bootstrapcalcs <- bootstrap_compare_pci2(dfSamples)
# 
# bootstrapcalcs[[2]]
# 
# 
# bootstrapsummary <- 
#   bootstrapcalcs[[1]] %>% 
#   group_by(group) %>%
#   summarize(mean_pci = mean(pci), 
#             sd_pci = sd(pci),
#             lcl = quantile(pci, 0.025),
#             ucl = quantile(pci, 0.975))
# bootstrapsummary
# 
# 
# ggplot(bootstrapcalcs[[1]], aes(color = factor(group), x = pci)) + geom_density()
# 
# ggplot(bootstrapsummary, 
#        aes(x = factor(group), y = mean_pci, ymin = lcl, ymax = ucl)) + 
#   geom_point() + 
#   geom_errorbar()
