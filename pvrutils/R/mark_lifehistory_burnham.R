
#' Omzetting naar Burnham LD (Chapter 9.2 in Mark book)
#'
#' @param data dataset met de nodige gegevens, met tenminste een identificatiekolom, een datumkolom (numeriek), een kolom met de state (bijvoorbeeld leeftijd bij ringen) en een kolom die het event beschrijft (ring aanbrengen, levend gezien, dood gevonden)
#' @param colnames om de kolommen te herkenen in de functie moet je deze in volgorde identificeren, de eerste kolomnaam moet de id bevatten, de tweede een numeriek tijdstip, de derde een event en de overige kolommen vormen de groeperingsvariabelen
#' @param names_event hoe de events in de data genoemd zijn, de eerste komt overeen met een ringevent, de tweede met levend gezien, de derde met dood gezien
#' @param dates_event Voor het Burnham model moeten de observatietijdstippen gedefinieerd zijn. Tussenliggende tijden worden gebruikt voor dead recovery.
#'
#' @return data.frame containing life history, with columns Ch, freq and group
#' @import dplyr
#' @export
#'
#' @examples{
#' ringing <- data.frame(id = 1:20, date = c(rep(1,10), rep(2,10)), 
#'                       group = rep(rep(0:1, 5), 2), event = "ringing")
#' period2 <- data.frame(id = c(1,2,3,4,5), date = 2, 
#'                       group = c(1,1,1,1,1), event = "liverecovery")
#' period2b <- data.frame(id = c(4, 20), date = 2.5, 
#'                        group = c(2,1), event = "deadrecovery")
#' period3 <- data.frame(id = c(1,3,5,12,15), date = 3, 
#'                       group = c(2,2,2,1,1), event = "liverecovery")
#' testdata <- rbind(ringing, period2, period2b, period3)
#' lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
#' print(lddata)
#' lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
#' print(lddata)
#' }
mark_lifehistory_burnham <- function(data, colnames = c("id", "date", "event", "group"),
                            names_event = c("ringing", "liverecovery", "deadrecovery"),
                            dates_event) {
  ndates <- length(dates_event)
  wdata <- data %>%
    transmute(id = .data[[colnames[1]]],
              date = .data[[colnames[2]]],
              event = .data[[colnames[3]]],
              group = interaction(.data[[colnames[4:length(colnames)]]])) %>%
    mutate(event = ifelse(.data$event == names_event[1], "R",
                          ifelse(.data$event == names_event[2], "L",
                                 ifelse(.data$event ==  names_event[3], "D", NA)))) %>%
    group_by(.data$id, .data$date, .data$event, .data$group) %>%
    do({
      udate <- unique(.$date)
      LDdate <- max(dates_event[udate >= dates_event])
      LDnumdate = (1:length(dates_event))[dates_event == LDdate]

      tibble(LDdate, LDnumdate)
    })
  Lcolnrs <- 6 + seq(1, 2 * ndates, by = 2)
  Dcolnrs <- 6 + seq(2, 2 * ndates, by = 2)

  rv <- filter(wdata, .data$event == "R")
  rv <- bind_cols(rv, as.data.frame(matrix(nrow = nrow(rv), ncol = 2 * ndates, data = 0)))
  for (i in 1:nrow(wdata)) {
    column <- ifelse(wdata$event[i] %in% c("R", "L"),
                     Lcolnrs[wdata$LDnumdate[i]],
                     ifelse(wdata$event[i] %in% c("D"),
                            Dcolnrs[wdata$LDnumdate[i]],
                            NA))
    rv[which(rv$id == wdata$id[i]), column] <- 1
  }
  rv$ch <- apply(rv[ , -(1:6), drop = FALSE], 1, paste, collapse = "")
  rv <- rv %>% 
    select(.data$ch, .data$group) %>%
    group_by(.data$ch, .data$group) %>% 
    summarize(freq = n()) %>% 
    filter(!is.na(.data$freq) & .data$freq != 0) %>%
    ungroup() %>%
    transmute(ch = as.character(.data$ch), freq = .data$freq, group = as.factor(.data$group))
  as.data.frame(rv)
}
# 
# ringing <- data.frame(id = 1:20, date = c(rep(1,10), rep(2,10)), group = rep(rep(0:1, 5), 2), event = "ringing")
# period2 <- data.frame(id = c(1,2,3,4,5), date = 2, group = c(1,1,1,1,1), event = "liverecovery")
# period2b <- data.frame(id = c(4, 20), date = 2.5, group = c(2,1) ,event = "deadrecovery")
# period3 <- data.frame(id = c(1,3,5,12,15), date = 3, group = c(2,2,2,1,1), event = "liverecovery")
# testdata <- rbind(ringing, period2, period2b, period3)
# lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
# print(lddata)
# lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
# print(lddata)


