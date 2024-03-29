% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mark_lifehistory_burnham.R
\name{mark_lifehistory_burnham}
\alias{mark_lifehistory_burnham}
\title{Omzetting naar Burnham LD (Chapter 9.2 in Mark book)}
\usage{
mark_lifehistory_burnham(
  data,
  colnames = c("id", "date", "event", "group"),
  names_event = c("ringing", "liverecovery", "deadrecovery"),
  dates_event
)
}
\arguments{
\item{data}{dataset met de nodige gegevens, met tenminste een identificatiekolom, een datumkolom (numeriek), een kolom met de state (bijvoorbeeld leeftijd bij ringen) en een kolom die het event beschrijft (ring aanbrengen, levend gezien, dood gevonden)}

\item{colnames}{om de kolommen te herkenen in de functie moet je deze in volgorde identificeren, de eerste kolomnaam moet de id bevatten, de tweede een numeriek tijdstip, de derde een event en de overige kolommen vormen de groeperingsvariabelen}

\item{names_event}{hoe de events in de data genoemd zijn, de eerste komt overeen met een ringevent, de tweede met levend gezien, de derde met dood gezien}

\item{dates_event}{Voor het Burnham model moeten de observatietijdstippen gedefinieerd zijn. Tussenliggende tijden worden gebruikt voor dead recovery.}
}
\value{
data.frame containing life history, with columns Ch, freq and group
}
\description{
Omzetting naar Burnham LD (Chapter 9.2 in Mark book)
}
\examples{
{
ringing <- data.frame(id = 1:20, date = c(rep(1,10), rep(2,10)), 
                      group = rep(rep(0:1, 5), 2), event = "ringing")
period2 <- data.frame(id = c(1,2,3,4,5), date = 2, 
                      group = c(1,1,1,1,1), event = "liverecovery")
period2b <- data.frame(id = c(4, 20), date = 2.5, 
                       group = c(2,1), event = "deadrecovery")
period3 <- data.frame(id = c(1,3,5,12,15), date = 3, 
                      group = c(2,2,2,1,1), event = "liverecovery")
testdata <- rbind(ringing, period2, period2b, period3)
lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
print(lddata)
lddata <- mark_lifehistory_burnham(testdata, dates_event = 1:3)
print(lddata)
}
}
