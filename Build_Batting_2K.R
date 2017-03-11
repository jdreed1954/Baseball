## ----init, message=FALSE, echo = FALSE-----------------------------------
library(tidyverse)
source("Baseball_Functions.R")
options(dplyr.show_progress = FALSE)

## ----data, echo=FALSE, message=FALSE, warning=FALSE----------------------
Master <- getMaster()
Batting <- getBatting()
Fielding <- getFielding()

## ----B2K-----------------------------------------------------------------
system.time( {
Batting.2000 <- getBatting2k(Batting)
PLAYER <- as.character(unique(Batting.2000$playerID))
POSITIONS <- sapply(PLAYER, find.position)
Fielding.2000 <- data.frame(playerID=names(POSITIONS), POS=POSITIONS)
Batting.2000 <- merge(Batting.2000, Fielding.2000) 
} )

## ----Career--------------------------------------------------------------
system.time( {
C.totals <- Batting.2000 %>% group_by(playerID) %>% 
  summarize( 
              C.G =  sum(G, na.rm = TRUE),
              C.AB = sum(AB, na.rm = TRUE),
              C.R  = sum(R, na.rm = TRUE),
              C.H  = sum(H, na.rm = TRUE),
              C.2B = sum(X2B, na.rm = TRUE),
              C.3B = sum(X3B, na.rm = TRUE),
              C.HR = sum(HR, na.rm = TRUE),
              C.RBI= sum(RBI, na.rm = TRUE),                
              C.BB = sum(BB, na.rm = TRUE),
              C.SO = sum(SO, na.rm = TRUE),
              C.SB = sum(SB, na.rm = TRUE),
              C.AVG = C.H / C.AB,
              C.SLG = (C.H - C.2B - C.3B - C.HR + 2 * C.2B + 3 * C.3B + 4 * C.HR) / C.AB)

C.totals <- merge(C.totals, Fielding.2000)
C.totals$Value.POS <- with(C.totals,
              ifelse(POS == "C", 240,
              ifelse(POS == "SS", 168,
              ifelse(POS == "2B", 132,
              ifelse(POS == "3B", 84,
              ifelse(POS == "OF", 48,
              ifelse(POS == "1B", 12, 0)))))))

} )

## ----PlayerStats, echo = FALSE-------------------------------------------
player.info <- subset(Master, nameFirst == "Mickey" & nameLast == "Mantle")
player.id   <- as.character(player.info$playerID)
PlayerStats <- get_stats(player.id)

## ----ColapseStints-------------------------------------------------------
system.time(
  Batting.2000 <- Batting.2000 %>% group_by(playerID,yearID) %>% do(collapse.stint(.)) 
)

## ----SaveB2K-------------------------------------------------------------

saveRDS(Batting.2000, "Batting.2000.rds")
Batting.2000_read <- readRDS("Batting.2000.rds")
identical(Batting.2000, Batting.2000_read)


## ----Age-----------------------------------------------------------------
player.list <- as.character(unique(Batting.2000$playerID))
birthyears <- sapply(player.list, get_birthyear)
Batting.2000 <- merge(Batting.2000, data.frame(playerID=player.list, Birthyear=birthyears))
Batting.2000$Age <- with(Batting.2000, yearID - Birthyear)

## ----Fitting-------------------------------------------------------------


