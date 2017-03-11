
# Here are some R functions which we will use to gather some specific data values about a player.
getMaster <- function() {
  M <- read_csv(file = "baseballdatabank-master/core/Master.csv",progress = FALSE )
  return(M)
}

getBatting <- function() {
  B <- read_csv(file = "baseballdatabank-master/core/Batting.csv",progress = FALSE )
  names(B)[names(B) == "3B"] <- "X3B"
  names(B)[names(B) == "2B"] <- "X2B"
  
  # Recoding batting data
  library(car)
  B$SF <- recode(B$SF, "NA = 0")
  B$HBP <- recode(B$HBP, "NA = 0")
  return(B)
}

getFielding <- function() {
  F <- read_csv(file = "baseballdatabank-master/core/Fielding.csv",progress = FALSE )
  return(F)
}

getBatting2k <- function(B) {
  #AB.totals <- ddply(B, .(playerID), summarize, Career.AB = sum(B$AB, na.rm = TRUE))
  AB.totals <-  B %>% group_by(playerID) %>% summarize(Career.AB = sum(AB, na.rm = TRUE))
  B <- as_tibble(merge(B, AB.totals))
  B2K <- subset(B, Career.AB >= 2000)
  return(B2K)
}

find.position <- function(p) {
  positions <- c("OF", "1B",  "2B", "SS", "3B", "C", "P", "DH")
  d <- subset(Fielding, playerID == p)
  count.games <- function(po)
    sum(subset(d, POS == po)$G)
  FLD <- sapply(positions, count.games)
  positions[FLD == max(FLD)][1]
}

get_birthyear <- function(player.id) {
  
  player.record <- subset(Master, playerID == player.id)
  
  birthyear <- player.record$birthYear
  birthmonth <- player.record$birthMonth
  ifelse(birthmonth >= 7, birthyear +1, birthyear)
}

get_stats <- function(player.id) {
  d <- subset(Batting, playerID == player.id)
  byear <- get_birthyear(player.id)
  d$Age <- d$yearID - byear
  d$Avg <- with(d, H / AB)
  d$SLG <- with(d, (H - X2B - X3B - HR + 2 * X2B + 3 * X3B + 4 * HR) / AB)
  d$OBP <- with(d, (H + BB) / (H + AB + BB + SF))
  d$OPS <- with(d,SLG + OBP)
  d$cumH <- with(d,cumsum(H))
  d$cumAB <- with(d,cumsum(AB))
  d$cumAvg <- with(d,cumH/cumAB)
  d
}

get_carreer <- function(d) {
  attach(d,warn.conflicts = FALSE)
  car <- tibble(
    "C.Y" = nrow(d),
    "C.G" =  sum(G, na.rm = TRUE),
    "C.AB" = sum(AB, na.rm = TRUE),
    "C.R"  = sum(R, na.rm = TRUE),
    "C.H"  = sum(H, na.rm = TRUE),
    "C.2B" = sum(X2B, na.rm = TRUE),
    "C.3B" = sum(X3B, na.rm = TRUE),
    "C.HR" = sum(HR, na.rm = TRUE),
    "C.RBI"= sum(RBI, na.rm = TRUE),
    "C.BB" = sum(BB, na.rm = TRUE),
    "C.SO" = sum(SO, na.rm = TRUE),
    "C.SB" = sum(SB, na.rm = TRUE),
    "C.AVG" = C.H / C.AB,
    "C.SLG" = (C.H - C.2B - C.3B - C.HR + 2 * C.2B + 3 * C.3B + 4 * C.HR) / C.AB,
    "C.OBP"  = (C.H + C.BB) / (C.H + C.AB + C.BB + sum(SF, na.rm = TRUE)),
    "C.OPS"  = C.OBP + C.SLG
  )
  detach(d)
  return(car)
}

fit.model <- function(d) {
  fit <- lm(OPS ~I(Age - 30) + I((Age - 30)^2), data = d)
  b <- coef(fit)
  Age.max <- 30 - b[2] / b[3] / 2
  Max <- b[1] - b[2]^2 / b[3] / 4
  list(fit = fit, Age.max = Age.max, Max= Max)
  
}

similar<- function(p, number = 10) {
  P <- subset(C.totals, playerID == p)
  C.totals$SS <- with(C.totals,
                1000 - 
                floor(abs(C.G - P$C.G) / 20) -
                floor(abs(C.AB - P$C.AB) / 75) -
                floor(abs(C.R - P$C.R) / 10) -
                floor(abs(C.H - P$C.H) / 15) -
                floor(abs(C.2B - P$C.2B) / 5) - 
                floor(abs(C.3B - P$C.3B) / 4) -
                floor(abs(C.HR - P$C.HR) / 2) -
                floor(abs(C.RBI - P$C.RBI) / 10) -
                floor(abs(C.BB - P$C.BB) / 25) -
                floor(abs(C.SO - P$C.SO) / 150) - 
                floor(abs(C.SB - P$C.SB) / 20) -
                floor(abs(C.AVG - P$C.AVG) / 0.001) -
                floor(abs(C.SLG - P$C.SLG) / 0.002) -
                abs(Value.POS - P$Value.POS))
  C.totals <- C.totals[order(C.totals$SS, decreasing = TRUE),]
  C.totals[1:number,]
}

collapse.stint <- function(d) {
   G <-  sum(d$G);  AB  <- sum(d$AB);    R <- sum(d$R)
   H <-  sum(d$H);  X2B <- sum(d$X2B); X3B <- sum(d$X3B)
  HR <-  sum(d$HR); RBI <- sum(d$RBI);  SB <- sum(d$SB)
  CS <-  sum(d$CS); BB <-  sum(d$BB);   SH <- sum(d$SB)
  SF <-  sum(d$SF); HBP <- sum(d$HBP)
  SLG <- (H - X2B - X3B - HR + 2 * X2B + 3 * X3B +4 * HR) / AB
  OBP <- (H + BB + HBP) / (AB + BB + HBP + SF)
  OPS <- SLG + OBP
  
  data.frame(G=G, AB=AB, R=R, H=H, X2B=X2B,
             X3B=X3B, HR=HR, RBI=RBI, SB=SB,
             CS=CS, BB=BB, HBP=HBP, SH=SH, SF=SF,
             SLG=SLG, OBP=OBP, OPS=OPS,
             Career.AB=d$Career.AB[1], POS=d$POS[1])
}

fit.trajectory <-  function(d) {
  fit <- lm(OPS ~ I(Age - 30) + I((Age - 30)^2), data = d)
  data.frame(Age = d$Age, Fit = predict(fit, Age = d$Age))
}

plot.trajectories <-  function(first, last, n.similar=5, ncol){
  require(ggplot2)
  
  get.name <-  function(playerid) {
    dl <- subset(Master, playerID == playerid)
    with(dl,paste(nameFirst, nameLast))
  }

  player.id <-   subset(Master, nameFirst == first & nameLast == last)$playerID
  player.id <-   as.character(player.id)
  player.list <- as.character(similar(player.id, n.similar)$playerID)
  Batting.new <- subset(Batting.2000, playerID %in% player.list)
  
  F2 <- Batting.new %>% group_by(playerID) %>% do(fit.trajectory(.))
  F2 <-  merge(F2, 
            data.frame(playerID=player.list, 
            Name = sapply(as.character(player.list), get.name)))
  
  print(ggplot(F2, aes(Age, Fit)) + geom_line(size=1.5) +
          facet_wrap(~ Name, ncol=ncol) + theme_bw())
  
  return(Batting.new)
}

summarize.trajectory <- function(d) {
  f  <- lm(OPS ~ I(Age - 30) + I((Age - 30) ^ 2), data=d)
  b  <- coef(f)
  Age.max <- round(30- b[2] / b[3] /2, 1)
  Max <-  round(b[1] - b[2] ^ 2 / b[3] / 4, 3)
  data.frame(Age.max=Age.max, Max = Max, Curve=round(b[3], 5))
}