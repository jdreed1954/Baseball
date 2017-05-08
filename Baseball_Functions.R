
Root <- "C:/Users/jdreed/Desktop/MyProjects/Baseball/baseballdatabank-2017.1/core/"
# Here are some R functions which we will use to gather some specific data values about a player.
getMaster <- function() {
  MasterFile <- paste(Root, "Master.csv", sep ="")
  M <- read_csv(file = MasterFile, progress = FALSE )
  return(M)
}

getPlayerID <- function(First, Last) {
   player.id <- subset(Master, nameLast == Last & nameFirst == First)$playerID
   return(player.id)
}
getBatting <- function() {
  BattingFile <-  paste(Root, "Batting.csv", sep = "")
  B <- read_csv(file = BattingFile ,progress = FALSE )
  names(B)[names(B) == "3B"] <- "X3B"
  names(B)[names(B) == "2B"] <- "X2B"
  
  # Recoding batting data
  library(car)
  B$SF <- recode(B$SF, "NA = 0")
  B$HBP <- recode(B$HBP, "NA = 0")
  return(B)
}

getFielding <- function() {
  FieldingFile <- paste(Root,"Fielding.csv", sep="")
  F <- read_csv(file = FieldingFile, progress = FALSE )
  return(F)
}

getBatting2k <- function() {
  B <- getBatting()
  AB.totals <-  B %>% group_by(playerID) %>% summarize(Career.AB = sum(AB, na.rm = TRUE))
  B <- as_tibble(merge(B, AB.totals))
  B2K <- subset(B, Career.AB >= 2000)
  
  player.list <- as.character(unique(B2K$playerID))
  birthyears <-  sapply(player.list, get.birthyear)
  B2K <-  merge(B2K, data.frame(playerID=player.list, Birthyear=birthyears))
  B2K$Age <- with(B2K, yearID - Birthyear)
  B2K <-  B2K[complete.cases(B2K$Age),]
  
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

get.birthyear <- function(player.id) {
  
  player.record <- subset(Master, playerID == player.id)
  
  birthyear <- player.record$birthYear
  birthmonth <- player.record$birthMonth
  ifelse(birthmonth >= 7, birthyear +1, birthyear)
}

get_stats <- function(player.id) {
  d <- subset(Batting, playerID == player.id)
  byear <- get.birthyear(player.id)
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

CalcCareer2k <- function() {
  C.totals <- Batting.2000 %>% summarize(
    C.G =  sum(G, na.rm = TRUE),
    C.AB = sum(AB, na.rm = TRUE),
    C.R  = sum(R, na.rm = TRUE),
    C.H  = sum(H, na.rm = TRUE),
    C.2B = sum(X2B, na.rm = TRUE),
    C.3B = sum(X3B, na.rm = TRUE),
    C.HR = sum(HR, na.rm = TRUE),
    C.RBI = sum(RBI, na.rm = TRUE),
    C.BB = sum(BB, na.rm = TRUE),
    C.SO = sum(SO, na.rm = TRUE),
    C.SB = sum(SB, na.rm = TRUE),
    C.AVG = C.H / C.AB,
    C.SLG = (C.H - C.2B - C.3B - C.HR + 2 * C.2B + 3 * C.3B + 4 * C.HR) / C.AB,
    C.OBP  = (C.H + C.BB) / (C.H + C.AB + C.BB + sum(SF, na.rm = TRUE)),
    C.OPS  = C.OBP + C.SLG
  )
  
  C.totals <- merge(C.totals,Fielding.2000)
  C.totals$Value.POS <- with(C.totals,
                             ifelse(POS == "C", 240,
                             ifelse(POS == "SS", 168,
                             ifelse(POS == "2B", 132,
                             ifelse(POS == "3B", 84,
                             ifelse(POS == "OF", 48,
                             ifelse(POS == "1B", 12, 0)))))))
  return(C.totals)
}

get_career <- function(d) {
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

get_career_long <- function(d) {
  attach(d,warn.conflicts = FALSE)
  
  birthyear <- get.birthyear(d$playerID[1])
  car <- tibble(
    "Grp" = subset(players,player.id == d$playerID[1])$Group,
    "Last" = subset(Master,playerID == d$playerID[1])$nameLast,
    "First" = subset(Master,playerID == d$playerID[1])$nameFirst,
    "Age"   = 2017 - birthyear,
    "Y" = nrow(d),
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

fit.traj <-  function(d) {
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
  Batting.2000 <- getBatting2k()
  #>>>>>>> Temporary
  PLAYER <-  as.character(unique(Batting.2000$playerID))
  POSITIONS <-  sapply(PLAYER,find.position)  # long running
  Fielding.2000 <- tibble(playerID=names(POSITIONS), POS = POSITIONS)
  Batting.2000 <-  as_tibble(merge(Batting.2000, Fielding.2000))
  
  # >>>>>>>>> End Temporary
  
  
  # Add Age to Batting.2000 data table.
  player.list <-  as.character(unique(Batting.2000$playerID))
  birthyears <- sapply(player.list,get.birthyear)
  Batting.2000 <-  merge(Batting.2000, data.frame(playerID=player.list, Birthyear=birthyears))
  Batting.2000$Age <-  with(Batting.2000, yearID - Birthyear)
  
  Batting.new <- subset(Batting.2000, playerID %in% player.list) %>% 
    group_by(playerID) %>% 
    do(collapse.stint(.))
  
  F2 <- Batting.new %>% group_by(playerID) %>% do(fit.traj(.))
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

getRockiesGBG <- function(Yr) {
  # Function retrieves the Game-by-Game from baseball-reference.com and re-formats
  # so it is suitable for display.  Note, Classes variable below helps cast the various
  # fields into a data type suitable to its purposes.
  #
  Classes <- c(
    "integer",
    rep("character", 6),
    rep("integer", 3),
    "character",
    rep("integer", 2),
    rep("character", 5),
    "integer",
    "character"
  )
  
  URL <- paste("http://www.baseball-reference.com/teams/COL/", Yr, 
               "-schedule-scores.shtml#team_schedule::none", sep="")
  
  gbgurl <- getURL(URL)
  
  tables <-
    readHTMLTable(gbgurl, stringsAsFactors = FALSE, colClasses = Classes)
  
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  
  n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
  colnames(tables$team_schedule)[3] <- "X3"
  colnames(tables$team_schedule)[5] <- "H/A"
  
  G <-  tables$team_schedule %>% filter(Date != "Date")
  
  G <-  as_tibble(G) %>% select(-c(3, 4, 10:13, 17, 19)) %>% 
    separate(Date, into = c("Day", "Date"), sep = ",")

  G$Day <- strtrim(G$Day, 3)
  
 # G <- mutate(G, `H/A` = ifelse(G$`H/A` == "@", "A", "H")) %>% 
 #  filter(!is.na(Win))
  BLANK <- " "
  G <- mutate(G, `H/A` = ifelse(G$`H/A` == "@", "A", "H"))
  n.col <- ncol(G)
  for ( i in 1:nrow(G)) {
    if (is.na(G$Win[i])) {
      G[i,6] <- BLANK
      G[i,7:8] <- 0
      G[i,9:n.col] <- BLANK
    }
  }
  
  return(G)
}

getSalaries <- function(Yr) {
  # Note, this website is only good back to the 2000 season.
  FieldClasses <- c("integer","character", "numeric", rep("character",6))
  
  
  url <- paste("http://www.spotrac.com/mlb/payroll/", Yr, "/", sep = "")
  Sal <- getURL(url)
  
  tables <- readHTMLTable(Sal, colClasses = FieldClasses,
                          stringsAsFactors = FALSE)
  tables <- list.clean(tables, fun = is.null, recursive = FALSE)
  
  s <- as_tibble(tables$`NULL`)
  s <- filter(s,Rank != is.na(Rank))
  s <- rename(s, X25.Man = `25-Man`, Disabled.List = `Disabled List`, 
              Total.Payroll = `Total Payroll`)
  #
  s$X25.Man <- with(s, X25.Man <-  sub("\\$", "", X25.Man)) %>% 
    gsub(",","",.) %>% sub("0-", "0", .)
  s$Disabled.List <- with(s, Disabled.List <-  sub("\\$", "", Disabled.List)) %>%    
    gsub(",","",.) %>% sub("0-", "0", .)
  s$Retained <- with(s, Retained <-  sub("\\$", "", Retained)) %>%
    gsub(",","",.) %>% sub("0-", "0", .)
  s$Buried <- with(s, Buried <-  sub("\\$", "", Buried)) %>%
    gsub(",","",.) %>% sub("0-", "0", .)
  s$Suspended <- with(s, Suspended <-  sub("\\$", "", Suspended)) %>%
    gsub(",","",.) %>% sub("0-", "0", .)
  s$Total.Payroll <- with(s, Total.Payroll <-  sub("\\$", "", Total.Payroll)) %>% 
    gsub(",","",.) %>% sub("0-", "0", .)
  # Now we convert character columns corresponding to numbers.
  s <- as_tibble(type_convert(s)) 
  return(s)
  
}

getCurrentTeams <- function() {
  c.Teams <- read_csv("../Baseball/Teams_Current.csv", trim_ws = TRUE)
  return(c.Teams)
}

getCurrentResults <- function(){
  GBG17 <- getRockiesGBG(2017) 
  GBG17 <- filter(GBG17, Win != " ")  
  C.Results <- tibble(
    Wins =  length(grep("W",GBG17$`W/L`)),
    Losses =  length(grep("L",GBG17$`W/L`)),
    Win.Pct = Wins/(Wins + Losses) * 100,
    Runs = sum(as.integer(GBG17$R)),
    Runs.Ag = sum(as.integer(GBG17$RA)),
    `Predicted Season Wins` = round(Runs^2.00/(Runs^2.00+Runs.Ag^2.00)*162))
  return(C.Results)
}
