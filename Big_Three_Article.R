# Code to produce some tables from the article on Big Three.

# setwd("/home/usdandres/Documents/Study_stuff/Sports Research/Tennis/")

library(stringr)
library(tidyverse)
library(gridExtra)
library(grid)

years <- c(1968:2018)
n.years <- length(years)
data <- list()


## Loading in the data, which can be originally found at https://github.com/JeffSackmann/tennis_atp
## Simply download the atp_matches_[insert year].csv files.

for (i in 1:n.years){
  data[[i]] <- read.csv(paste("tennis_atp-master/atp_matches_",years[i],".csv",sep=""),as.is=T)
  data[[i]]$winner_age <- round(data[[i]]$winner_age)
  data[[i]]$loser_age <- round(data[[i]]$loser_age)
  data[[i]]$year <- sapply(data[[i]]$tourney_id,function(x) substr(as.character(x),1,4))
}



data.full <- NULL
for (i in 1:n.years){
  data.full <- rbind(data.full,data[[i]])
}
dim(data.full)


## Defining the Head-to-Head calculation function, for future use.

head2head <- function(name, dec=F, total=0, top=10, data = data.full){
  Player.won <- data %>%
    select(winner_name, loser_name) %>%
    filter(winner_name == name) %>%
    table() %>%
    data.frame() %>%
    select(-winner_name)
  
  Player.lost <- data %>%
    select(winner_name, loser_name) %>%
    filter(loser_name == name) %>%
    table() %>%
    data.frame() %>%
    select(-loser_name)
  
  H2H <- Player.won %>%
    full_join(Player.lost, by = c("loser_name" = "winner_name")) %>%
    mutate(Freq.x = replace_na(Freq.x, 0), Freq.y = replace_na(Freq.y,0)) %>%
    mutate(Diff = Freq.x - Freq.y, Total = Freq.x + Freq.y) %>%
    filter(Total >= total)
  
  colnames(H2H)[1:3] <- c("Opponent", "Won","Lost")
  
  if (dec == TRUE) H2H.out <- head(arrange(H2H,desc(Diff),desc(Won)),top)
  else H2H.out <- head(arrange(H2H,Diff,desc(Lost)), top)
  
  
  # H2H.out <- ifelse(dec, head(arrange(H2H,desc(Diff),desc(Won)),top), 
  #                        head(arrange(H2H,Diff,Lost), top))
  return(H2H.out)
}



#########
## Roger Federer: King of (Non-) Retirements
#########

player.names <- unique(c(data.full$winner_name,data.full$loser_name))

player.n.matches <- sapply(player.names,function(x) sum(data.full$winner_name == x) + sum(data.full$loser_name == x))
RET.vec <- grepl("RET",data.full$score)
player.n.RETs <- sapply(player.names,function(x) sum( (data.full$loser_name == x) & (RET.vec)))


player.retirements <- data.frame(Name=player.names,
                                 Matches=player.n.matches,
                                 Retirements=player.n.RETs)

player.retirements[order(player.retirements$Matches,decreasing=T),]

long.player.retirements <- subset(player.retirements,Matches>=400)
long.player.retirements[order(long.player.retirements$Retirements),]

zero.retirements <- subset(long.player.retirements,Retirements == 0)
rownames(zero.retirements) <- 1:nrow(zero.retirements)

rownames(player.retirements) <- 1:nrow(player.retirements)
library(grid)
library(gridExtra)
grid.newpage()
grid.table(zero.retirements[order(zero.retirements$Matches,decreasing=T)[1:10],])

head(sort(player.n.RETs,dec=T),20)

grid.newpage()
grid.table(head(player.retirements[order(player.retirements$Retirements,decreasing=T),],15))


#############
### Novak Djokovic: King of Head-to-Heads
#############
dim(data.full)

Fed.win <- data.frame(table(subset(data.full, winner_name == "Roger Federer")[,c("loser_name")]))
Fed.lose <- data.frame(table(subset(data.full, loser_name == "Roger Federer")[,c("winner_name")]))


player <- "Roger Federer"
head2head(player, dec = F, top=10)
#head2head(player, main.stat = "WinPct", dec = F, top=10)
# head2head(player, dec = T, top=10)

player <- "Rafael Nadal"
head2head(player, dec = F, top=10)
head2head(player, dec = T, top=10)

player <- "Novak Djokovic"
head2head(player, dec = F, top=10)
#head2head(player, dec = T, top=10)


player <- "Rafael Nadal"
head2head(player, dec = F, total=0, top=10)
grid.newpage()
hey <- head2head(player, dec = F, total=0, top=10)
#hey[2,3:5] <- c(27,-2,52)
grid.table(hey)
#grid.table(head2head(player, dec = F, total=3, top=10))

player <- "Novak Djokovic"
head2head(player, dec = F, total=0, top=10)
grid.newpage()
grid.table(head2head(player, dec = F, total=0, top=10))


player <- "Roger Federer"
head2head(player, dec = F, total=3, top=10)
grid.newpage()
grid.table(head2head(player, dec = F, total=3, top=10))



## Rafael Nadal: King of Clay (duh)

library(tidyverse)

data.clay <- data.full %>%
  filter(surface == "Clay")

dim(data.full)
dim(data.clay)




data.insert <- data.clay

player.names <- unique(c(data.insert$winner_name,data.insert$loser_name))
player.n.matches <- sapply(player.names,function(x) sum(data.insert$winner_name == x) + sum(data.insert$loser_name == x))
player.n.wins <- sapply(player.names,function(x) sum(data.insert$winner_name == x))
player.n.losses <- sapply(player.names,function(x) sum(data.insert$loser_name == x))

player.record <- data.frame(Name=player.names,
                            Matches=player.n.matches,
                            Wins = player.n.wins,
                            Losses = player.n.losses)

dim(player.record)


library(grid)
library(gridExtra)

grid.newpage()
# grid.table(zero.retirements[order(zero.retirements$Matches,decreasing=T)[1:10],])



player.record %>%
  arrange(desc(Wins)) %>%
  head(5)



grid.table(player.record %>%
             mutate(Diff = Wins - Losses) %>%
             arrange(desc(Diff)) %>%
             head(5))

grid.newpage()

grid.table(player.record %>%
             mutate(WinPct = round(Wins/Matches,3)) %>%
             filter(Matches > 300) %>%
             arrange(desc(WinPct)) %>%
             head(5))



head2head("Rafael Nadal", total=3, top=10, data=data.insert)
head2head("Novak Djokovic", total=3, top=10, data=data.insert)

grid.newpage()
grid.table(head2head("Rafael Nadal", total=3, top=1, data=data.insert))

grid.newpage()
grid.table(head2head("Novak Djokovic", total=3, top=5, data=data.insert))

grid.newpage()
grid.table(head2head("Roger Federer", total=3, top=8, data=data.insert))
