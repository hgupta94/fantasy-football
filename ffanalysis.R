ffrankings <- read.csv('cool league.csv')

# Delete unnecessary columns
ffrankings$playerId <- NULL
ffrankings$age <- NULL
ffrankings$exp <- NULL
ffrankings$bye <- NULL
ffrankings$sleeper <- NULL
ffrankings$salary <- NULL
ffrankings$overallECR <- NULL
ffrankings$team <- NULL
head(ffrankings)
str(ffrankings)
summary(ffrankings)

ffrankings$tier=as.factor(ffrankings$tier)
ffrankings <- ffrankings[order(ffrankings$points,decreasing = TRUE),]
row.names(ffrankings) <- 1:nrow(ffrankings)

# Filter by position
qb <- ffrankings[ffrankings$position == "QB",]
rb <- ffrankings[ffrankings$position == "RB",]
wr <- ffrankings[ffrankings$position == "WR",]
te <- ffrankings[ffrankings$position == "TE",]
dst <- ffrankings[ffrankings$position == "DST",]
k <- ffrankings[ffrankings$position == "K",]
head(rb)

# Renumber rows
row.names(qb) <- 1:nrow(qb)
row.names(rb) <- 1:nrow(rb)
row.names(wr) <- 1:nrow(wr)
row.names(te) <- 1:nrow(te)
row.names(dst) <- 1:nrow(dst)
row.names(k) <- 1:nrow(k)
head(wr)

# Filter for relevant players from each position
qb <- subset(qb,qb$positionRank<=24)
rb <- subset(rb,rb$positionRank<=49)
wr <- subset(wr,wr$positionRank<=49)
te <- subset(te,te$positionRank<=24)
k <- subset(k,k$positionRank<=32)
tail(qb)

# Combine all players
players <- bind_rows(qb,rb,wr,te,dst,k)
head(players)

# Sort players by points
allplayers <- players[order(players$points, decreasing = TRUE),]
row.names(allplayers) <- 1:nrow(allplayers)
head(allplayers)

# Plots
ggplot(data=allplayers, aes(x=points)) +
  geom_histogram(binwidth=1, aes(fill=position), color="black") +
  facet_grid(position~.,)

ggplot(allplayers,aes(x=allplayers$points,fill=allplayers$position)) +
  geom_density(alpha=0.2)

ggplot(qb,aes(x=points,y=qb$positionRank,color=qb$tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.3) +
  geom_point(size=5,color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-0.1,vjust=0.4) +
  scale_y_reverse(lim=c(25,0)) + xlim(5,30) +
  xlab("Points") + ylab("Position Rank") + ggtitle("Quarterbacks") +
  theme_bw()+ theme(legend.position = "none") + 
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))

ggplot(rb,aes(x=points,y=positionRank,color=tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.55) +
  geom_point(size=ifelse(rb$points>=9.49,5,3), color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-0.1,vjust=0.4) +
  scale_y_reverse(lim=c(50,0)) + xlim(0,25) +
  xlab("Points") + ylab("Position Rank") + 
  theme_bw() + theme(legend.position = "none") + ggtitle("Running Backs") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))

ggplot(wr,aes(x=points,y=positionRank,color=tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.55) +
  geom_point(size=ifelse(wr$points>=9.5,5,3), color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-.1,vjust=0.4) +
  scale_y_reverse(lim=c(50,0)) + xlim(0,20) +
  xlab("Points") + ylab("Position Rank") +
  theme_bw() + theme(legend.position = "none") + ggtitle("Wide Receivers") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))

ggplot(te,aes(x=points,y=positionRank,color=tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.3) +
  geom_point(size=ifelse(te$points>=9.5,5.5,3), color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-0.1,vjust=0.4) +
  scale_y_reverse(lim=c(25,0)) + xlim(0,20) +
  xlab("Points") + ylab("Position Rank") +
  theme_bw() + theme(legend.position = "none")+ ggtitle("Tight Ends") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))

ggplot(dst,aes(x=points,y=positionRank,color=tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.3) +
  geom_point(size=ifelse(dst$points>10,5,3), color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-0.15,vjust=0.4) +
  scale_y_reverse(lim=c(35,0)) + xlim(0,15) +
  xlab("Points") + ylab("Position Rank") +
  theme_bw() + theme(legend.position = "none") + ggtitle("Defense/Special Teams") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))

ggplot(k,aes(x=points,y=positionRank,color=tier)) +
  geom_errorbarh(aes(xmin=lower,xmax=upper),height=.3) +
  geom_point(size=ifelse(k$points>10,5,3), color="white") +
  geom_text(aes(x=points,label=round(points,0))) +
  geom_text(aes(x=upper,label=player),hjust=-0.1,vjust=0.4) +
  scale_y_reverse(lim=c(35,0)) + xlim(-0,20) +
  xlab("Points") + ylab("Position Rank") +
  theme_bw() + theme(legend.position = "none") + ggtitle("Kickers") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5)) +
  theme(axis.title = element_text(size=12)) +
  theme(axis.text.x = element_text(size=12)) + theme(axis.text.y = element_text(size=12))


# Scrape ESPN rankings to compare
library(rvest)
espn <- read_html("http://www.espn.com/fantasy/football/story/_/page/17RanksPreseason200nonPPR/2017-fantasy-football-standard-rankings-non-ppr-top-200")

rank <- espn %>%
  html_nodes("table.inline-table") %>%
  .[[2]] %>%
  html_table()
 rank

# Turn into data frame & delete unecessary columns
espnrank <- data.frame(rank)
espnrank$PosRank <- NULL
espnrank$Bye <- NULL
str(espnrank)

# Clean up data frame
espnrank <- separate(espnrank,Player.Position.Team, into=c("Player","Rank"),sep=",",extra="merge")
espnrank <- separate(espnrank,Rank,into=c("Position","Team"),sep=",",extra="merge")
espnrank$Team <- NULL
espnrank <- separate(espnrank,Player,into=c("Rank","Player"),sep=". ",extra="merge")
espnrank$Rank <- NULL
head(espnrank)

# Delete analyst ranks
espnrank$MB = NULL
espnrank$MC = NULL
espnrank$TC = NULL
espnrank$EK = NULL
espnrank$FY = NULL
tail(espnrank)

# Make position a factor
espnrank$Position = as.factor((espnrank$Position))
str(espnrank)

# Fix spelling/formatting to match original dataset
espnrank
colnames(espnrank)[1] <- "player"
espnrank[2,1] = "LeVeon Bell"
espnrank[4,1] = "Odell Beckham"
espnrank[27,1] ="Terrelle Pryor"
espnrank[94,1] = "Duke Johnson"
espnrank[118,1] = "Ted Ginn"
espnrank[123,1] = "Marvin Jones"
espnrank[162,1] = "DOnta Foreman"
espnrank[195,1] = "Will Fuller"
espnrank[198,1] = "DeAngelo Henderson"
espnrank$player <- gsub('/','',espnrank$player)
espnrank$Position <- gsub('/','',espnrank$Position)
espnrank$player <- gsub(' DST','',espnrank$player)
row.names(espnrank) <- 1:nrow(espnrank)
head(espnrank)

espnrank$Position = as.factor((espnrank$Position))
espnrank$player = as.factor(espnrank$player)

# Merge original dataset with ESPNs
merged <- merge(allplayers,espnrank,by="player",all=TRUE)
merged
colnames(merged)[28] <- "espnRank"
merged$Position <- NULL
merged$team <- NULL
str(merged)
merged$player = as.character(merged$player)

# Rename positions
qball <- merged[merged$position == "QB",]
rball <- merged[merged$position == "RB",]
wrall <- merged[merged$position == "WR",]
teall <- merged[merged$position == "TE",]
dstall <- merged[merged$position == "DST",]
kall <- merged[merged$position == "K",]

# Order players by ESPN rank
qball <- qball[order(qball$espnRank),]
rball <- rball[order(rball$espnRank),]
wrall <- wrall[order(wrall$espnRank),]
teall <- teall[order(teall$espnRank),]
dstall <- dstall[order(dstall$espnRank),]
kall <- kall[order(kall$espnRank),]

row.names(qball) <- 1:nrow(qball)
row.names(rball) <- 1:nrow(rball)
row.names(wrall) <- 1:nrow(wrall)
row.names(teall) <- 1:nrow(teall)
row.names(dstall) <- 1:nrow(dstall)
row.names(kall) <- 1:nrow(kall)

# Remove NA rows
# Check before running
qball <- qball[-c(24:26,28,29,31,33,34,36,37,40,41),]
rball <- rball[-c(65:71,73,75,77,80,81),]
wrall <- wrall[-c(67:74,76,78,80,81),]
teall <- teall[-c(22,23,25,26,28,30,33,34,36,37,39,40),]
dstall <- dstall[-c(20,22,23,26,28,29,32,37,41:43),]
kall <- kall[-c(19:21,25,27,30,33,37,41:43),]

row.names(qball) <- 1:nrow(qball)
row.names(rball) <- 1:nrow(rball)
row.names(wrall) <- 1:nrow(wrall)
row.names(teall) <- 1:nrow(teall)
row.names(dstall) <- 1:nrow(dstall)
row.names(kall) <- 1:nrow(kall)

# Create position rank column
qball$espn.position.rank <- seq.int(nrow(qball))
rball$espn.position.rank <- seq.int(nrow(rball))
wrall$espn.position.rank <- seq.int(nrow(wrall))
teall$espn.position.rank <- seq.int(nrow(teall))
dstall$espn.position.rank <- seq.int(nrow(dstall))
kall$espn.position.rank <- seq.int(nrow(kall))

# Remove sleeper column
qball$sleeper <- NULL
rball$sleeper <- NULL
wrall$sleeper <- NULL
teall$sleeper <- NULL
dstall$sleeper <- NULL
kall$sleeper <- NULL

# Change all NA ranks to 190, after last ranked player
qball$espnRank[is.na(qball$espnRank)] <- 190
rball$espnRank[is.na(rball$espnRank)] <- 190
wrall$espnRank[is.na(wrall$espnRank)] <- 190
teall$espnRank[is.na(teall$espnRank)] <- 190
dstall$espnRank[is.na(dstall$espnRank)] <- 190
kall$espnRank[is.na(kall$espnRank)] <- 190

# Plot Differences

ggplot(qball, aes(qball$positionRank,qball$espn.position.rank,color=qball$tier)) +
  geom_point(size=3) +
  geom_abline(slope=1,intercept=0) +
  annotate("segment", x=5,xend=5,y=20,yend=23, arrow=arrow()) +
  annotate("text",x=5,y=19.5,label="ESPN Underrates") +
  annotate("segment", x=25,xend=25,y=6.5,yend=3.5, arrow=arrow()) +
  annotate("text",x=25,y=7,label="ESPN Overrates") +
  geom_text(aes(label=ifelse(qball$espn.position.rank-qball$positionRank>0,qball$player,""),hjust=1.07)) +
  geom_text(aes(label=ifelse(qball$positionRank-qball$espn.position.rank>0,qball$player,""),hjust=-0.08)) +
  scale_x_continuous(lim=c(-3,35)) + scale_y_continuous(lim=c(0,30)) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("Average Position Rank") + ylab("ESPN Position Rank") + ggtitle("Quarterbacks") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))
  

ggplot(rball,aes(rball$positionRank,rball$espn.position.rank,color=tier)) +
  geom_point(size=2) +
  geom_abline(slope=1,intercept=0) +
  annotate("segment", x=15,xend=15,y=60,yend=67, arrow=arrow()) +
  annotate("text",x=15,y=59.25,label="ESPN Underrates") +
  annotate("segment", x=65,xend=65,y=15,yend=8, arrow=arrow()) +
  annotate("text",x=65,y=16.25,label="ESPN Overrates") +
  geom_text_repel(aes(label=ifelse(rball$espn.position.rank-rball$positionRank>5,rball$player,"")),nudge_x = -2,nudge_y = 1) +
  geom_text_repel(aes(label=ifelse(rball$positionRank-rball$espn.position.rank>5,rball$player,"")),nudge_x = 4,nudge_y = -1) +
  scale_x_continuous(lim=c(0,80)) + scale_y_continuous(lim=c(0,80)) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("Average Position Rank") + ylab("ESPN Position Rank") + ggtitle("Running Backs") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))

ggplot(wrall,aes(wrall$positionRank,wrall$espn.position.rank,color=tier)) +
  geom_point(size=2) +
  xlim(0,80) + ylim(0,80) +
  geom_abline(slope=1,intercept=0) +
  annotate("segment", x=10,xend=10,y=60,yend=67, arrow=arrow()) +
  annotate("text",x=10,y=59.25,label="ESPN Underrates") +
  annotate("segment", x=68.75,xend=68.75,y=20,yend=13, arrow=arrow()) +
  annotate("text",x=68.85,y=22,label="ESPN Overrates") +
  geom_text_repel(aes(label=ifelse(wrall$espn.position.rank-wrall$positionRank>5,wrall$player,"")),nudge_x = -2) +
  geom_text_repel(aes(label=ifelse(wrall$positionRank-wrall$espn.position.rank>5,wrall$player,"")),nudge_x = 2) +
  theme_minimal() + theme(legend.position = "none") +
  xlab("Average Position Rank") + ylab("ESPN Position Rank") + ggtitle("Wide Receivers") +
  theme(plot.title = element_text(face="bold",size=15,hjust = 0.5))

ggplot(teall, aes(teall$positionRank,teall$espn.position.rank,color=tier)) +
  geom_point(size=2) +
  xlim(0,30) + ylim(0,30) +
  geom_abline(slope=1,intercept=0) +
  annotate("segment", x=5,xend=5,y=19,yend=22, arrow=arrow()) +
  annotate("text",x=5,y=18.75,label="ESPN Underrates") +
  annotate("segment", x=22.5,xend=22.5,y=7,yend=4, arrow=arrow()) +
  annotate("text",x=22.5,y=7.5,label="ESPN Overrates") +
  geom_text_repel(aes(label=ifelse(teall$espn.position.rank-teall$positionRank>0,teall$player,"")),nudge_x = -1) +
  geom_text_repel(aes(label=ifelse(teall$positionRank-teall$espn.position.rank>0,teall$player,"")),nudge_x = 1) +
  theme_minimal()+ theme(legend.position = "none") +
  ggtitle("Tight Ends") + theme(plot.title = element_text(face = "bold",size = 15,hjust = 0.5)) + 
  xlab("Average Position Rank") + ylab("ESPN Position Rank")
