# Create data frame for each team

hirsh_projected <- c(95.7, 92.7, 89.6, 89.7, 82.5, 94.1, 96.1, 94.4, 91.1, 81.8)
hirsh_actual <- c(112.9, 104.2, 66.3, 68.7, 82, 94.8, 109.4, 97.6, 102.8, 117.7)
hirsh <- list(hirsh_actual, hirsh_projected)
hirsh <- as.data.frame(hirsh, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5", 
                                            "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(hirsh) <- c("actual", "projected")

ayaz_projected <- c(104.6, 110, 100.3, 100.9, 89.4, 101.9, 103.3, 102.2, 77.9, 105.8)
ayaz_actual <- c(67, 107.8, 95.3, 106.2, 50, 123.4, 86.6, 88.9, 80.3, 93.3)
ayaz <- list(ayaz_actual, ayaz_projected)
ayaz <- as.data.frame(ayaz, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                          "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(ayaz) <- c("actual", "projected")

harsh_projected <- c(94.1, 102.6, 95.6, 105.9, 97.8, 94.5, 103.1, 105.6, 96.6, 87.7)
harsh_actual <- c(122.3, 111.7, 96.2, 125.7, 104.6, 85.1, 97.1, 81.5, 69.9, 79.5)
harsh <- list(harsh_actual, harsh_projected)
harsh <- as.data.frame(harsh, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                            "Week 6", "Week7", "Week 8", "Week 9", "Week 10"))
colnames(harsh) <- c("actual", "projected")

akshat_projected <- c(96.8, 98.1, 96.1, 99.7, 95.1, 93.2, 94, 85.6, 99.2, 94.3)
akshat_actual <- c(91.4, 74.7, 118.3, 118.5, 69.7, 94.7, 137, 107.7, 104.7, 99.2)
akshat <- list(akshat_actual, akshat_projected)
akshat <- as.data.frame(akshat, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                              "Weeek 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(akshat) <- c("actual", "projected")

charles_projected <- c(87.7, 87.1, 84.3, 86.5, 90.9, 89.1, 79.1, 79.8, 71.2, 81.5)
charles_actual <- c(88.3, 69.6, 76.4, 71.3, 97.9, 70.1, 74.8, 86.3, 66.1, 87.4)
charles <- list(charles_actual, charles_projected)
charles <- as.data.frame(charles, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                                "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(charles) <- c("actual", "projected")

sharan_projected <- c(83.2, 89.4, 83.0, 92.5, 83.4, 86, 88.4, 88.0, 75.9, 98.1)
sharan_actual <- c(78.9, 69.5, 59.5, 91.8, 132.3, 107.6, 83.9, 130.6, 87.7, 77.0)
sharan <- list(sharan_actual, sharan_projected)
sharan <- as.data.frame(sharan, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(sharan) <- c("actual", "projected")

vikram_projected <- c(91.5, 98.8, 91.2, 93.9, 83.2, 90.7, 86.4, 90.0, 79.8, 89.4)
vikram_actual <- c(90.2, 82.5, 75.3, 44.7, 99.7, 111.4, 54.1, 100.7, 78.7, 69.2)
vikram <- list(vikram_actual, vikram_projected)
vikram <- as.data.frame(vikram, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(vikram) <- c("actual", "projected")

faizan_projected <- c(97.7, 98.6, 98.8, 98.6, 85.6, 90.1, 88.4, 84.6, 83.9, 87.6)
faizan_actual <- c(43.3, 108.7, 76.8, 84.6, 96.2, 53.2, 88.8, 63.2, 94.3, 100.6)
faizan <- list(faizan_actual, faizan_projected)
faizan <- as.data.frame(faizan, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                              "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(faizan) <- c("actual", "projected")

nick_projected <- c(94.8, 94.1, 93.7, 93.3, 95.5, 89.7, 93, 97.8, 109.1, 96.9)
nick_actual <- c(88.3, 78.7, 86.9, 141.3, 63.4, 115, 89.6, 111.3, 108.1, 79.5)
nick <- list(nick_actual, nick_projected)
nick <- as.data.frame(nick, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                          "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(nick) <- c("actual", "projected")

varun_projected <- c(95.3, 99.2, 88.3, 92.7, 92.4, 94.8, 90.2, 86.1, 84.3, 92.1)
varun_actual <- c(71.2, 99.2, 83.9, 65.8, 85.0, 93.6, 88.8, 61.1, 75.1, 91)
varun <- list(varun_actual, varun_projected)
varun <- as.data.frame(varun, row.names = c("Week 1", "Week 2", "Week 3", "Week 4", "Week 5",
                                            "Week 6", "Week 7", "Week 8", "Week 9", "Week 10"))
colnames(varun) <- c("actual", "projected")


# Plot 
library(ggplot2)
library(gridExtra)
a <- ggplot(hirsh, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Hirsh")

b <- ggplot(ayaz, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Ayaz")

c <- ggplot(harsh, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Harsh")

d <- ggplot(akshat, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Akshat")

e <- ggplot(charles, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Charles")

f <- ggplot(sharan, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Sharan")

g <- ggplot(vikram, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Vikram")

h <- ggplot(faizan, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Faizan")

i <- ggplot(nick, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Nick")

j <- ggplot(varun, aes(x=projected, y=actual)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE, fullrange = TRUE) +
  xlim(70,110) + ylim(40,145) +
  ggtitle("Varun")

grid.arrange(a,b,c,d,e,f,g,h,i,j, nrow = 2)

# Create plot for total league actual vs projected scores
projected <- c(hirsh_projected, ayaz_projected, harsh_projected, akshat_projected, charles_projected,
               sharan_projected, vikram_projected, faizan_projected, nick_projected, varun_projected)

actual <- c(hirsh_actual, ayaz_actual, harsh_actual, akshat_actual, charles_actual, 
            sharan_actual, vikram_actual, faizan_actual, nick_actual, varun_actual)

team <- rep(c("Hirsh", "Ayaz", "Harsh", "Akshat", "Charles", "Sharan", "Vikram", "Faizan", "Nick", "Varun"), 
            each = 10)

total <- data.frame(projected, actual, team)
total

ggplot(total, aes(x=projected, y=actual)) +
  geom_point(size=4, aes(color=team)) +
  scale_color_brewer("Team", palette = "Paired") +
  geom_smooth(method = "lm", se = FALSE, formula = y~x) +
  geom_abline(slope=1, intercept=0, linetype="dotted", size=1.25) +
  xlab("Projected") + ylab("Actual") +
  ggtitle("Actual vs. Projected Points") + 
  theme(plot.title = element_text(size=18))
  

# Power rankings
# hirsh, ayaz, harsh, akshat, charles, sharan, vik, faiz, nick, vv
week1 <- c(6,3,1,2,8,7,5,10,4,9)
week2 <- c(3,2,1,4,9,10,5,8,6,7)
week3 <- c(5,2,1,3,9,10,8,6,7,4)
week4 <- c(5,3,1,2,6,10,9,8,4,7)
week5 <- c(10,2,1,3,4,9,7,5,6,8)
week6 <- c(7,2,1,3,10,5,9,8,4,6)
week7 <- c(5,3,2,1,8,9,10,7,4,6)
week8 <- c(5,4,3,1,10,6,8,7,2,9)
week9 <- c(5,3,4,1,9,6,10,7,2,8)
week10 <- c(2,4,6,1,7,8,10,5,3,9)
week11 <- c(1,4,5,2,7,8,10,6,3,9)

powerranks <- data.frame(week1, week2, week3, week4, week5, week6, week7, week8, week9, week10,
                         week11)
powerranks$avg <- round((powerranks$week1 + powerranks$week2 + powerranks$week3 + powerranks$week4 +
                         powerranks$week5 + powerranks$week6 + powerranks$week7 + powerranks$week8 +
                           powerranks$week9 + powerranks$week10 + powerranks$week11)/11,1)
powerranks$team <- c("Hirsh", "Ayaz", "Harsh", "Akshat", "Charles", "Sharan", "Vikram", "Faizan", 
                          "Nick", "Varun")

powerranks <- powerranks[c(13,1,2,3,4,5,6,7,8,9,10,11,12)] #change order every week to put most recent week at the end
powerranks <- powerranks[order(powerranks$avg),]
row.names(powerranks) <- 1:nrow(powerranks)
powerranks

library(reshape2)
a <- melt(powerranks, id.vars="team",
          measure.vars = grep("^week",names(powerranks),value=TRUE))
a

# Plot power ranking
library(ggplot2)
ggplot(a, aes(x=variable, y=value,color=team)) +
  geom_point() +
  geom_line(aes(group = team), size=0.75) + 
  scale_y_continuous(breaks = seq(1,10,1),trans = "reverse") +
  geom_text(aes(label=team),data=a[a$variable == "week11",],hjust=-0.18) + #change week
  theme_bw() +
  theme(legend.position = "none") +
  ylab("") + 
  theme(axis.title.x = element_blank()) +
  scale_x_discrete(labels=c("week1"="Week 1", "week2"="Week 2", "week3"="Week 3", "week4"="Week 4",
                            "week5"="Week 5", "week6"="Week 6", "week7"="Week 7", "week8"="Week 8",
                            "week9"="Week 9", "week10"="Week 10", "week11"="Week 11")) #add week
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text = element_text(size=12)) +
  theme(plot.title = element_text(size=18, face="bold",hjust = 0.5)) +
  ggtitle("Weekly Power Rankings")
