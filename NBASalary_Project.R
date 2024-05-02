rm(list=ls())

stats <-  read.csv("NBAStats_Salary.csv")

#install.packages("ggplot2")
#library(ggplot2)

#Rename Columns
names(stats) <- c("Player", "Salary", "Position", "Age", "Team",
                  "GP", "GS", "MP", "FGM", "FGA", "FG%", "3PM",
                  "3PA", "3P%", "2PM", "2PA", "2P%", "eFG%", "FTM",
                  "FTA", "FT%", "ORB", "DRB", "TRB", "AST", "STL", 
                  "BLK", "TOV", "PF", "PTS", "Total_Min", "PER",
                  "TS%", "3PAr", "Ftr", "ORB%", "DRB%", "TRB%", 
                  "AST%", "STL%", "BLK%", "TOV%", "USG%", "OWS", 
                  "DWS", "WS", "WS/48", "OBPM", "DBPM", "BPM", "VORP")

#Update column types for feature engineering
stats$Total_Min <- as.numeric(stats$Total_Min)

#Feature Engineering
stats$MinPerDollar <- stats$Total_Min / stats$Salary
stats$PtPerDollar <- stats$PTS / stats$Salary
stats$PRA <- stats$PTS + stats$TRB + stats$AST
stats$PRAPerDollar <- stats$PRA/ stats$Salary
stats$PERperDollar <- stats$PER / stats$Salary
stats$GPPerDollar <- stats$GP / stats$Salary

#Section off top 150 players by Salary, top 50, top 25
stats <- stats[order(-stats$Salary), ] #Sort descending by salary
top300salary <- head(stats, 300)
top150salary <- head(stats, 150)
top50salary <- head(stats, 50) 
top25salary <- head(stats, 25) 

#Summary Tables with Top 10s - Using top150 df due to players having low salary or minutes
top10_minperdol <- top150salary[order(-top150salary$MinPerDollar), c("Player", "Salary", "MinPerDollar")][1:10, ]
top10_minperdol

top10_ptperdol <- top150salary[order(-top150salary$PtPerDollar), c("Player", "Salary", "PtPerDollar")][1:10, ]
top10_ptperdol

top10_PRAperdol <- top150salary[order(-top150salary$PRAPerDollar), c("Player", "Salary", "PRAPerDollar")][1:10, ]
top10_PRAperdol

top10_PERperdol <- top150salary[order(-top150salary$PERperDollar), c("Player", "Salary", "PERperDollar")][1:10, ]
top10_PERperdol

top10_GPperdol <- top150salary[order(-top150salary$GPPerDollar), c("Player", "Salary", "GPPerDollar")][1:10, ]
top10_GPperdol

#Show overall density of salaries
ggplot(top300salary, aes(x = Salary)) +
  geom_density(fill = "skyblue", alpha = 0.7) +
  labs(title = "Distribution of Player Salaries", x = "Salary (in 10s of millions)", y = "Density") +
  scale_x_continuous(labels = scales::comma_format(scale = 1e-7, accuracy = 0.1))

#Show top 10 players by minperdol
top10_minperdol <- top150salary[order(-top150salary$MinPerDollar), c("Player", "Salary", "MinPerDollar")][1:10, ]
top10_minperdol$Rank <- 1:10
top10_minperdol <- top10_minperdol[, c("Rank", "Player", "Salary", "MinPerDollar")]
top10_minperdol

#Look at correlation to salary for different variables
correlation <- cor(top300salary[, c("Salary", "GP","MP", "PTS", "TRB", "AST", "BLK",
                                   "STL", "PRA", "PER", "VORP", "3PM")], 
                   use = "pairwise.complete.obs")
print(correlation)
#### HIGHEST CORRELATION VARIABLES TO SALARY ####
##        1.PRA 2.PTS 3.VORP 4.MP 5.AST        ##

#Show correlation between Pts and Salary
ggplot(top300salary, aes(x = PTS, y = Salary)) +
  geom_point(color = "skyblue", alpha = 0.7) +
  labs(title = "Scatterplot of Salary vs. PTS", x = "PTS", y = "Salary")

#Show correlation between PRA and Salary
ggplot(top300salary, aes(x = PRA, y = Salary)) +
  geom_point(color = "skyblue", alpha = 0.7) +
  labs(title = "Scatterplot of Salary vs. PRA", x = "PRA", y = "Salary")

