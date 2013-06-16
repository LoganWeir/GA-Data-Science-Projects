data <- read.csv ("HIST_FED_TAX_IND_ITR.csv")
data$Year <- substr(data$Year, 1, 4)
data$Year <- as.numeric(data$Year)

t1 <- data[,c("Year","Lowest.Quintile")]
t2 <- data[,c("Year","Middle.Quintile")]
t3 <- data[,c("Year", "Highest.Quintile")]

t3$Bracket <- "Highest Quintile"
t2$Bracket <- "Middle Quintile"
t1$Bracket <- "Lowest Quintile"

colnames(t1) [2] <- "tax"
colnames(t2) [2] <- "tax"
colnames(t3) [2] <- "tax"

taxes <- rbind (t1,t2,t3)

tplot <- ggplot(t) + aes(Year, tax, color=bracket)
tplot <- tplot + geom_path()
tplot <- tplot + geom_abline(intercept = 0.0, slope =0)
tplot <- tplot + xlab("Year") + ylab("Individual Income Tax Rate") + ggtitle("Enjoy Your Taxes, Bourgeois Scum")
tplot <- tplot + annotate("text", x = 1985, y = 12, color = "red", label = "-2.5% Change")
tplot <- tplot + annotate("text", x = 1985, y = 4, color = "blue", label = "-6.1% Change")
tplot <- tplot + annotate("text", x = 1985, y = -4, color = "green", label = "-9.3% Change")

tplot