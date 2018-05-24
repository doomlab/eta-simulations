library(readr)
fulldata <- read_csv("~/OneDrive - Missouri State University/RESEARCH/2 projects/Eta simulations/totals/fulldata.csv")
fulldata = subset(fulldata, stdev == 5)
fulldata$levels = as.factor(fulldata$levels)
histeta = ggplot(fulldata, aes(RM1.ges, fill = levels))
histeta +
  geom_histogram(binwidth = .001) +
  cleanup + 
  xlab("Repeated Measures GES") + 
  ylab("Frequency") +
  scale_fill_manual(name = "Levels", 
                    values = c("yellow", "red", "blue", "purple"))

fulldata$correl = as.factor(fulldata$correl)
histeta = ggplot(fulldata, aes(RM1.ges, fill = correl))
histeta +
  geom_histogram(binwidth = .001) +
  cleanup + 
  xlab("Repeated Measures GES") + 
  ylab("Frequency") +
  scale_fill_manual(name = "Levels", 
                    values = c("yellow", "red", "blue", "purple"))