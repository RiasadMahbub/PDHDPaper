### plot some DOP and DOH data
## read the csv file
dfgroudtruthdata<- read.csv('C:/Users/rbmahbub/Documents/Data/DOPDOH-paper/GroundTruthData/CG_RiceDOPDOH_201520162018201920202021.csv')
plot(dfgroudtruthdata)
dfgroudtruthdata
library(ggplot2)

# Load the data
data(diamonds)
dfgroudtruthdata$DOP_doy <-as.numeric(dfgroudtruthdata$DOP_doy)
# Create the histogram
ggplot(dfgroudtruthdata, aes(x=DOP_doy)) +
  geom_histogram()+
  xlab("Day of planting") +
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(0, 170, by = 10)) +
  geom_vline(aes(xintercept = mean(DOP_doy, na.rm = TRUE)),col='red',size=2)+
  annotate("text", x= 20, y = 30, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

# Create the histogram
ggplot(dfgroudtruthdata, aes(x=DOH_doy)) +
  geom_histogram()+
  xlab("Day of harvest")+
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(220, 300, by = 10)) +
  geom_vline(aes(xintercept = mean(DOH_doy)),col='red',size=2)+
  annotate("text", x= 230, y = 15, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

dfgroudtruthdata$LOGS<-as.numeric(dfgroudtruthdata$LOGS)
# Create the histogram
ggplot(dfgroudtruthdata, aes(x=LOGS)) +
  geom_histogram()+
  xlab("Length of growing season (days)")+
  ylab("Frequency")+
  scale_x_continuous(breaks = seq(100, 270, by = 15)) +
  geom_vline(aes(xintercept = mean(LOGS, na.rm = TRUE)),col='red',size=2)+
  annotate("text", x= 220, y = 40, label = paste0("N= ", nrow(dfgroudtruthdata)))+
  theme_classic() +
  theme(text = element_text(size = 12))

