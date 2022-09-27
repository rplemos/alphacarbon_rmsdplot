#Libraries
library(ggplot2)
library(dplyr)

#File loading (4 chain comparisons in .csv format. Missing values were changed
#to "NA", and outliers in terminal regions were removed.
ChainA <- read.csv(file = 'chainA.csv')
ChainB <- read.csv(file = 'chainB.csv')
ChainC <- read.csv(file = 'chainC.csv')
ChainD <- read.csv(file = 'chainD.csv')

#Loading all 4 files into a dataframe ([2] equals to RMSD values only)
df = data.frame(ChainA,ChainB[2],ChainC[2],ChainD[2])

#Calculating the mean for every row in the dataframe, and loading the values in,
#a new dataframe, along with the residue number
mean_line <- rowMeans(df[2:5], na.rm=TRUE)
df2 <- data.frame(df[1],mean_line)

#Calculating the mean of the corrected RMSD values for the whole column, as well
#as the Standard Deviation (SD)
mean_total <- mean(df2$mean_line, na.rm=TRUE)
stddev <- sd(df2$mean_line, na.rm=TRUE)

#Getting the last residue for the x axis of the plot
last_residue<-as.numeric(tail(df[1],n=1))

#Plotting the graph
ggplot(df2,aes(x=Residue,y=mean_line)) +
  geom_bar(stat="identity", color="white",fill="black") +
  ggtitle ("Protein 1 x Protein 2") +
  ylab("C\u03b1 RMSD (Å)") +
  xlab("Residue") +
  geom_hline(yintercept = mean_total, col='red', size=1) +
  geom_hline(yintercept = mean_total + stddev, linetype='dashed', col='red',size=1) +
  geom_hline(yintercept = 2*stddev, linetype='dotted', col='red', size=1) +
  annotate("text",x=5, y=mean_total+0.1, label="Mean", size=4, col='red') +
  annotate("text",x=5, y=mean_total+stddev-0.08, label="Mean+SD", size=4, col='red') +
  annotate("text",x=5, y=2*stddev+0.08, label="2*SD", size=4, col='red') +
  scale_x_continuous(breaks=seq(0,last_residue,5), limits=c(0,last_residue), expand=c(0,0)) +
  scale_y_continuous(limits=c(0,5.5), expand=c(0,0)) +
  theme_bw() +
  theme(
    plot.title = element_text(size=15,hjust=0.5, face = "bold"),
    axis.title.x = element_text(size=11, face = "bold"),
    axis.title.y = element_text(size=11, face = "bold"),
    axis.text.x = element_text(size=11, face = "bold"),
    axis.text.y = element_text(size=11, face = "bold")
  )

