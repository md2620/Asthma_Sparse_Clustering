#### MATRIX PLOTS REVISED #####

#Clear environment
rm(list=ls())

#Load dataset
mcclain_viz = read.csv("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_mcclain.csv")

#Drop first useless variable
mcclain_vizu <- mcclain_viz[-1]

#Find min and max mcclain score
min(mcclain_vizu$Mcclain_Score) #0.866971
max(mcclain_vizu$Mcclain_Score) #1.061877
midpoint <- (1.061877+0.866971)/2 #0.964424

#Order of optimality
#score of 1.06 for K = 2 and lambda = 3.1
#score of 0.993 for K = 3 and lambda = 0.2
#score of 0.971 for K = 4 and lambda = 4.1
#Greatest mcclain score grouped by K
opt_mcclain <- mcclain_vizu %>% group_by(K) %>% top_n(-1, Mcclain_Score)
opt_mcclain <- as.data.table(opt_mcclain)
opt_mcclain$Mcclain_Score
#Create dataframe of this and plot it + HTML table of optimal lambda and score for each K. 
#################### Plot mcclain score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/optimum_mcclain.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(opt_mcclain$K,opt_mcclain$Mcclain_Score, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(opt_mcclain$Mcclain_Score == min(opt_mcclain$Mcclain_Score), "red", "black"),
     xlab="Number of clusters K",
     ylab="McClain score")
legend("topright", legend = "optimum criterion : min")
title(main = "Optimized mcclain score according to number of clusters K chosen")

dev.off()

#Visualize mcclain matrix
library(ggplot2)
library(scales)
plt_mcclain_edit <- ggplot(mcclain_vizu,aes(Lambda,K))+
  geom_tile(aes(fill=Mcclain_Score))+
  #scale_fill_gradient2(low = muted("darkred"), 
                       #mid = "white", 
                       #high = muted("red"), 
                       #midpoint = 0.964424) +
  theme(legend.title=element_text(face="bold", size=14)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7",trans = 'reverse')+
  ggtitle("Minimize McClain Score by calibrating lambda coefficient and K")+
  geom_vline(xintercept = c(2.3,3.1)
             ,linetype= c("dashed","dotted"), 
             color = c("grey","red")
             , size=1) +
  geom_hline(yintercept = 3,
             linetype= "dotted",
             color = "red"
             ,size=1) +
  geom_text(aes(x=2.35, label="\ndefault lambda", y=8), colour="grey", angle=90, text=element_text(face="plain",size=6))+
  geom_text(aes(x=3.2, label="\noptimal choice for K-prototype", y=8), colour="red", angle=90, text=element_text(face="plain",size=1))+
  labs(fill="Mcclain score")

plt_mcclain_edit + scale_x_continuous(name="Lambda", limits=c(0, 5)) +
  scale_y_continuous(name="K", limits=c(2, 15))

# Graduation de K de 1 en 1
plt_mcclain_edit + scale_y_continuous(breaks=seq(2,15,1)) + scale_x_continuous(breaks=seq(0,5,0.1))
ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/mcclain_matrix_edited.jpeg", width = 14, height = 8)



############### EDIT SILHOUETTE SCORE MATRIX ##############
#Load dataset
silhouette_viz = read.csv("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/df_silhouette.csv")

#Drop first useless variable
silhouette_vizu <- silhouette_viz[-1]

#Find min and max silhouette score
min(silhouette_vizu$Silhouette_Score) #-0.08444784
max(silhouette_vizu$Silhouette_Score) #0.1529021
midpoint <- (-0.08444784+0.1529021)/2 #0.03422713

#Order of optimality
#Greatest silhouette score grouped by K
opt_silhouette <- silhouette_vizu %>% group_by(K) %>% top_n(1, Silhouette_Score)
opt_silhouette <- as.data.table(opt_silhouette)
opt_silhouette
#Create dataframe of this and plot it + HTML table of optimal lambda and score for each K. 
#################### Plot silhouette score #####################
pdf(file = "/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/optimum_silhouette.pdf",   # The directory you want to save the file in
    width = 8, # The width of the plot in inches
    height = 8)

plot(opt_silhouette$K,opt_silhouette$Silhouette_Score, frame = TRUE, 
     col = "black",
     type = "o",
     pch = 21,
     bg = ifelse(opt_silhouette$Silhouette_Score == max(opt_silhouette$Silhouette_Score), "red", "black"),
     xlab="Number of clusters K",
     ylab="Silhouette score")
legend("topright", legend = "optimum criterion : max")
title(main = "Optimized silhouette score according to number of clusters K chosen")

dev.off()

#Visualize silhouette matrix
library(ggplot2)
library(scales)
plt_silhouette_edit <- ggplot(silhouette_vizu,aes(Lambda,K))+
  geom_tile(aes(fill=Silhouette_Score))+
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")+
  #scale_fill_gradient2(low = muted("darkred"), 
  #mid = "white", 
  #high = muted("red"), 
  #midpoint = 0.964424) +
  theme(legend.title=element_text(face="bold", size=14)) + 
  ggtitle("Maximize Silhouette Score by calibrating lambda coefficient and K") +
  geom_vline(xintercept = c(2.3,3.1),
             linetype= c("dashed","dotted"),
             color = c("grey","red")
             ,size=1) +
  geom_hline(yintercept = 3,
             linetype= "dotted",
             color = "red"
             ,size=1) +
  geom_text(aes(x=2.35, label="\ndefault lambda", y=8), colour="grey", angle=90, text=element_text(face="plain",size=1))+
  geom_text(aes(x=3.2, label="\noptimal choice for K-prototype", y=8), colour="red", angle=90, text=element_text(face="plain",size=1))+
  labs(fill="Silhouette score")


plt_silhouette_edit + scale_x_continuous(name="Lambda", limits=c(0, 5)) +
  scale_y_continuous(name="K", limits=c(2, 15))

# Graduation de K de 1 en 1
plt_silhouette_edit + scale_y_continuous(breaks=seq(2,15,1)) + scale_x_continuous(breaks=seq(0,5,0.2))

ggsave("/rds/general/user/md2620/home/asthma/Malo/Results/Calibration/silhouette_matrix_edited.jpeg", width = 14, height = 8)

