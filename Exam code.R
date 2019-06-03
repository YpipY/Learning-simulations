setwd("C:/Users/slmoni/Documents/Uni/Models for perception and action") #setting working diretory

t1<-read.delim("timsigF_211_1_1_1_short.txt", sep= ',',header = FALSE) #loading data

colnames(t1)<-c('sample','time','x','y','z') # renaming columns

library(ggplot2)
ggplot(data=t1, aes(x=x,y=z)) + #plotting x and z coordinates 
  geom_point()+
  labs(title="Plot of x and z coordinates")

t1$z<-t1$z*-1 # Flipping z coordinates 

ggplot(data=t1, aes(x=x,y=z)) + #plotting x and z coordinates 
  geom_point()+
  labs(title="Plot of x and z coordinates")

library(dplyr)
t1$v<-((t1$z-dplyr::lag(t1$z, n= 1L))/(t1$time-dplyr::lag(t1$time, n= 1L))) #calculating z velocity

ggplot(data=t1, aes(x=time,y=v)) + #plotting time and z velocity
  geom_line()+
  labs(title="Plot of z velocity against time", x="time", y="z velocity")

t1[is.na(t1)] <- 0 # changing NA's to 0


library(signal)
bf <- butter(2, 0.2, type ='low') # setting up the Butterworth filter, using a value of 0.2 since it seems to smooth the data well while keeping the overall shape
t1$vs <- filtfilt(bf,t1$v) # applying the filter to the z velocity data
plot(t1$time,t1$v,'l', col = "black") # making a plot to assess the Butterworth filter, first plotting the unfiltered z velocity
lines(t1$time,t1$vs,'l', col = "red") # then plotting the filtered z velocity on top of the unfiltered z velocity
title(paste("z velocity (filter cutoff ",0.2,")",sep="")) # adding a title to the plot

ggplot(data=t1, aes(x=time,y=z)) + #plotting time and z coordinates 
  geom_point() +
  labs(title="Plot of z coordinates against time")

ggplot(data=t1, aes(x=time,y=vs)) + #plotting time and filtered z velocity
  geom_line() +
  labs(title="Plot of z coordinates against time", x="time", y="z velocity")


