#Script to visualize data
#Question 1:CRCL Edge vs CRCL Out
#Difference in structure between channel and edge

#load packages----
library(tidyverse)
library(vegan)

#load data----
wholedata<-bind_cols(read.csv("wdata/environment data.csv"), read.csv("wdata/community data.csv"))%>%
  filter(location %in% c("CRCL.edge", "CRCL.channel"))%>%
  filter(season!="Winter")

#split data
crclenv<-wholedata[,1:7]
crclcom<-wholedata[,-1:-7]
crclcom<-crclcom[,!colSums(crclcom)==0]

#season factorization
crclenv$season<-factor(x=crclenv$season,levels = c("Fall", "Winter", "Spring"))

#graph parameters
theme_set(theme_bw()+theme(panel.grid = element_blank(),
                           axis.title = element_text(size=18),
                           axis.text = element_text(size=13),
                           legend.text = element_text(size=11),
                           legend.title = element_text(size=12)))

#graph data boxplot
ggplot(data=crclenv) +
  geom_boxplot(aes(x=season, y=spr, fill=location))+
  ylab("Taxa Richness")+
  xlab("Season") +
  scale_fill_viridis_d(option = "D",begin = 0.4, end = .8, name = "Location", labels = c("Channel", "Edge"))
  
#graph NMDS
crclnmds<-metaMDS(crclcom)
plot(crclnmds)
nout<-data.frame(scores(crclnmds,choices = c(1,2),display = "sites"))
crclenv<-bind_cols(crclenv, nout)%>%
  mutate(pos=paste(location,season))
ggplot(data = crclenv)+
  geom_point(aes(x=NMDS1, y=NMDS2, color=location, shape=season),size=4)+
  scale_color_viridis_d(option = "D",begin = 0.4, end = .8, name = "Location", labels = c("Channel", "Edge")) +
  stat_ellipse(aes(x=NMDS1, y=NMDS2, group = pos, color=location))+
  facet_wrap(~season)
