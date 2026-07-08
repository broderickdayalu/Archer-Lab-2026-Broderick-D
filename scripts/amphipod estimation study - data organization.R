# script to organize amphipod estimation data

source("scripts/install_packages_function.R")
lp("readxl")
lp("tidyverse")

# read in data
ag<-read_xlsx("odata/MBON_Lab Biodiversity.xlsx",sheet=5)
ac<-read_xlsx("odata/MBON_Lab Biodiversity.xlsx",sheet=6)



# calculate estimates of abundance and biomass
# there are 36 grid cells in a plate

# start with all 

all<-ac%>%
  filter(!is.na(dry.weight))%>%
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))%>%
  group_by(lab.processor,date.retrieved,site,tray,taxaID)%>%
  summarize(true.abundance=sum(abundance,na.rm = T),
            true.biomass=sum(biomass,na.rm=T))

# then first 50
# separate into trays that did/didn't have more than 50
l50<-ac%>%
  group_by(date.retrieved,site,tray)%>%
  summarize(abund=sum(abundance))%>%
  filter(abund<=50)%>%
  select(-abund)
# deal with the less than 50s first because what it is is what it is

l502<-ac%>%
  inner_join(l50)%>%
  filter(!is.na(dry.weight))%>%
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))%>%
  group_by(lab.processor,date.retrieved,site,tray,taxaID)%>%
  summarize(c50.biomass.est=sum(biomass,na.rm=T),
            c50.abundance.est=sum(abundance,na.rm=T))

# now deal with more than 50s
# need the "gross" weight of all remaining
all2<-ac%>%
  filter(method=="all")%>%
  filter(!is.na(dry.weight))%>%
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))%>%
  group_by(lab.processor,date.retrieved,site,tray)%>%
  summarize(gdw=sum(biomass,na.rm=T))

m502<-ac%>%
  anti_join(l50)%>%
  filter(method!="all")%>%
  filter(!is.na(dry.weight))%>%
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))%>%
  group_by(lab.processor,date.retrieved,site,tray,taxaID)%>%
  summarize(bio=sum(biomass),
            abd=sum(abundance))%>%
  left_join(all2)%>%
  mutate(bi=round(abd/50*gdw,3),
         ai=round(bi/(bio/abd),0),
         c50.biomass.est=bi+bio,
         c50.abundance.est=ai+abd)%>%
  select(colnames(l502))
  
c50<-bind_rows(l502,m502)%>%
  mutate(c50.biomass.est=ifelse(c50.biomass.est<0.001,0.001,c50.biomass.est))

#finally grid.

all3<-ac%>%
  filter(!is.na(dry.weight))%>%
  mutate(biomass=dry.weight-tin.weight,
         biomass=ifelse(biomass<=0,0.001,biomass))%>%
  group_by(lab.processor,date.retrieved,site,tray)%>%
  summarize(gdw=sum(biomass,na.rm=T))

ag2<-ag%>%
  pivot_longer(grid1:grid5,names_to = "rep",values_to="count")%>%
  group_by(lab.processor,date.retrieved,site,tray,taxaID)%>%
  summarize(grid.abundance.est=round(mean(count)*36,0))%>%
  ungroup()%>%
  group_by(lab.processor,date.retrieved,site,tray)%>%
  mutate(total.c=sum(grid.abundance.est),
         prop.c=grid.abundance.est/total.c)%>%
  left_join(all3)%>%
  mutate(grid.biomass.est=round(gdw*prop.c,3),
         grid.biomass.est=ifelse(grid.biomass.est<0.001,0.001,grid.biomass.est))%>%
  select(lab.processor,date.retrieved,site,tray,taxaID,grid.abundance.est,grid.biomass.est)


# full abundance and biomass est dataset
full<-left_join(all,c50)%>%
  left_join(ag2)

full[is.na(full)]<-0

write.csv(full,"wdata/amphipod estimation biomass abundance.csv",row.names = F)
