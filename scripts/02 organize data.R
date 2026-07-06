# organize CRCL data

#Stephanie K. Archer 7/2/2026

#load packages----

source("scripts/install_packages_function.R")
source("scripts/01 download data from drive.R")
lp("tidyverse")
lp("readxl")

# load data----
crcl.trps.dr<-read_xlsx("odata/CRCL Traps.xlsx",sheet = 2)
crcl.trps<-read_xlsx("odata/CRCL Traps.xlsx",sheet = 3)
crcl.trays<-read_xlsx("odata/CRCL Trays.xlsx",sheet = 3)
mbon.trays<-read_xlsx("odata/MBON_Lab Biodiversity.xlsx",sheet=3)
taxa<-read_xlsx("odata/MBON_Lab Biodiversity.xlsx",sheet=8)%>%
  select(OldTaxaID,taxaID,scientific)%>%
  distinct()


# start to organize data----
# first CRCL trays
# first deal with the easy taxa (aka not vouchers)
crcl.trays2.novouch<-crcl.trays%>%
  filter(!voucher %in% c("y","Y"))%>%
  rename(OldTaxaID=taxaID)%>%
  left_join(taxa)%>%
  mutate(biomass=ifelse(dry.weight<=tin.weight,0.001,dry.weight-tin.weight))%>%
  group_by(lab.processor,date.retrieved,location,tray,taxaID)%>%
  summarize(abund=sum(abundance),
            biomass=sum(biomass))

# pull out vouchered taxa
crcl.trays2.vouch<-crcl.trays%>%
  filter(voucher %in% c("y","Y"))

# see if any of these taxa have a dry weight
vouch.tax<-unique(crcl.trays2.vouch$taxaID)
vouch.tax[vouch.tax%in%unique(crcl.trays2.novouch$taxaID)]

# some do and some don't. So for now we'll use abundance so can do only one dataset
crcl.trays2<-crcl.trays%>%
  rename(OldTaxaID=taxaID)%>%
  left_join(taxa)%>%
  group_by(lab.processor,date.retrieved,location,tray,taxaID)%>%
  summarize(abund=sum(abundance))%>%
  mutate(location=case_when(
    location=="e"~"CRCL.edge",
    location=="o"~"CRCL.channel"))

#now organize MBON trays and only keep what has been collected at LUMO3 or LUMO6 since September 2025
mbon.trays2<-mbon.trays%>%
  rename(OldTaxaID=taxaID)%>%
  left_join(taxa)%>%
  group_by(lab.processor,date.retrieved,location=site,tray,taxaID)%>%
  summarize(abund=sum(abundance))%>%
  filter(date.retrieved>="2025-09-01")%>%
  filter(location %in% c("LUMO3","LUMO6"))

# need to add in code to bring in amphipods for MBON here

# combine and add season
combined.trays<-bind_rows(mbon.trays2,crcl.trays2)%>%
  mutate(season=case_when(
    month(date.retrieved) %in% c(12,1,2)~"Winter",
    month(date.retrieved) %in% c(3,4,5)~"Spring",
    month(date.retrieved) %in% c(6,7,8)~"Summer",
    month(date.retrieved) %in% c(9,10,11)~"Fall"),
    yr=year(date.retrieved),
    yr=ifelse(month(date.retrieved)==12,yr+1,yr))

# after amphipod diversity is added back in we can save the dataset here for analysis

# get number of samples processed
samples.processed<-combined.trays%>%
  ungroup()%>%
  select(yr,season,location,tray)%>%
  distinct()%>%
  mutate(deploy=paste(season,yr))%>%
  group_by(deploy,location)%>%
  summarize(n.remaining=6-n())%>%
  pivot_wider(names_from=deploy,values_from = n.remaining,values_fill = 6)
  
