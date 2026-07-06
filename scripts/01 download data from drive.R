# script to download CRCL and MBON data

# load packages
source("scripts/install_packages_function.R")

lp("googledrive")
lp("tidyverse")

# downloading CRCL data
fid<-"https://drive.google.com/drive/u/0/folders/1d6T2TQPgf9abLvkLtPIKGUu44PRmGwDH"
folder_id = drive_get(as_id(fid))
# log in the first time 
files =drive_ls(folder_id)

files<-files[files$name!="scanned",]
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
               path = paste0("odata/",files$name[i]),
               overwrite = TRUE)
}

# downloading MBON data
fid<-"https://drive.google.com/drive/u/0/folders/1ELXeYVzwTcor9PniwcvubCS5SDsnXW2G"
folder_id = drive_get(as_id(fid))
# log in the first time 
files =drive_ls(folder_id)

files<-files[files$name%in% c("Sites and Deployments","Lab Biodiversity"),]
for(i in 1:nrow(files)){
  drive_download(file = files$id[i],
                 path = paste0("odata/MBON_",files$name[i]),
                 overwrite = TRUE)
}
