library(tidyverse)

all_data<-readxl::read_excel("2014.09.xls",skip=7,col_names=FALSE)
colnames(all_data) <- c("brand_name","auto_dom","auto_imp","auto_total","comm_dom","comm_imp","comm_total","total_dom","total_imp","total_total")
all_data <- all_data %>% mutate_if(is.numeric,funs(ifelse(is.na(.),0,.))) %>% mutate(year=2014,month=09)

row_count = nrow(all_data)

for(files in dir()){
  if (files == "2014.09.xls"){
    next
  }
  raw_data<-readxl::read_excel(files,skip=7,col_names=FALSE)
  
  row_count = row_count + nrow(raw_data)
  colnames(raw_data) <- c("brand_name","auto_dom","auto_imp","auto_total","comm_dom","comm_imp","comm_total","total_dom","total_imp","total_total")
  raw_data <- raw_data %>% mutate_if(is.numeric,funs(ifelse(is.na(.),0,.))) %>% mutate(year=as.numeric(strsplit(files, "[.]")[[1]][1]),month=as.numeric(strsplit(files, "[.]")[[1]][2]))
  all_data = rbind(all_data, raw_data)                                                                                    
}

str(all_data)
row_count

all_data = all_data %>%
  filter(!(is.na(brand_name) | brand_name == "TOPLAM:" | brand_name == "TOPLAM" | grepl("ODD", brand_name)))

all_data = all_data %>%
  mutate(brand_name = ifelse(brand_name == "ASTON MARTÝN", "ASTON MARTIN", brand_name))

saveRDS(all_data, file = "all_data")
# all_data = readRDS("all_data")
# all_data = readRDS(gzcon(url("https://github.com/pjournal/boun01g-data-mine-r-s/blob/gh-pages/Final%20TakeHome/all_data?raw=true")))
