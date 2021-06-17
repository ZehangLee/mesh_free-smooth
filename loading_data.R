library(vroom)
library(tictoc)
library(data.table)
library(tidyverse)
library(tidyselect)
library(lubridate)


# 1. read secondary market data firstly 
data_dir = 'D:/esis_data/Secondary-Reserve'
data_dir_folders1 = list.files(data_dir)
data_dir_folders1 = sapply(data_dir_folders1, function(x) paste(data_dir, x, sep = "/"))
data_dir_folders1 = unname(data_dir_folders1)


data_dir_folders2_list = c()
for(iter in data_dir_folders1){
  data_dir_folders2 = list.files(iter)
  data_dir_folders2 = sapply(data_dir_folders2, function(x) paste(iter, x, sep = "/"))
  data_dir_folders2 = unname(data_dir_folders2)
  
  data_dir_folders2_list = append(data_dir_folders2_list, data_dir_folders2)
}

csv_dir_list = sapply(data_dir_folders2_list, function(x) list.files(x, pattern = "csv", full.names = TRUE)[1])
csv_dir_list = unname(csv_dir_list)

tic()
ds = vroom(file = csv_dir_list,col_types = list(datetime = col_character() )) # 374.88 sec elapsed
toc()

secondary_ds = as.data.frame(ds)
head(secondary_ds)
fwrite(secondary_ds, "secondary_ds.csv",col.names=T,row.names=F,quote = F)
rm(ds)


#secondary_ds = vroom('secondary_ds.csv')
#unique(secondary_ds$name)

#date1= make_datetime(2013,12,31,23,0,0,"UTC")




# 2. generate quantity matrix and price matrix of the secondary market to up

Secondary_up_bids = secondary_ds %>%
  filter(name == "Secondary reserve up bids")


secondary_up_bids_sort = Secondary_up_bids %>% drop_na() %>%
  group_by(datetime) %>%
  arrange(`Euro/MW`,.by_group = TRUE)

secondary_up_bids_offers_all =  secondary_up_bids_sort %>%
  dplyr::select(datetime, MW) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, MW) %>%
  select(-row_id)

secondary_up_bids_offers_all = t(secondary_up_bids_offers_all)
secondary_up_bids_offers_all = data.frame(secondary_up_bids_offers_all)
secondary_up_bids_offers_all = secondary_up_bids_offers_all %>% add_column(0, .before = 1)
colnames(secondary_up_bids_offers_all)[1] = "X0"
secondary_up_bids_offers_all = rownames_to_column(secondary_up_bids_offers_all, var = "datetime")
vroom_write(secondary_up_bids_offers_all, "secondary_up_bids_offers_all.csv",delim = ",")


secondary_up_bids_prices_all =  secondary_up_bids_sort %>%
  dplyr::select(datetime, `Euro/MW`) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, `Euro/MW`) %>%
  select(-row_id)

secondary_up_bids_prices_all = t(secondary_up_bids_prices_all)
secondary_up_bids_prices_all = data.frame(secondary_up_bids_prices_all)
secondary_up_bids_prices_all = secondary_up_bids_prices_all %>% add_column(0, .before = 1)
colnames(secondary_up_bids_prices_all)[1] = "X0"
secondary_up_bids_prices_all = rownames_to_column(secondary_up_bids_prices_all, var = "datetime")
vroom_write(secondary_up_bids_prices_all, "secondary_up_bids_prices_all.csv",delim = ",")


rm(Secondary_up_bids,secondary_up_bids_sort)



# 3. generate quantity matrix and price matrix of the secondary market to down

Secondary_down_bids = secondary_ds %>%
  filter(name == "Secondary reserve down bids")


secondary_down_bids_sort = Secondary_down_bids %>% drop_na() %>%
  group_by(datetime) %>%
  arrange(`Euro/MW`,.by_group = TRUE)

secondary_down_bids_offers_all =  secondary_down_bids_sort %>%
  dplyr::select(datetime, MW) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, MW) %>%
  select(-row_id)

secondary_down_bids_offers_all = t(secondary_down_bids_offers_all)
secondary_down_bids_offers_all = data.frame(secondary_down_bids_offers_all)
secondary_down_bids_offers_all = secondary_down_bids_offers_all %>% add_column(0, .before = 1)
colnames(secondary_down_bids_offers_all)[1] = "X0"
secondary_down_bids_offers_all = rownames_to_column(secondary_down_bids_offers_all, var = "datetime")
vroom_write(secondary_down_bids_offers_all, "secondary_down_bids_offers_all.csv",delim = ",")


secondary_down_bids_prices_all =  secondary_down_bids_sort %>%
  dplyr::select(datetime, `Euro/MW`) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, `Euro/MW`) %>%
  select(-row_id)

secondary_down_bids_prices_all = t(secondary_down_bids_prices_all)
secondary_down_bids_prices_all = data.frame(secondary_down_bids_prices_all)
secondary_down_bids_prices_all = secondary_down_bids_prices_all %>% add_column(0, .before = 1)
colnames(secondary_down_bids_prices_all)[1] = "X0"
secondary_down_bids_prices_all = rownames_to_column(secondary_down_bids_prices_all, var = "datetime")
vroom_write(secondary_down_bids_prices_all, "secondary_down_bids_prices_all.csv",delim = ",")

rm(Secondary_down_bids,secondary_down_bids_sort)
rm(secondary_ds)





# 4. read tertiary market (to up) data 
data_dir = 'D:/esis_data/Tertiary-reserve_Up'
data_dir_folders1 = list.files(data_dir)
data_dir_folders1 = sapply(data_dir_folders1, function(x) paste(data_dir, x, sep = "/"))
data_dir_folders1 = unname(data_dir_folders1)


 data_dir_folders2_list = c()
 for(iter in data_dir_folders1){
   data_dir_folders2 = list.files(iter)
   data_dir_folders2 = sapply(data_dir_folders2, function(x) paste(iter, x, sep = "/"))
   data_dir_folders2 = unname(data_dir_folders2)

   data_dir_folders2_list = append(data_dir_folders2_list, data_dir_folders2)
 }

csv_dir_list = sapply(data_dir_folders2_list, function(x) list.files(x, pattern = "csv", full.names = TRUE)[1])
csv_dir_list = unname(csv_dir_list)


tic()
ds = vroom(file = csv_dir_list, delim = ";",col_types = list(datetime = col_character() )) 
toc()

tertiary_up_ds = as.data.frame(ds)
fwrite(tertiary_up_ds, "tertiary_up_ds.csv",col.names=T,row.names=F,quote = F)
rm(ds)


#tertiary_up_ds = vroom('tertiary_up_ds.csv')





# 5. generate quantity matrix and price matrix of the tertiary market to up
tertiary_up_bids = tertiary_up_ds %>%
  filter(name == "Tertiary reserve up bids")


tertiary_up_bids_sort = tertiary_up_bids %>% drop_na() %>%
  group_by(datetime) %>%
  arrange(`Euro/MW`,.by_group = TRUE)

tertiary_up_bids_offers_all =  tertiary_up_bids_sort %>%
  dplyr::select(datetime, MW) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, MW) %>%
  select(-row_id)

tertiary_up_bids_offers_all = t(tertiary_up_bids_offers_all)
tertiary_up_bids_offers_all = data.frame(tertiary_up_bids_offers_all)
tertiary_up_bids_offers_all = tertiary_up_bids_offers_all %>% add_column(0, .before = 1)
colnames(tertiary_up_bids_offers_all)[1] = "X0"
tertiary_up_bids_offers_all = rownames_to_column(tertiary_up_bids_offers_all, var = "datetime")
vroom_write(tertiary_up_bids_offers_all, "tertiary_up_bids_offers_all.csv",delim = ",")


tertiary_up_bids_prices_all =  tertiary_up_bids_sort %>%
  dplyr::select(datetime, `Euro/MW`) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, `Euro/MW`) %>%
  select(-row_id)

tertiary_up_bids_prices_all = t(tertiary_up_bids_prices_all)
tertiary_up_bids_prices_all = data.frame(tertiary_up_bids_prices_all)
tertiary_up_bids_prices_all = tertiary_up_bids_prices_all %>% add_column(0, .before = 1)
colnames(tertiary_up_bids_prices_all)[1] = "X0"
tertiary_up_bids_prices_all = rownames_to_column(tertiary_up_bids_prices_all, var = "datetime")
vroom_write(tertiary_up_bids_prices_all, "tertiary_up_bids_prices_all.csv",delim = ",")





# 6. read tertiary market (to down) data 
data_dir = 'D:/esis_data/Tertiary-reserve_Down'
data_dir_folders1 = list.files(data_dir)
data_dir_folders1 = sapply(data_dir_folders1, function(x) paste(data_dir, x, sep = "/"))
data_dir_folders1 = unname(data_dir_folders1)


data_dir_folders2_list = c()
for(iter in data_dir_folders1){
  data_dir_folders2 = list.files(iter)
  data_dir_folders2 = sapply(data_dir_folders2, function(x) paste(iter, x, sep = "/"))
  data_dir_folders2 = unname(data_dir_folders2)

  data_dir_folders2_list = append(data_dir_folders2_list, data_dir_folders2)
}

csv_dir_list = sapply(data_dir_folders2_list, function(x) list.files(x, pattern = "csv", full.names = TRUE)[1])
csv_dir_list = unname(csv_dir_list)

# csv_dir_list = sapply(data_dir_folders1, function(x) list.files(x, pattern = "csv", full.names = TRUE)[1])
# csv_dir_list = unname(csv_dir_list)
csv_dir_list = na.omit(csv_dir_list)

tic()
ds = vroom(file = csv_dir_list, delim = ";",col_types = list(datetime = col_character() )) 
toc()

tertiary_down_ds = as.data.frame(ds)
fwrite(tertiary_down_ds, "tertiary_down_ds.csv",col.names=T,row.names=F,quote = F)
rm(ds)




# 7. generate quantity matrix and price matrix of the tertiary market to down
tertiary_down_bids = tertiary_down_ds %>%
  filter(name == "Tertiary reserve down bids")


tertiary_down_bids_sort = tertiary_down_bids %>% drop_na() %>%
  group_by(datetime) %>%
  arrange(`Euro/MW`,.by_group = TRUE)

tertiary_down_bids_offers_all =  tertiary_down_bids_sort %>%
  dplyr::select(datetime, MW) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, MW) %>%
  select(-row_id)

tertiary_down_bids_offers_all = t(tertiary_down_bids_offers_all)
tertiary_down_bids_offers_all = data.frame(tertiary_down_bids_offers_all)
tertiary_down_bids_offers_all = tertiary_down_bids_offers_all %>% add_column(0, .before = 1)
colnames(tertiary_down_bids_offers_all)[1] = "X0"
tertiary_down_bids_offers_all = rownames_to_column(tertiary_down_bids_offers_all, var = "datetime")
vroom_write(tertiary_down_bids_offers_all, "tertiary_down_bids_offers_all.csv",delim = ",")


tertiary_down_bids_prices_all =  tertiary_down_bids_sort %>%
  dplyr::select(datetime, `Euro/MW`) %>%
  mutate(row_id = row_number()) %>%
  spread(datetime, `Euro/MW`) %>%
  select(-row_id)

tertiary_down_bids_prices_all = t(tertiary_down_bids_prices_all)
tertiary_down_bids_prices_all = data.frame(tertiary_down_bids_prices_all)
tertiary_down_bids_prices_all = tertiary_down_bids_prices_all %>% add_column(0, .before = 1)
colnames(tertiary_down_bids_prices_all)[1] = "X0"
tertiary_down_bids_prices_all = rownames_to_column(tertiary_down_bids_prices_all, var = "datetime")
vroom_write(tertiary_down_bids_prices_all, "tertiary_down_bids_prices_all.csv",delim = ",")
