# Script for loading and tidying data from HFD

rm(list = ls())

source_dir <- "input_data/hfd/"

raw_births <- read.table(
  paste(source_dir, "birthsRR.txt", sep = "/"), 
  skip = 2, header = T, stringsAsFactors = F
) %>% tbl_df

raw_exposure <- read.table(
  paste(source_dir, "exposRR.txt", sep = "/"),
  skip = 2, header = T, stringsAsFactors = F
) %>% tbl_df

#glimpse(raw_births)
#glimpse(raw_exposure)

make_age_numeric <- function(x){
  x %>% 
    str_replace_all("[-+]", "") %>% 
    as.integer()
}

raw_births %>% 
  mutate(Age = make_age_numeric(Age)) -> raw_births

names(raw_births) <- tolower(names(raw_births))
names(raw_exposure) <- tolower(names(raw_exposure))

all_hfd <- inner_join(raw_births, raw_exposure)
write_csv(x = all_hfd, path = "tidied_data/tidied_hfd.csv")

rm(list = ls())
