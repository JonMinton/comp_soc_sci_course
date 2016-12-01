
# Examples of automating outputs using HFD data 


# 3_1_1) First, we load the data 

dta_hfd <- read_csv("tidied_data/tidied_hfd.csv")



# 3_1_2) Heatmaps as figures 
# In this exercise we will create heatmaps of ASFRs for each country in the HFD

# We start by creating a directory to put the figures in
dir.create("figures/hfd/asfr/", recursive = TRUE)


spool_asfr_figs <- function(CODE, DTA){
  min_year <- min(DTA$year)
  max_year <- max(DTA$year)
  
  title_label <- paste0(
    CODE, " (", min_year, " - ", max_year, ")"
  )
  
  file_label <- paste0(
    "asfr_", CODE, "_(", min_year, "_", max_year, ")" 
  )
  
  png(paste0("figures/hfd/asfr/", file_label, ".png"),
      res=300, width=25, height=25, units = "cm"
  )
  
  
  
  p <- DTA %>% filter( age <= 50 ) %>% 
    levelplot(
      asfr ~ year * age , 
      data=. , 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      cex=1.4,
      cuts=40,
      col.regions=colorRampPalette(brewer.pal(12, "Paired"))(200),
      main=CODE,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  print(p)
  dev.off()
  
  return(NULL)
}

dta_hfd %>% 
  mutate(asfr = total / exposure) %>% 
  group_by(code) %>% 
  nest() -> nested_hfd

walk2(
    .x = nested_hfd[["code"]], .y = nested_hfd[["data"]], 
    .f = spool_asfr_figs
)

# 3_1_3) Heatmaps as figures - additional arguments and parameters

# We might instead want to look over a common range of years, and use a common level plot scale for all figures 

# We know that the maximum ASFR is slightly below 0.30, so this will be hard-coded into the data
# If we want to make the function more generic this can be adapted accordingly. 

dir.create("figures/hfd/asfr_fixed/", recursive = TRUE)
spool_fixed_asfr_figs <- function(CODE, DTA, YEAR_RANGE){
  
  min_year <- max(YEAR_RANGE[1], min(DTA$year))
  max_year <- min(YEAR_RANGE[2], max(DTA$year))
  
  title_label <- paste0(
    CODE, " (", min_year, " - ", max_year, ")"
  )
  
  file_label <- paste0(
    "asfr_", CODE, "_(", min_year, "_", max_year, ")" 
  )
  
  png(paste0("figures/hfd/asfr_fixed/", file_label, ".png"),
      res=300, width=25, height=25, units = "cm"
  )
  
  
  
  p <- DTA %>% filter( age <= 50 ) %>% 
    levelplot(
      asfr ~ year * age , 
      data=. , 
      region=T, 
      par.strip.text=list(cex=1.4, fontface="bold"),
      ylab=list(label="Age in years", cex=1.4),
      xlab=list(label="Year", cex=1.4),
      xlim = YEAR_RANGE,
      cex=1.4,
      col.regions=colorRampPalette(brewer.pal(12, "Paired"))(200),
      at = seq(0, 0.30, by = 0.005),
      main=CODE,
      labels=list(cex=1.2),
      col="black",
      scales=list(
        x=list(cex=1.4), 
        y=list(cex=1.4),
        alternating=3
      )
    )
  
  print(p)
  dev.off()
  
  return(NULL)
}

dta_hfd %>% 
  mutate(asfr = total / exposure) %>% 
  mutate(max_asfr = max(asfr)) %>% 
  group_by(code) %>% 
  nest() -> nested_hfd

walk2(
  .x = nested_hfd[["code"]], 
  .y = nested_hfd[["data"]],
  .f = spool_fixed_asfr_figs,
  YEAR_RANGE = c(1950, 2010) # additional arguments to the function specified by .f go here
)

# Note: if we want to produce many permutations of the above, instead of writing out almost the 
# same function twice, we should produce a more generic function with more arguments. 


# 3_1_4) Outputing data as matrices

# In the example below we first calculate the ASFR for all data combined, then the deviation from the overall
# value for each specific country. We then convert the ASFR differences into a matrix and output to a text file


dir.create("tidied_data/asfr_difference_matrices", recursive = TRUE)

# First ASFR for all countries combined
dta_hfd %>% 
  group_by(year, age) %>% 
  summarise(total = sum(total), exposure = sum(exposure)) %>% 
  ungroup %>% 
  mutate(asfr_all = total/exposure) %>% 
  select(year, age, asfr_all) -> asfr_overall

# Now calculate and combine for individual countries 
dta_hfd %>% 
  mutate(asfr = total/exposure) %>% 
  select(code, year, age, asfr) %>% 
  inner_join(asfr_overall) %>% 
  mutate(asfr_deviation = asfr - asfr_all) %>% 
  select(code, year, age, asfr_deviation) %>% 
  group_by(code) %>% 
  nest() -> nested_asfr_differences

#our code for writing out the differences as matrices is now as follows

write_out_difference_matrix <- function(CODE, DTA){
  years <- DTA$year
  
  DTA %>% 
    spread(age, asfr_deviation) -> tmp
  years <- tmp$year
  tmp %>% 
    select(-year) %>% 
    as.matrix -> dta_matrix
  rownames(dta_matrix) <- years
  
  write.csv(
    x = dta_matrix,
    file  = paste0(
      "tidied_data/asfr_difference_matrices/",
      CODE, ".csv"
    )
  )
  
  return(NULL)
}

walk2(
  .x = nested_asfr_differences$code,
  .y = nested_asfr_differences$data,
  .f = write_out_difference_matrix
)

# Optional task: look through the .csv files produced in excel and use the colour coding feature to 
# produce simple heatmaps of the data.





