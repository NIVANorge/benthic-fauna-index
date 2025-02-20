library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(lubridate)
library(openxlsx)

source("app/functions.R")

options <- get_options("app/options.R")

# example data for testing the app
xlfile <- "notes/20241111 Macroalgae Index/examples/test2.xlsx"

# status class boundaries
dfbnds <- read.table("app/class_boundaries.txt", sep=";", header=T)

# read types for station codes - 
# specifies the type of index to use at a station (RSLA 1, etc.)
dftype <- read.table("app/stn_index_type.txt", sep=";", header=T)

# read species lists for matching with observations
dfgrps  <- read.table("app/species_lists.txt", sep=";", header=T)

dfgrps <- dfgrps  %>%
  mutate(taxaID=row_number()) %>%
  relocate(taxaID, .before=1) %>%
  mutate(RSLA2=RSLA.1.2) %>%
  rename(RSLA1=RSLA.1.2, RSLA3 = RSLA.3, RSL4=RSL.4) %>% 
  relocate(RSLA2, .after=RSLA1)

# get all data from the Excel file
df_list <- read_excel_all(xlfile)

# get the sheet containing species observations (default = 'Strandobservasjoner')
df <- df_list[[default_sheet()]]

# get station information, including points for site characteristics
dfstns <- observation_info(df, options)

# get the species observations
dfobs <- species_data(df, dfstns, options)


# match observations with species lists
df <- match_obs_species_lists(dfstns, dfobs, dfgrps, dftype)

# calculate the index values required 
# ratios of number of species in different algae classes, etc. 
dfind <- obs_indices(df, dfstns)

# calculate EQR value for the index parameters, using classcboundary values
dfeqr <- eqr_results(dfind, dfbnds)

# calculate the overall average EQR for each station
dfeqr_stn <- eqr_results_mean(dfeqr)

# this function combines the EQR results for individual parameterd
# with EQR averages per staion to give the final result table
dfeqr_tab <- eqr_for_table(dfeqr, dfeqr_stn)




#  ----------  comparison of shiny results with manual results ----------

#  ØKOKYST_2023_RSLA_RSL_Manuell utregning_mal.xlsx

df0 <- read.table("app/expected.txt", sep=";", header=T) %>%
  rename(stn_code=stn, value0=value, EQR0=EQR)

df1 <- dfeqr %>%
  select(stn_code, index, calc, value, EQR) %>%
  bind_rows(dfeqr_stn %>% select(stn_code, index, EQR) %>% mutate(calc="EQR"))

df0 <- df0 %>%
  left_join(df1, by=c("stn_code","calc"))
  
df0err <- df0 %>%
  filter(abs(EQR0-EQR) > 0.001)

