# upload and stack renal reg files

library(data.table)
library(readxl)
library(tidyverse)

setwd("D:/Data integration/Databases/Renal Registry/Data/")

test <- read_xlsx("test.xlsx",sheet=1)

# read in datasets

DT_JUl04_Oct18 <- read_xlsx("Jul04 - Oct18 modified.xlsx",sheet = 1)

DT_JUl04_Oct18$DVA <- as.character(DT_JUl04_Oct18$DVA)
DT_JUl04_Oct18$PostCode <- as.character(DT_JUl04_Oct18$PostCode)
DT_JUl04_Oct18$Sex <- as.character(DT_JUl04_Oct18$Sex)

DT_Nov18 <- ?read_xlsx("Mar19.xlsx",sheet = 1)
DT_Dec18 <- read_xlsx("Dec18 modified.xlsx",sheet = 1)

DT_Jan19 <- read_xlsx("Jan19 modified.xlsx",sheet = 1)
DT_Feb19 <- read_xlsx("Feb19 modified.xlsx",sheet = 1)
DT_Mar19 <- read_xlsx("Mar19 modified.xlsx",sheet = 1)


# review data details (classes, str etc)
data_details(DT_Mar19)
data_details(DT_Nov18)
data_details(DT_Dec18)
data_details(DT_Feb19)
data_details(DT_JUl04_Oct18)


# combine datasets
dim(DT_Jan19)

# stack all the dataset together
concat_ds <- bind_rows(list(DT_JUl04_Oct18, DT_Nov18, DT_Dec18, DT_Jan19,DT_Feb19, DT_Mar19))

test <- as.data.frame(concat_ds)

WriteXLS(test,"Renal_Register Jul04-Mar19.xlsx",   AdjWidth = TRUE, BoldHeaderRow = TRUE)

library("xlsx")
