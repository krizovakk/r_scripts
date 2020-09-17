# base --------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)
require(corrgram)

rplatf <- read_excel("data/200416_platf_spad.xlsx") #rapeseed, 16th April 2020; platform+spad results; n=948
rlab <-  read_excel("data/200416_lab.xls") #rapeseed, 16th April 2020; lab results; n=90
wplatf <- read_excel("data/200618_platf_spad.xlsx") #wheat; 18th June 2020; platform+spad results; n=900
wlab <- read_excel("data/200618_lab.xls") #wheat; 18th June 2020; lab results; n=90

# final table -------------------------------------------------------------

# rape

rplatf$id <- paste(rplatf$var, rplatf$plant, rplatf$leaf, sep = "")
rplatfag <- aggregate(rplatf[, 5:38], list(rplatf$id), FUN = median) # 5:39 <-- agg 5th to 39th column
colnames(rplatfag)[1] <- "id"
rmer <- merge(rplatfag, rlab, by = "id") # merged table for rapeseed, n=90

# wheat

wplatf$id <- paste(wplatf$var, wplatf$plant, sep = "")
wplatfag <- aggregate(wplatf[, 4:37], list(wplatf$id), FUN = median)
colnames(wplatfag)[1] <- "id"
wmer <- merge(wplatfag, wlab, by = "id") # merged table for wheat, n=90

