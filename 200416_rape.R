# BASE --------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)

platf <- read_excel("data/200416_rape_outliers_HSV.xlsx")
lab <- read_excel("data/200416_lab.xls")

# CREATING FINAL TABLE ----------------------------------------------------

platf$id <- paste(platf$var,platf$plant, platf$leaf, sep="") # creates id based on var+plant+leaf; no separator
platfag <- aggregate(platf[, 5:39], list(platf$id), FUN = median) # agreggation / median based on id
colnames(platfag)[1] <- "id" #renames the first column

platfag <- platfag %>% 
  select(id, spad, R, G, B, mean_rgb, r, g, b, 
         cmax, cmin, c, hue, saturation , brightness, Y, Cb, Cr, 
         GMR, GDR, VI, DGCI, NRI, NGI,
         ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas) # vyber jen relevantnich sloupcu


mer <- merge(platfag, lab, by = "id") # merge platfag and lab by id; 90 entries in total

# SPAD EQUATION -----------------------------------------------------------

mer <- mer %>% 
  mutate(spadeq = (99*mer$spad)/(144-mer$spad))

# LOGSIG ? ----------------------------------------------------------------

#install.packages("sigmoid")
require(sigmoid)

mer$rabs <- mer$r*255
mer$gabs <- mer$g*255
mer$babs <- mer$b*255

mer <- mer %>% 
  mutate(logsig = sigmoid((gabs-(rabs/3)-(babs/3))/255))

cor.test(mer$chab, mer$logsig) #0,36
cor.test(mer$chabcm, mer$logsig) #0,38


# CORRELATION -------------------------------------------------------------

corm <- mer %>%  
  select(spad, spadeq, R, G, B, mean_rgb, r, g, b, 
         cmax, cmin, c, hue, saturation , brightness, Y, Cb, Cr, 
         GMR, GDR, VI, DGCI, NRI, NGI,
         ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas, 
         cha, chb, chab, car, chacm, chbcm, chabcm, carcm)

#install.packages("corrgram")
require(corrgram)

corrgram(corm, lower.panel=panel.conf, upper.panel=NULL)

# MODEL -------------------------------------------------------------------

m1 <- lm(data = mer, chab ~ r + g + b + R + G + B) 
summary(m1)

m2 <- lm(data=mer, chab ~ R + G + B )
summary(m2)

m3 <- lm(data=mer, chab ~ R)
summary(m3)
