# BASE --------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)

#platf <- read_excel("data/200416_rape_complete.xlsx")
platf <- read_excel("data/200416_rape_outliers_HSV.xlsx")
lab <- read_excel("data/200416_lab.xls")

# CREATING FINAL TABLE ----------------------------------------------------

platf$id <- paste(platf$var,platf$plant, platf$leaf, sep="") # creates id based on var+plant+leaf; no separator
platfag <- aggregate(platf[, 5:22], list(platf$id), FUN = median) # agreggation / mean based on id
colnames(platfag)[1] <- "id" #renames the first column

platfag <- platfag %>% 
  select(id, spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas) # vyber jen relevantnich sloupcu

mer <- merge(platfag, lab, by = "id") # merge platfag and lab by id; 90 entries in total


# PLOTS -----------------------------------------------------------------

plot(mer$chab)
plot(mer$chabcm)
plot(mer$g)
plot(mer$G)
plot(mer$nas)
plot(mer$spadeq)

ggplot(platf, aes(var, spad))+
  geom_point()


platf %>% 
  group_by(var) %>% 
  summarise(minspad = min(spad),
            maxspad = max(spad))

# PLOTS RELATIONS ----------------------------------------------------------------

names(mer)

plot(mer$spad, mer$r)#not bad
plot(mer$spad, mer$g)#not bad
plot(mer$spad, mer$b)#nothing :D

plot(mer$spad, mer$R)
plot(mer$spad, mer$G)
plot(mer$spad, mer$B)

plot(mer$spad, mer$ExG)
plot(mer$spad, mer$ExG_n)

plot(mer$spad, mer$nas)

plot(mer$spad, mer$cha)
plot(mer$spad, mer$chb)
plot(mer$spad, mer$chab)

plot(mer$spad, mer$chacm)
plot(mer$spad, mer$chbcm)
plot(mer$spad, mer$chabcm)

plot(mer$chab, mer$r)
plot(mer$chab, mer$g)
plot(mer$chab, mer$b)

plot(mer$chab, mer$R)
plot(mer$chab, mer$G)
plot(mer$chab, mer$B)

plot(mer$chab, mer$ExG)
plot(mer$chab, mer$ExG_n)

# SPAD EQUATION -----------------------------------------------------------

mer <- mer %>% 
  mutate(spadeq = (99*mer$spad)/(144-mer$spad))

plot(mer$spadeq, mer$chab)
plot(mer$spadeq, mer$chabcm)


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
  select(spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas, 
         cha, chb, chab, car, chacm, chbcm, chabcm, carcm, spadeq)

install.packages("corrgram")
require(corrgram)

corm2 <- mer %>%  
  select(r, g, b, R, G, B, mean_rgb, ExG, ExG_n, nas, chab, chabcm, spad, spadeq)

corrgram(corm, lower.panel=panel.conf, upper.panel=NULL)
corrgram(corm2, lower.panel=panel.conf, upper.panel=NULL)

# MODEL -------------------------------------------------------------------

m1 <- lm(data = mer, chab ~ r + g + b + R + G + B) 
summary(m1)

m2 <- lm(data=mer, chab ~ R + G + B )
summary(m2)

m3 <- lm(data=mer, chab ~ R)
summary(m3)


# rozptyly ----------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)

platf <- read_excel("data/200416_rape_complete.xlsx")
platf$id <- paste(platf$var,platf$plant, platf$leaf, sep="")


ggplot(platf, aes(id, spad))+
             geom_point()

# RMSE --------------------------------------------------------------------

#install.packages("modelr")
require(modelr)

#rmse(model, data)

m1 <- lm(r ~ chab, data = mer)
rmse(m1, mer)

m2 <- lm(nas ~ chab, data = mer)
rmse(m2, mer)
