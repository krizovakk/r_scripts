# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")

require(tidyverse)
require(readxl)
require(reshape2)

dat <- read_excel("data/dat0.xlsx", sheet = 1)
names(dat)
dat <- dat %>% 
  select(var, plant, leaf, meas, spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas) # vyber jen relevantnich sloupcu

# ID and MEAN calculation before the analysis

dat$id <- paste(dat$var,dat$plant, dat$leaf, sep="_") # vytvoreni sloupce ID ze sloupců var+plant+leaf

datag <- aggregate(dat[, 5:20], list(dat$id), mean) # agregace / prumer na zaklade ID
colnames(datag)[1] <- "id" #renames the first column

# ggplots -----------------------------------------------------------------

ggplot(datag, aes(spad, r))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_r.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, g))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_g.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, b))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_b.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, R))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_R.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, G))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_G.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, B))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_B.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, ExG))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_ExG.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, ExG_n))+
  geom_smooth(method = "lm", color = "darkgreen")+
  geom_point(color = "darkgreen")+
  labs(x="SPAD value", y="ExG index / platform")+
  theme_minimal()
ggsave("spad_ExG_n_darkgreen.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, kawa))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_kawa.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, yuzhu))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_yuzhu.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, adam))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_adam.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, perez))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_perez.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, geor))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_geor.png", height = 6, width = 9, dpi = 300)  

ggplot(datag, aes(spad, nas))+
  geom_smooth(method = "lm")+
  geom_point()
ggsave("spad_nas.png", height = 6, width = 9, dpi = 300)  

# correlation -------------------------------------------------------------

corm <- datag %>%  
  select(spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas)

# install.packages("corrgram")
require(corrgram)

# corrgram(corm, lower.panel=panel.pts, upper.panel=panel.conf)
# corrgram(corm, lower.panel=panel.ellipse, upper.panel=panel.conf)
corrgram(corm, lower.panel=panel.conf, upper.panel=NULL)


# leaf 1 and 2 -----------------------------------------------

l1 <- dat %>% 
  filter(dat$leaf == 1) 

l2 <- dat %>% 
  filter(dat$leaf == 2)

l1ag <- aggregate(l1[, 5:20], list(l1$id), mean)

l2ag <- aggregate(l2[, 5:20], list(l2$id), mean)

corm1 <- l1ag %>%  
  select(spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas)
corrgram(corm1, lower.panel=panel.conf, upper.panel=NULL)

corm2 <- l2ag %>%  
  select(spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas)
corrgram(corm2, lower.panel=panel.conf, upper.panel=NULL)

# lab ---------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)
 
dat <- read_excel("data/outli.xlsx", sheet = 1)
names(dat)

dat <- dat %>%
  select(var, plant, leaf, meas, spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas)

dat$id <- paste(dat$var, dat$plant, dat$leaf, sep="_")

datag <- aggregate(dat[, 1:20], list(dat$id), mean)

colnames(datag)[1] <- "id" #renames the first column

lab <- read_excel("data/200416_lab.xls")
names(lab)

lab <- lab %>% 
  select(var, plant, leaf, cha, chb, chab, car, chacm, chbcm, chabcm, car_cm)

lab$id <- paste(lab$var, lab$plant, lab$leaf, sep="_")

mer <- merge(datag, lab, by = "id") # merge by id

names(mer)

mer <- mer %>% 
  select(id, spad, r, g, b, R, G, B, mean_rgb, ExG, ExG_n, kawa, yuzhu, adam, perez, geor, nas,
         cha, chb, chab, car, chacm, chbcm, chabcm, car_cm, leaf.y)

# prepocty




# cor analysis

# install.packages("corrgram")
require(corrgram)

# corrgram(corm, lower.panel=panel.pts, upper.panel=panel.conf)
# corrgram(corm, lower.panel=panel.ellipse, upper.panel=panel.conf)
corrgram(mer, lower.panel=panel.conf, upper.panel=NULL)

ggplot(mer, aes(mer$id, mer$chab))+
  geom_point()

# LEAF 1, LEAF 2

mer1 <- mer %>% 
  filter(mer$leaf == "1")
mer2 <- mer %>% 
  filter(mer$leaf == "2")

ggplot(mer1, aes(mer1$id, mer1$chab, color = mer1$var))+
  geom_point()

ggplot(mer1, aes(mer1$id, mer1$chabcm, color = mer1$var))+
  geom_point()

ggplot(mer1, aes(mer1$id, mer1$spad, color = mer1$var))+
  geom_point()

ggplot(mer2, aes(mer2$id, mer2$ExG_n, color = mer2$var))+
  geom_point()

corrgram(mer1, lower.panel=panel.conf, upper.panel=NULL)

corrgram(mer2, lower.panel=panel.conf, upper.panel=NULL)

# lm ----------------------------------------------------------------------

lm <- lm(data=mer, chabcm ~ r+ g+ b+ R+ G+ B+ mean_rgb+ 
           ExG+ ExG_n+ kawa+ yuzhu+ adam+ perez+ geor+ nas + leaf.y)
summary(lm)

lm4 <- lm(data=mer2, chab ~ r+ g+ b+ R+ G+ B+ mean_rgb+ 
            ExG+ ExG_n+ kawa+ yuzhu+ adam+ perez+ geor+ nas)
summary(lm4)

lm_check <- lm(data=mer1, chab ~ nas)
summary(lm_check) # 0,289

lm_check <- lm(data=mer1, chab ~ r + g + b)
summary(lm_check) # 0,33

lm_check <- lm(data=mer1, chab ~ R + G + B)
summary(lm_check) # 0,3936

lm_check <- lm(data=mer1, chab ~  r + g + b + R + G + B)
summary(lm_check) # 0,4033

lm_check <- lm(data=mer1, chab ~ r + g + b + R + G + B + mean_rgb)
summary(lm_check) # 0,4033

lm_check <- lm(data=mer1, chab ~ r + g + R + G + B)
summary(lm_check) # 0,4033

#leaf 2

lm_check <- lm(data=mer2, chab ~ nas)
summary(lm_check) # 0,289

lm_check <- lm(data=mer2, chab ~ r + g + b)
summary(lm_check) # 0,024

lm_check <- lm(data=mer2, chab ~ R + G + B)
summary(lm_check) # 0,1572

lm_check <- lm(data=mer2, chab ~  r + g + b + R + G + B)
summary(lm_check) # 0,2155

lm_check <- lm(data=mer2, chab ~ r + g + b + R + G + B + mean_rgb)
summary(lm_check) # 0,2155

# ttest / wilcox leaf 1

par(mfrow=c(2,2))
hist(mer1$spad)
hist(mer1$nas)

shapiro.test(mer1$spad)
shapiro.test(mer1$nas)

t.test(mer1$spad, mer1$nas, paired=T)
cor.test(mer1$spad, mer1$nas)
cor.test(mer1$chab, mer1$nas)
cor.test(mer1$chabcm, mer1$nas)

m1 <- lm(mer1$spad)

# PCA ---------------------------------------------------------------------

pca1 <- prcomp(mer[,3:17])
plot(pca1)

install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

ggbiplot(pca1)+geom_label(aes(label=mer$id))

mer2 <- subset(mer, !(id %in% c( "4_a_1", "2_e_2", "2_b_1", "2_h_2", "3_d_2", "4_g_2")))

lm2 <- lm(data=mer2, chabcm ~ r+ g+ b+ R+ G+ B+ mean_rgb+ ExG+ ExG_n+ kawa+ yuzhu+ adam+ perez+ geor+ nas)
summary(lm2)             

pca1 <- prcomp(mer2[,3:17])
plot(pca1)
ggbiplot(pca1)+geom_label(aes(label=mer2$id))
ggbiplot(pca1)

plot(mer2$mean_rgb, mer2$chabcm)
plot(mer2$perez, mer2$chabcm)
plot(mer2$spad, mer2$chabcm)
plot(mer2$r, mer2$chabcm)
plot(mer2$g, mer2$chabcm)

lm3 <- lm(data = mer2, chabcm ~ r + g + perez)
summary(lm3)
lm5 <- lm(data = mer2, chab ~ r + g + perez)
summary(lm5)


