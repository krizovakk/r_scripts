# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")

require(tidyverse)
require(readxl)
require(reshape2)

sb <- read_excel("red/linplat.xlsx") #sb = sugar beet
sb$n <- paste(sb$dose, sb$treat, sep = "_")

# sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","120_NPK2","160_NPK3", "200_NPK4",
#                           "105_FYM","185_FYM+NPK1","225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4"))

sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","105_FYM","120_NPK2","160_NPK3","185_FYM+NPK1","200_NPK4",
                          "225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4")) # ordered

sb$year <- as.factor(sb$year)

## TUBER

tube <- sb %>% 
  select(year, treat, dose, n, tuber)

## TOP

top <- sb %>% 
  select(year, treat, dose, n,  top)

# explorative -------------------------------------------------------------

ggplot(tube, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_ordered.png", path = "plots", height = 6, width = 9, dpi = 300)  

ggplot(top, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP", x = "", y = "Top yield (t ha-1)")
ggsave("top_ordered.png", path = "plots", height = 6, width = 9, dpi = 300)  


# linplat model -----------------------------------------------------------

# install.packages("easynls")
require(easynls)

## create dataframe for TUBER
df_tube <- data.frame(tube$dose, tube$tuber)

## create dataframe for TUBER
df_top <- data.frame(top$dose, top$top)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_tube, model=1) #linear
nlsplot(df_tube, model=2) #quadratic
nlsplot(df_tube, model=3) #linear-plateau

## RESULTS

nlsfit(df_tube, model=3)
nlsfit(df_top, model=3)

nlsplot(df_tube, model=3, xlab = "N dose [kg ha-1]", ylab = "tuber yield [t ha-1]")
nlsplot(df_top, model=3, xlab = "N dose [kg ha-1]", ylab = "top yield [t ha-1]")

# ggplot + linp -----------------------------------------------------------



# ***2ND ROUND*** --------------------------------------------------------------

## separate plots for NPK and FYM

# base --------------------------------------------------------------------

require(tidyverse)
require(readxl)
require(reshape2)

sb <- read_excel("red/linplat.xlsx") 
sb$n <- paste(sb$dose, sb$treat, sep = "_")

sb$year <- as.factor(sb$year)

# sb$n <- factor(sb$n, levels = c("0_Control","80_NPK1","105_FYM","120_NPK2","160_NPK3","185_FYM+NPK1","200_NPK4",
#                                 "225_FYM+NPK2","265_FYM+NPK3","305_FYM+NPK4")) 

## separate data for NPK

tar_npk <- c("Control", "NPK1", "NPK2", "NPK3", "NPK4") # for filtering NPK
n_npk <- c("0_Control", "80_NPK1", "120_NPK2",
           "160_NPK3", "200_NPK4") # levels of n factor

npk <- sb %>% 
  filter(treat %in% tar_npk)

## separate data for FYM

tar_fym <- c("Control", "FYM", "FYM+NPK1", "FYM+NPK2", "FYM+NPK3", "FYM+NPK4")
n_fym <- c("0_Control", "105_FYM", "185_FYM+NPK1", "225_FYM+NPK2", 
           "265_FYM+NPK3", "305_FYM+NPK4")

fym <- sb %>% 
  filter(treat %in% tar_fym)

## TUBER

tube_npk <- npk %>% 
  select(year, treat, dose, n, tuber)
tube_npk$n <- factor(tube_npk$n, 
                     levels = n_npk)

tube_fym <- fym %>% 
  select(year, treat, dose, n, tuber)
tube_fym$n <- factor(tube_fym$n, levels = n_fym)

## TOP

top_npk <- npk %>% 
  select(year, treat, dose, n,  top)
top_npk$n <- factor(top_npk$n, levels = n_npk)

top_fym <- fym %>% 
  select(year, treat, dose, n,  top)
top_fym$n <- factor(top_fym$n, levels = n_fym)

# explorative -------------------------------------------------------------

##  TUBER

ggplot(tube_npk, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER / NPK", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_npk.png", path = "plots", height = 6, width = 9, dpi = 300) 

ggplot(tube_fym, aes(n, tuber, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TUBER / FYM", x = "", y = "Tuber yield (t ha-1)")
ggsave("tuber_fym.png", path = "plots", height = 6, width = 9, dpi = 300) 

## TOP

ggplot(top_npk, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP / NPK", x = "", y = "Top yield (t ha-1)")
ggsave("top_npk.png", path = "plots", height = 6, width = 9, dpi = 300)  

ggplot(top_fym, aes(n, top, colour=year))+
  geom_point(size = 3)+
  theme_minimal(base_size = 15)+
  theme(axis.text.x = element_text(angle = 45, hjust = 0.5, vjust = 0.5))+
  labs(title = "TOP / FYM", x = "", y = "Top yield (t ha-1)")
ggsave("top_fym.png", path = "plots", height = 6, width = 9, dpi = 300)  


# linplat model -----------------------------------------------------------

#install.packages("easynls")
require(easynls)

## create dataframes for linplat model

df_tube_npk<- data.frame(tube_npk$dose, tube_npk$tuber) #TUBER / NPK

df_tube_fym <- data.frame(tube_fym$dose, tube_fym$tuber) #TUBER / FYM

df_top_npk <- data.frame(top_npk$dose, top_npk$top) # TOP / NPK

df_top_fym <- data.frame(top_fym$dose, top_fym$top) # TOP / FYM

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_tuber, model=1) #linear
nlsplot(df_tuber, model=2) #quadratic
nlsplot(df_tuber, model=3) #linear-plateau

## RESULTS

nlsfit(df_tube_npk, model=3)
nlsfit(df_tube_fym, model=3)

nlsfit(df_top_npk, model=3)
nlsfit(df_top_fym, model=3)

nlsplot(df_tube_fym, model=3, xlab = "N dose [kg ha-1]", 
        ylab = "tuber yield [t ha-1]")
nlsplot(df_top_fym, model=3, xlab = "N dose [kg ha-1]", 
        ylab = "top yield [t ha-1]")

nlsplot(df_top_fym, model=3, 
        start = c(a = 17.4, b = 0.0339, c = 180.8333), 
        xlab = "Explanatory Variable" , 
        ylab = "Response Variable", position = 1) # might be the path

# linplat manual ----------------------------------------------------------

## https://rcompanion.org/handbook/I_11.html

#install.packages("rcompanion") # quite long installation, 5 minutes
library(rcompanion) # for plot part

## TOP NPK / done

fit.lm    = lm(top ~ dose, data=top_npk) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(top_npk$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_topnpk = nls(top ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = top_npk,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_topnpk)

# par(oma = c(1,0,0,0))
plotPredy(data  = top_npk,
          x     = dose,
          y     = top,
          model = model_topnpk,
          main  = "NPK",
          xlab  = "N dose [kg.ha-1]",
          ylab  = "top yield [t.ha-1]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
# axis(1, at = seq(0, 220, by = 10), las=2)
axis(1, at = top_npk$dose, labels = top_npk$dose, 
     las = 1, cex.axis = 1.1)
#text(40,33, "y = 17.4037+0.0639(x-122.2971)", col = "blue", cex=0.9)
mtext("y = 17.4037+0.0639(x-122.2971)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")

## TOP FYM /// TO BE SOLVED LATER

fit.lm    = lm(top ~ dose, data=top_fym) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(top_fym$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_topfym = nls(top ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = top_fym,
            # start = list(a   = a.ini,
            #              b   = b.ini,
            #              clx = clx.ini),
            start = list(a = 17.4, b = 0.039, clx = 180.8333),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_topfym)

plotPredy(data  = top_fym,
          x     = dose,
          y     = top,
          model = model_topfym,
          xlab  = "FYM / N dose [kg.ha-1]",
          ylab  = "top yield [t.ha-1]")

## TUBE FYM / done

fit.lm    = lm(tuber ~ dose, data=tube_fym) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(tube_fym$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_tubefym = nls(tuber ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = tube_fym,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_tubefym)

# par(oma = c(3,0,0,0))
plotPredy(data  = tube_fym,
          x     = dose,
          y     = tuber,
          model = model_tubefym,
          main  = "FYM",
          xlab  = "N dose [kg.ha-1]",
          ylab  = "tuber yield [t.ha-1]",
          xaxt  = "n", 
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
axis(1, at = tube_fym$dose, labels = tube_fym$dose, 
     las = 1, cex.axis = 1.1)
# text(105,50, "y = 52.9120+0.0789(x-165.4596)", col = "blue", cex=0.9)
mtext("y = 52.9120+0.0789(x-165.4596)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")

## TUBE NPK / done

fit.lm    = lm(tuber ~ dose, data=tube_npk) # Find reasonable initial values for parameters

a.ini     = fit.lm$coefficients[1] # fixed
b.ini     = fit.lm$coefficients[2] # fixed
clx.ini   = mean(tube_npk$dose)

linplat = function(x, a, b, clx) # fixed, Define linear plateau function
{ifelse(x < clx, a + b * x,
        a + b * clx)}

model_tubenpk = nls(tuber ~ linplat(dose, a, b, clx), # Find best fit parameters
            data = tube_npk,
            start = list(a   = a.ini,
                         b   = b.ini,
                         clx = clx.ini),
            trace = FALSE,
            nls.control(maxiter = 1000))

summary(model_tubenpk)

# par(mar = c(6,0,0,0))
# par(oma = c(1,0,0,0))
plotPredy(data  = tube_npk,
          x     = dose,
          y     = tuber,
          model = model_tubenpk,
          main  = "NPK",
          xlab  = "N dose [kg.ha-1]", 
          ylab  = "tuber yield [t.ha-1]",
          xaxt  = "n",
          cex   = 1.3,
          cex.lab=1.3, 
          cex.axis=1.2, 
          cex.main=1.3)
axis(1, at = tube_npk$dose, labels = tube_npk$dose, 
     las = 1, cex.axis = 1.1)
# text(140,55, "y = 52.9120+0.1046(x-111.5920)", col = "blue", cex=0.9)
mtext("y = 52.9120+0.1046(x-111.5920)", side = 3, line = 0,
      outer = FALSE, cex = 1, col = "blue")
 
# TOPFYM issue ------------------------------------------------------------

install.packages("FertBoot")
require(FertBoot)

df_top_fym <- data.frame(top_fym$dose, top_fym$top) 

f_mod <- f.linear.plateau(
  df_top_fym,
  start = list(a = 17.4, b = 0.039, c = 180.833),
  plus_minus = 10,
  n.start = 1000,
  msg = FALSE)

summary(f_mod$nls.model)


# ggplots -------------------------------------------------------------------

# tips --------------------------------------------------------------------

install.packages("minpack.lm")
library(minpack.lm)
library(ggplot2)

linplat2 <- function(expr(y=a+b(x-clx)))


ggplot(top_fym, aes(x=dose, y=top)) +
  geom_point()+
  geom_smooth(method="nlsLM",
              formula= linplat,
              method.args=list(start=c(a=17.4,
                                      b=0.06399,
                                      clx=122.29715)))


ggplot(top_fym, aes(x=dose, y=top)) +
  geom_point(size = 0.5) +
  geom_smooth(method = "nlsLM", se=FALSE,
              formula=y~17.4+0.06399(x-122.29715))


ggplot(dd_, aes(n_trt, yield)) +
  geom_point(size = 0.5) +
  geom_smooth(method="nlsLM",
              se=FALSE,
              formula=y~quadratic.plateau(A,B,C, D, x),
              method.args=list(start=list(A=2.005904,
                                          B=0.03158664,
                                          C=-0.0001082836, 
                                          D = 145.8515 )))

library(ggplot2)
library(minpack.lm)
d <- ggplot(test,aes(x=t, y=fold))+ 
  geom_point()+
  geom_smooth(method="nlsLM", 
              formula=y~1+Vmax*(1-exp(-x/tau)), 
              method.args = list(start=c(tau=0.2,Vmax=2)), 
              se=FALSE)
