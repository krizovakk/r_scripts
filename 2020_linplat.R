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
tube$year <- as.factor(tube$year)

## TOP

top <- sb %>% 
  select(year, treat, dose, n,  top)
top$year <- as.factor(top$year)

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
df_tuber <- data.frame(tube$dose, tube$tuber)

## create dataframe for TUBER
df_top <- data.frame(top$dose, top$top)

## examples https://rdrr.io/cran/easynls/man/nlsplot.html

nlsplot(df_tuber, model=1) #linear
nlsplot(df_tuber, model=2) #quadratic
nlsplot(df_tuber, model=3) #linear-plateau

## RESULTS

nlsfit(df_tuber, model=3)
nlsfit(df_top, model=3)

nlsplot(df_tuber, model=3, xlab = "N dose [kg ha-1]", ylab = "tuber yield [t ha-1]")
nlsplot(df_top, model=3, xlab = "N dose [kg ha-1]", ylab = "top yield [t ha-1]")


# ggplot + linp -----------------------------------------------------------




# tips --------------------------------------------------------------------

ggplot(dd_, aes(n_trt, yield)) +
  geom_point(size = 0.5) +
  geom_smooth(method="nlsLM",
              se=FALSE,
              formula=y~quadratic.plateau(A,B,C, D, x),
              method.args=list(start=list(A=2.005904,
                                          B=0.03158664,
                                          C=-0.0001082836, 
                                          D = 145.8515 )))




