# 201902
# Pro Vaclava
# Vyzkum v chovu skotu
# -analyza a grafy
# sezona = "year"
# hloubky = d4 atd
# CESKY !!!
# grafy: w8 h4 dpi500 base size=15

# base --------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("readxl")
# install.packages("reshape2")
require(tidyverse)
require(readxl)
require(reshape2)


# PENRES ------------------------------------------------------------------

pen <- read_excel("data/vuchs20.xlsx", sheet = 1)

# pen$var <- as.factor(pen$var, levels = "1011", "1006", "1007", "1001", "1002")

pen$var[pen$var == 1001] <- "hnSOL"
pen$var[pen$var == 1002] <- "hnFIXSOL"
pen$var[pen$var == 1006] <- "hn"
pen$var[pen$var == 1007] <- "hnFIX"
pen$var[pen$var == 1011] <- "NPK"

pen$var <- factor(pen$var, levels = c("NPK", "hn", "hnFIX", "hnSOL", "hnFIXSOL"))

penl <- pen %>% 
  melt(id.vars = c("year", "var"), variable.name = ("depth"), value.name = "penres")

p18 <- pen %>% 
  filter(year == 2018)

p19 <- pen %>% 
  filter(year == 2019)

p18l <- p18 %>% 
  select(var, d4, d8, d12, d16, d20) %>% 
  melt(id.vars = c("var"), variable.name = ("depth"), value.name = "penres")

p19l <- p19 %>% 
  select(var, d4, d8, d12, d16, d20) %>% 
  melt(id.vars = c("var"), variable.name = ("depth"), value.name = "penres")

# penres explorative ------------------------------------------------------

dept <- c("20", "16", "12", "8", "4")

#  VUCHS both years in one plot
penl$depth <- factor(penl$depth, levels = c("d20", "d16", "d12", "d8", "d4"), labels = dept)

penl$year <- factor(penl$year, levels = c("2019", "2018"))

ggplot(penl, aes(depth, penres, fill=year))+
  geom_bar(aes(width = 0.5), stat = "summary", fun.y = "mean", 
           position = position_dodge())+
  scale_fill_manual(values=c("grey30", "darkgrey"), breaks = rev(levels(penl$year)))+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "\npenetrační odpor [MPa]", x = "hloubka [cm]", fill = "", title = "")+
  theme_classic(base_size = 15)+
  theme(legend.position="top")
ggsave("penres_both.png", path = "plots", device = "png", width = 8, height = 4, dpi = 500)

# separate plots for 2015 nad 2017
p15l$depth <- factor(p15l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)
p17l$depth <- factor(p17l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)

ggplot(p15l, aes(depth, penres))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "Penetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "2015")+
  theme_minimal()
ggsave("penres15.png", device = "png", width = 6, height = 3, dpi = 500)

ggplot(p17l, aes(depth, penres))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
  coord_flip()+
  facet_grid(. ~ var)+
  labs(y = "Penetration Resistance [MPa]", x = "Depth [cm]", fill = "", title = "2017")+
  theme_minimal()
ggsave("penres17.png", device = "png", width = 6, height = 3, dpi = 500)


# penres analysis ---------------------------------------------------------

# 2018

pairwise.wilcox.test(p18$d4, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$d8, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$d12, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$d16, p18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p18$d20, p18$var,
                     p.adjust.method = "BH")

# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

#2019

pairwise.wilcox.test(p19$d4, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$d8, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$d12, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$d16, p19$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(p19$d20, p19$var,
                     p.adjust.method = "BH")

# KFS ------------------------------------------------------------

kfs <- read_excel("data/vuchs20.xlsx", sheet = 3)

kfs$var[kfs$var == 1001] <- "hnSOL"
kfs$var[kfs$var == 1002] <- "hnFIXSOL"
kfs$var[kfs$var == 1006] <- "hn"
kfs$var[kfs$var == 1007] <- "hnFIX"
kfs$var[kfs$var == 1011] <- "NPK"

kfs$var <- factor(kfs$var, levels = c("NPK", "hn", "hnFIX", "hnSOL", "hnFIXSOL"))

# kfs explorative ---------------------------------------------------------

ggplot(kfs, aes(var, kfs))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
  scale_fill_manual(values=c("darkgrey"))+
  labs(y = expression("hydraulická vodivost [ mm."~ h^-1~"]"), 
       x = "", fill = "", title = "")+
  theme_classic(base_size = 15)
ggsave("kfs_both.png", path = "plots", device = "png", width = 8, height = 4, dpi = 500)

# kfs analysis ------------------------------------------------------------

pairwise.wilcox.test(kfs$kfs, kfs$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

# RBD ---------------------------------------------------------------------

rbd <- read_excel("data/vuchs20.xlsx", sheet = 2)

rbd$var[rbd$var == 1001] <- "hnSOL"
rbd$var[rbd$var == 1002] <- "hnFIXSOL"
rbd$var[rbd$var == 1006] <- "hn"
rbd$var[rbd$var == 1007] <- "hnFIX"
rbd$var[rbd$var == 1011] <- "NPK"

rbd$var <- factor(rbd$var, levels = c("NPK", "hn", "hnFIX", "hnSOL", "hnFIXSOL"))

rbd18 <- rbd %>% 
  filter(year == 2018)

rbd19 <- rbd %>% 
  filter(year == 2019)

# uprava tabulky pro analyzu var~year

hnSOL <- rbd %>% 
  filter(var == "hnSOL")

hnFIXSOL <- rbd %>% 
  filter(var == "hnFIXSOL")

hn <- rbd %>% 
  filter(var == "hn")

hnFIX <- rbd %>% 
  filter(var == "hnFIX")

NPK <- rbd %>% 
  filter(var == "NPK")

# rbd analysis ------------------------------------------------------------

pairwise.wilcox.test(rbd18$roh, rbd18$var,
                     p.adjust.method = "BH")

pairwise.wilcox.test(rbd19$roh, rbd18$var,
                     p.adjust.method = "BH")
# varianty mezirocne

pairwise.wilcox.test(hnSOL$roh, hnSOL$year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(hnFIXSOL$roh, hnFIXSOL$year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(hn$roh, hn$year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(hnFIX$roh, hnFIX$year,
                     p.adjust.method = "BH")

pairwise.wilcox.test(NPK$roh, NPK$year,
                     p.adjust.method = "BH")

# rbd <- rbd %>% 
#   melt(id.vars = c("var"), variable.name = ("year"), value.name = "rbd")

rbd$year <- as.factor(rbd$year)

ggplot(rbd, aes(var, roh, fill = year))+
  geom_bar(aes(width = 0.5), stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("darkgrey","grey30"))+
  labs(y = expression("objemová hmotnost [ g."~cm^-3~"]"), x = "", fill = "")+
  theme(legend.title=element_blank())+
  theme_classic(base_size = 15)
ggsave("rbd_bothyears.png", path = "plots", device = "png", width = 8, height = 4, dpi = 500)

# VUCHS NEPOCITA METEO -------------------------------------------------------------------

met <- read_excel("data/meteo.xlsx")
colnames(met) [1] <- "month"

met$month <- factor(met$month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

met <- met %>% 
  melt(id.vars = c("month"), variable.name = ("year"), value.name = "rain")

ggplot(met, aes(month, rain, fill=year))+
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_manual(values=c("grey90", "grey70", "grey50", "grey1"), 
                    name = "", 
                    labels = c("2015", "2016", "2017", 
                               "longterm normal (1981-2010)"))+
  labs(y = "Sum of Precipitation [mm]", x = "", fill = "")+
  theme_minimal()+
  theme(legend.position="top")
ggsave("meteo.png", device = "png", width = 6, height = 4, dpi = 500)

# VUCHS NEPOCITA UNITD -------------------------------------------------------------------

uni <- read_excel("data/unitd.xlsx")

uni$var[uni$var == 7] <- "ZF"
uni$var[uni$var == 8] <- "ZF_SOL"
uni$var[uni$var == 9] <- "C"
uni$var[uni$var == 10] <- "SOL"

unil <- uni %>% 
  select(seas, var, unitd)

u15 <- uni %>% 
  filter(seas == 2015)

u17 <- uni %>% 
  filter(seas == 2017)

u15l <- u15 %>% 
  select(var, unitd)

u17l <- u17 %>% 
  select(var, unitd)

# VUCHS NEPOCITA unitd explorative -------------------------------------------------------

# both years

unil$unitd <- unil$unitd*100
unil$seas <- factor(unil$seas)


ggplot(unil, aes(var, unitd, fill = seas))+
  geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean",
           position = position_dodge())+
  scale_fill_manual(values=c("darkgrey", "grey30"))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "")+
  theme_minimal()
ggsave("unitd_both_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# single year

unil$id <- paste(unil$seas, unil$var, sep="_") # vytvoreni sloupce ID ze sloupců 
unil$id <- factor(unil$id)

ggplot(unil, aes(x = var, y = unitd, fill = id)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
  ylim(0, 105)+
  theme_minimal() 
ggsave("unitd15_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# p15l$depth <- factor(p15l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)
# p17l$depth <- factor(p17l$depth, levels = c("cm20", "cm16", "cm12", "cm8", "cm4"), labels = dept)

u15l$unitd <- u15l$unitd * 100
u17l$unitd <- u17l$unitd * 100

# u15 issue - control mean is not 100 % - why?

u15$unitd <- u15$unitd * 100

u15 %>% 
  group_by(var) %>% 
  summarise(mean = mean(unitd)) %>% 
  ggplot(aes(x = var, y = mean)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
  ylim(0, 105)+
  theme_minimal() 

ggsave("unitd15_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

u17$unitd <- u17$unitd * 100

u17 %>% 
  group_by(var) %>% 
  summarise(mean = mean(unitd)) %>% 
  ggplot(aes(x = var, y = mean)) + 
  geom_col(aes(width = 0.5))+
  labs(y = "Unit Draft [%]", x = "", fill = "", title = "2017")+
  ylim(0, 120)+
  theme_minimal() 

ggsave("unitd17_percentage.png", device = "png", width = 6, height = 3, dpi = 500)

# ggplot(u15l, aes(var, unitd))+
#   geom_bar(aes(width = 0.5), stat = "summary", fun.y = "mean")+
#   labs(y = "Unit Draft [%]", x = "", fill = "", title = "2015")+
#   ylim(0.00, 130)+
#   theme_minimal()
# ggsave("unitd15.png", device = "png", width = 6, height = 3, dpi = 500)

# ggplot(u17l, aes(var, unitd))+
#   geom_bar(aes(width = 0.5),stat = "summary", fun.y = "mean")+
#   labs(y = "Unit Draft [%]", x = "", fill = "", title = "2017")+
#   ylim(0.00, 110)+
#   theme_minimal()
# ggsave("unitd17.png", device = "png", width = 6, height = 3, dpi = 500)

# VUCHS NEPOCITA unitd analysis ----------------------------------------------------------

pairwise.wilcox.test(u15$unitd, u15$var,
                     p.adjust.method = "BH")
# significant difference is where p-value < 0.05
# diff everywhere except Var 4 & control

pairwise.wilcox.test(u17$unitd, u17$var,
                     p.adjust.method = "BH")
