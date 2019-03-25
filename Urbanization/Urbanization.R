library(data.table)
library(ggplot2)
library(factoextra)
library(ggplot2)
library(compareGroups)
library(rworldmap)
library(clValid)
library(NbClust)

dt_lex <- fread("WPP2017_Period_Indicators_Medium.csv")

## Get desired columns
dt_lex <- dt_lex[,.(Location,Time,LEx)]
dt_lex <- dt_lex[Time=="1950-1955"]
dt_lex <- dt_lex[complete.cases(dt_lex),]
dt_lex <- dt_lex[,.(Location,LEx)]


dt_urb <- fread("Urbanization/WUP2018-F01-Total_Urban_Rural.csv")
dt_urb <- dt_urb[,.(Location,Percent1955)]


dt <- merge(dt_lex,dt_urb, by="Location")

ggplot(data=dt,aes(x=LEx,y=Percent1955)) + geom_point()


library("ggpubr")

x <- dt[,LEx]
y <- dt[,Percent1955]
cor(x, y, method = c("pearson", "kendall", "spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

ggscatter(dt, x = "LEx", y = "Percent1955", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")

shapiro.test(x) # => p = 0.1229
# Shapiro-Wilk normality test for wt
shapiro.test(y)

ggqqplot(x, ylab = "MPG")

ggqqplot(y, ylab = "WT")

## Ha influido la urbanización de los países en el incremento de esperanza de vida?
## Estudiar si la correlación entre las dos variables ha ido aumentando
res <- cor.test(x, y, 
                method = "pearson")
res

## Corr Actualidad 0.63
## Corr 1955 0.75
## Esto confirma que antiguamente vivir en una ciudad era más influyente en la Esperanza de vida
## Ahora el desarrollo global y la influencia de otros factores que también han afectado a zonas 
## rurales (medicina,tecnología,..) han hecho que la relación entre ambas variables sea un poco menor
## aunque sigue siendo bastante alta.

rows_to_remove = c("WORLD","AFRICA","Eastern Africa","Middle Africa",
                   "Northern Africa", "Southern Africa","Western Africa",
                   "ASIA","Eastern Asia","South-Central Asia","Central Asia", "Southern Asia",
                   "South-Eastern Asia", "Western Asia",
                   "EUROPE","Eastern Europe","Northern Europe",
                   "Southern Europe","Western Europe",
                   "Caribbean", "LATIN AMERICA AND THE CARIBBEAN", "Central America",
                   "South America","NORTHERN AMERICA","OCEANIA",
                   "Australia/New Zealand","Melanesia","Micronesia","Polynesia",
                   "More developed regions","Less developed regions","Least developed countries",
                   "Less developed regions, excluding least developed countries",
                   "Less developed regions, excluding China", "High-income countries",
                   "Middle-income countries","Upper-middle-income countries",
                   "Lower-middle-income countries", "Low-income countries",
                   "Sub-Saharan Africa"
)


#dt_current <- dt_current[!Location %in% rows_to_remove]

