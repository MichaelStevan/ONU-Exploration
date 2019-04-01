library(data.table)
library(ggplot2)
library(factoextra)
library(ggplot2)
library(compareGroups)
library(rworldmap)
library(clValid)
library(NbClust)
library(Hmisc)

dt_lex <- fread("WPP2017_Period_Indicators_Medium.csv")
dt_lex <- dt_lex[,.(Location,Time, CNMR,LEx)]
dt_lex <- dt_lex[Location %in% c("Spain","United States of America","Argentina","Paraguay")]
dt_lex <- dt_lex[Time<=2050]

ggplot() + geom_line(data=dt_lex, aes(x=Time,y=LEx, col=Location,group=Location), size=1.5) +
  geom_point(data=dt_lex,aes(x=Time,y=LEx),size=3) +
  xlab("Year") + ylab("Saldo migratorio")+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.title.x = element_text(size=18)) +
  theme(axis.title.y = element_text(size=18)) +
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) +
  theme(legend.text=element_text(size=16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  scale_x_continuous(breaks=seq(1950, 2050, by = 10)) +
  #scale_y_continuous(breaks=seq(-5,15, by = 5)) +
  guides(linetype=F)+
  ggtitle("Saldo migratorio de países desarrollados (con alta esperanza de vida)")


## Get desired columns
dt_lex <- dt_lex[,.(Location,Time,LEx,CNMR,TFR)]
dt_lex <- dt_lex[Time=="1950-1955"]
dt_lex <- dt_lex[complete.cases(dt_lex),]
#dt_lex <- dt_lex[,.(Location,Time,LEx,TFR,CNMR)]

dt_lex[,Time:=apply(dt_lex[,.(Time)],1,function(x){as.numeric(unlist(strsplit(x,"-"))[1])})]
#dt_lex <- dt_lex[Time<="2050"]
#dt_lex <- dcast(dt_lex, Location ~ Time  , value.var = "LEx")


#dt_urb <- fread("Urbanization/WUP2018-F01-Total_Urban_Rural.csv")
dt_urb <- fread("Urbanization/WUP2018-F02-Proportion_Urban.csv",header=TRUE)
dt_urb <- dt_urb[,.(Location,UrbanPercent)]
dt_urb <- dt_urb[,.("Location","1950")]
year <- 1950
vect <- vector()

for (year in seq(1950,2050,by=5)){
  lex_year <- dt_lex[Time==year]
  lex_year <- lex_year[,.(Location,LEx,CNMR)]
  
 urb_year <- dt_urb[,.(Location)]
  urb <- dt_urb[,get(as.character(year))]
  urb_year[,Urb:=urb]
  #merge_year <- merge(lex_year,urb_year, by="Location")
  
  x <- lex_year[,LEx]
  y <- lex_year[,CNMR]
  
  res <- cor.test(x, y, method=c("spearman"))
  vect <- append(vect,res$estimate)
}

df <- data.frame()
df$Time <- seq(1950,2050,by=5)
df$LEx_Urb_Corr <- vect

dt <- merge(dt_lex,dt_urb, by="Location")

dtggplot(data=dt,aes(x=LEx,y=TFR)) + geom_point()


library("ggpubr")

x <- dt[,Urb]
y <- dt[,LEx]
cor(x, y, method = c("spearman"))
cor.test(x, y, method=c("pearson", "kendall", "spearman"))

ggscatter(dt, x = "LEx", y = "CNMR", 
          add = "reg.line", conf.int = TRUE, 
          #cor.coef = TRUE, cor.method = "pearson",
          xlab = "Esperanza de vida (1950)", ylab = "Saldo migratorio (1950)")

shapiro.test(x) 
# Shapiro-Wilk normality test for wt
shapiro.test(y)

ggqqplot(x, ylab = "Saldo Migratorio")

ggqqplot(y, ylab = "Esperanza de vida")

data <- dt[,.(LEx,UrbanPercent,TFR,CNMR,NetMigrations)]
res <- cor(data, method = "spearman")
rcorr(as.matrix(data), type ="spearman")
## Ha influido la urbanización de los países en el incremento de esperanza de vida?
## Estudiar si la correlación entre las dos variables ha ido aumentando
res <- cor.test(x, y, 
                method = "pearson")
res

library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

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

corr_time <- fread("Urbanization/corr_over_time.csv")

#corr_time <- corr_time[,.(Time,Lex_Urb_Corr)]
corr_time_current <- corr_time[Time==2015]
ggplot() + geom_line(data=corr_time,aes(x=Time,y=Lex_CNMR_Corr),size=1.5) +
  geom_point(data=corr_time_current,aes(x=Time,y=Lex_CNMR_Corr),size=4) +
  scale_x_continuous(breaks=seq(1950, 2050, by = 10)) +
  #scale_y_continuous(breaks=seq(min_lex,max_lex, by = 10)) 
  xlab("Year") + ylab("Spearman correlation value")+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.title.x = element_text(size=18)) +
  theme(axis.title.y = element_text(size=18)) +
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) +
  guides(linetype=F)+
  ggtitle("Correlación entre la esperanza de vida y el saldo migratorio a lo largo del tiempo")
  
  
names(corr_time) <- c("Time","Urbanización","Fertilidad", "Saldo migratorio")

melted <- melt(corr_time,id.vars="Time")
melted_current <- melted[Time==2015]
ggplot() + geom_line(data=melted, aes(x=Time,y=value, col=variable), size=1.5) +
  geom_point(data=melted_current,aes(x=Time,y=value),size=3) +
  xlab("Year") + ylab("Spearman correlation value")+
  theme(plot.title = element_text(face="bold", size=20))+
  theme(axis.title.x = element_text(size=18)) +
  theme(axis.title.y = element_text(size=18)) +
  theme(axis.text.x = element_text(size=18))+
  theme(axis.text.y = element_text(size=18)) +
  theme(legend.text=element_text(size=16)) +
  theme(legend.position="bottom", legend.box = "horizontal") +
  scale_x_continuous(breaks=seq(1950, 2050, by = 10)) +
  scale_y_continuous(breaks=seq(-1,1, by = 0.2)) +
  guides(linetype=F)+
  ggtitle("Correlación entre la esperanza de vida y otras variables a lo largo del tiempo")
