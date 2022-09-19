library(readr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(GGally)
library(highcharter)
library(lattice)
library(caret)
library(treemap)
library(highcharter)
library (rpart)
library(rpart.plot)
library(tidyverse)
library(hexbin)
library(energy)
library(corrplot)
library(dHSIC)
library(minerva)
library(sjstats)
library(rstatix)
library(nnet)
library(e1071)
library(NeuralNetTools)

base <- read.csv(file = "C://Users//vicky//OneDrive//Documents//PREDICTIVA//TP1Vallejo//CardFraud.csv")

df <- base %>% mutate(step = as.factor(step), type = as.factor(type),isFraud = as.factor(isFraud), isFlaggedFraud = as.factor(isFlaggedFraud))
dim(base)
# Chequeo si hay valores faltantes
base %>% is.na() %>% sum()
# No hay missing values

# EDA
str(df)

df %>% group_by(isFraud) %>%
  summarise(cant = n()) %>%
  mutate(freq = round(cant / sum(cant), 5)) %>% 
  arrange(desc(freq))

# Bar de frecuencia de fraude
df %>% ggplot(aes(x = isFraud, y = (..count..))) +  
  geom_bar(fill =  c( "#FFA500", "#FBCEB1" ) , stat = "count")+
  geom_label(stat='count',aes(label=  paste0(round(((..count..)/sum(..count..)),4)*100,"%" ) ) )+  # para el porcentaje
  labs(x = "Fraude o No", y = "Frecuencia", title = "Frequencia de fraude ") # labels


transac <- df %>% group_by(type) %>% count(type) %>% arrange(desc(n))
tmap <- treemap(transac,index="type", title = "Transacciones por tipo de medio",palette = "RdYlGn",vSize="n")
#con los números al pararse sobre cada "type"
hctreemap(tmap, allowDrillToNode = TRUE) %>% hc_title(text = "Cantidad de transacciones por tipo de medio ") %>% hc_exporting(enabled = TRUE)

# Medio de transacción x Fraude
total_fraud <- df %>% filter(isFraud == 1) %>% select(type) %>% group_by(type) %>% count(type) %>% arrange(desc(n)) # aquellos con fraude
trmf <- treemap(total_fraud,index="type", title = "Transacciones con fraude por tipo de medio",palette = "RdGy",vSize="n")
hctreemap(trmf, allowDrillToNode = TRUE) %>% hc_title(text = "Transacciones fraude por tipo de medio") %>% hc_exporting(enabled = TRUE)

# Al no existir fraudes de "CASH_IN", "DEBIT", "PAYMENT", elimino aquellos registros que no pertenezcan a "CASH_OUT" o "TRANSFER"
dfsin = df %>% filter(!(type %in% c("CASH_IN", "DEBIT", "PAYMENT")))
dim (dfsin)
# Medio de transacción x SIN Fraude
total_fraud <- dfsin %>% filter(isFraud == 0) %>% select(type) %>% group_by(type) %>% count(type) %>% arrange(desc(n))
trmf <- treemap(total_fraud,index="type", title = "Transacciones sin fraude por tipo de medio sin fraude",palette = "RdYlGn",vSize="n")
hctreemap(trmf, allowDrillToNode = TRUE) %>% hc_title(text = "Transacciones sin fraude por tipo de medio sin fraude") %>% hc_exporting(enabled = TRUE)

# Medio de transacción x CON Fraude
total_fraud <- dfsin %>% filter(isFraud == 1) %>% select(type) %>% group_by(type) %>% count(type) %>% arrange(desc(n))
trmf <- treemap(total_fraud,index="type", title = "Transacciones con fraude por tipo de medio con fraude",palette = "RdGy",vSize="n")
hctreemap(trmf, allowDrillToNode = TRUE) %>% hc_title(text = "Transacciones con fraude por tipo de medio con fraude") %>% hc_exporting(enabled = TRUE)

# Sin fraude presenta mayor registro de cash_out, con fraude presenta una frecuencia casi idéntica
# entre cash_out y transfer

# Plot "Amount" Fraude vs Sin Fraude
dfsin %>% ggplot(aes(x = log(amount,10), y = "isFraud" , fill = isFraud)) + geom_violin() + coord_flip() + 
  scale_fill_manual(values=c("#FF0000", "#F2D2BD") , name="Media", labels=c( "Sin Fraude","Fraude"))+
  labs(x = "Amount (escala log)", y = "", title = "Monto de transacciones", subtitle = "Fraude vs No Fraude" ) +
  stat_summary(fun.y = "mean",geom = "point", size = 2, col =c("#FF0000", "#F2D2BD"))+
  theme_classic()

# amount difiere en fraude y no fraude. 
# Amount Sin Fraude se encuentran aprox cerca de 100000
# Amount Con Faude se distribuyen + uniforme, 
# la variable "Amount" puede contribuir al modelo.

# Chi cuadrado, discretizando variable
# que sea fraude es independiente o no de tener un monto mayor a 100000
# H0: fraude <= 100000
# Ha: fraude > 100000
a = table (df$isFraud,df$amount>100000) # se discretiza la variable numerica
chisq.test(a)
plot(a)
# p value chico < 0,05 rechazo h0

# Cramer's V (chi-square) 
tab = table(df_numc$isFraud, df_numc$amount)
as.matrix(tab) %>% rstatix::cramer_v()

# Base numerica
df_num = df  [,c(3,5,6,8,9,10)]

# achico base
df_numc= df_num %>% sample_n(floor(nrow(df)*.5))

#Pairplot
df_numeric = df [,c(3,5,6,8,9)]
df_cor = sample_n(df_numeric, 100000)
corplot (df_cor)

# MODELOS
# Partición en T y T
set.seed(8) ; particion= createDataPartition(y=df_numc$isFraud, p=0.8, list=FALSE)
Train <- df_numc[particion,]
Test <- df_numc[-particion,]

#AdD
arbol=rpart(isFraud~.,Train,method="class")
rpart.plot(arbol,extra=1,type=5,cex=0.8 )

pred=predict(arbol,Test,type="class")
confusionMatrix(pred,Test$isFraud)

# Regresión Logística
model = glm (isFraud~.,data=Train, family = "binomial")
summary (model)

set.seed (2) ; i <- sample(2,nrow(df_numc),prob=c(0.8,0.2),replace= TRUE)
Train2 <- df_numc[i==1,]
Test2 <- df_numc[i==2,]
reg = glm(isFraud~.,data=df_numc, family = binomial )
# Voy a fittear
pre = predict(reg , type ="response")
pre[1:10]

p=rep("0",635000) 
p[pre >.5]="1" # si la probabilidad de ser frause es > al 50%

table(p,df_numc$isFraud)
confusionMatrix(p,df_numc$isFraud)
Accuracy = (634058 +2070) / (634058 +2070+404+ 847)




