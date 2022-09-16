
if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(splitTools)){
  install.packages("splitTools")
  library(splitTools)
}
if(!require(pROC)){
  install.packages("pROC")
  library(pROC)
}



## Ustaw ziarno losowania, np. 12345
set.seed(1394)
## Wczytaj dane, sprawdź czy są braki danych i dokonaj przekształcenia wartości ostatniej zmiennej z {1,2} na {0,1}.


ad <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric")
class(ad)

names(ad)<-c(paste("V",1:24,sep=""),"class") 
ad<- ad %>%
  mutate(class = ifelse(class == 1, 0, 1)) 
if(sum(is.na(ad))==0) {
  print("Wszsytkie dane są obecne")} else {
    print("Mamy braki...")}
ad
class(ad)

## Dokonaj losowego podziału zbioru na zbiory: uczący, walidacyjny i testowy w proporcjach odpowiednio 40%, 30% i 30%.

zbiory<-sample(seq(1,3),size=nrow(ad),replace=TRUE,prob=c(0.4,0.3,0.3))
Train<-ad[zbiory==1,]
Test<-ad[zbiory==2,]
Validate<-ad[zbiory==3,]

## Zbuduj dwa różne modele wykorzystując metodę regresji logistycznej
model1<-glm(class~.,data=Train,family=binomial) 
model2<-glm(class~ V1 + V2 + V3 + V4 + V5, data=Train, family=binomial)

##Zaprezentuj zdolność predykcyjną obu modeli na zbiorze uczącym i walidacyjnym 
##wykresem ROC i wyznacz wartość statystyki AUC.
pred1<- model1 %>% predict(Test, type="response")
summary(pred1)
pred1 <- ifelse(pred1 > 0.5,1,0)
test_roc1 = roc(Test$class ~ pred1, plot = TRUE, print.auc = FALSE)
print("Predykcja testu1:")
test_roc1$auc

pred2<- model1 %>% predict(Validate, type="response")
summary(pred2)
pred2 <- ifelse(pred2 > 0.5,1,0)
val_roc1 = roc(Validate$class ~ pred2, plot = TRUE, print.auc = FALSE, add=TRUE)
print("Predykcja validacji1:")
val_roc1$auc

pred3<- model2 %>% predict(Test, type="response")
summary(pred3)
pred3 <- ifelse(pred3 > 0.5,1,0)
test_roc2 = roc(Test$class ~ pred3, plot = TRUE, print.auc = FALSE, add=TRUE)

print("Predykcja testu2:")
test_roc2$auc

pred4<- model2 %>% predict(Validate, type="response")
summary(pred4)
pred4 <- ifelse(pred4 > 0.5,1,0)
val_roc2 = roc(Validate$class ~ pred4, plot = TRUE, print.auc = FALSE, add=TRUE)

print("Predykcja testu2:")
val_roc2$auc

NAJLEPSZY = roc(Validate$class ~ pred2, plot = TRUE, print.auc = TRUE, add=FALSE)

## Zadanie ostatnie

roc2<- ci.thresholds(val_roc1,
              thresholds=seq(0.5, 0.9, 0.01),
              boot.n=10000, conf.level=0.9, stratified=TRUE)
val_roc1
plot(roc2)

a<- sum(coords(test_roc1, ret="fn"))*1000
b<-sum(coords(test_roc1, ret="fp"))*150
print("Kosz złych decyzji w zł")
suma(a+b)


