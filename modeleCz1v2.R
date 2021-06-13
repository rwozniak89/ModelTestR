load("wszystkieOferty080621.RData")
View(wszystkieOferty080621)
summary(wszystkieOferty080621)
str(wszystkieOferty080621)
library(tidyverse)

#install.packages("sjmisc")
library(sjmisc)
str_contains(wszystkieOferty080621$cena,",",logic=NULL)
str_contains(wszystkieOferty080621$`Pojemność skokowa`,",",logic=NULL)
str_contains(wszystkieOferty080621$`Moc`,",",logic=NULL)
str_contains(wszystkieOferty080621$`Rok produkcji`,",",logic=NULL)
?str_contains

#to nie dziala jakbysmy chcieli:as.numeric(wszystkieOferty080621$cena)

names(wszystkieOferty080621)<-gsub(" ","_",names(wszystkieOferty080621))
summary(wszystkieOferty080621)
str(wszystkieOferty080621)
names(wszystkieOferty)
nrow(wszystkieOferty)

#dplyr::mutate
wszystkieOferty <- wszystkieOferty080621%>%dplyr::mutate(cena=   as.numeric(cena%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) ) 

wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Przebieg=as.numeric(Przebieg%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Rok_produkcji=as.numeric(Rok_produkcji%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Moc=as.numeric(Moc%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
#wszystkieOferty<-wszystkieOferty%>%dplyr::mutate(Pojemność_skokowa= substr(Pojemność_skokowa,1,str_length(Pojemność_skokowa)-1) )
#wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Pojemność_skokowa=as.numeric(Pojemność_skokowa%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Pojemność_skokowa=(Pojemność_skokowa%>%str_replace_all("cm3","")%>%str_replace_all("[^\\d,]","") ))
wszystkieOferty<- wszystkieOferty%>%dplyr::mutate(Pojemność_skokowa=as.numeric(Pojemność_skokowa))
#wszystkieOfertyTest<- wszystkieOferty%>%dplyr::filter(waluta=="PLN", (Bezwypadkowy=='Tak' | is.na(Bezwypadkowy))  )
wszystkieOferty<-wszystkieOferty%>%dplyr::filter(waluta=="PLN",Uszkodzony!="Tak" | is.na(Uszkodzony) )
str(wszystkieOferty)
summary(wszystkieOferty)

nrow(wszystkieOferty)


#dplyr::select
wszystkieOferty<-wszystkieOferty%>%dplyr::select(-waluta,-Kategoria)
wszystkieOferty<-wszystkieOferty%>%dplyr::select(cena:Kolor,Stan)
summary(wszystkieOferty)


ofertyV<-wszystkieOferty%>%dplyr::filter(Marka_pojazdu=='Volkswagen')
str(ofertyV)
#dplyr::dplyr::arrange
#dplyr::group_by
#dplyr::summarise
wszystkieOferty%>%dplyr::group_by(Marka_pojazdu)%>%dplyr::summarise(n=n())%>%dplyr::arrange(desc(n))
str(ofertyV)
summary(ofertyV)
levels(ofertyV$Model_pojazdu)
ofertyV<-ofertyV%>%dplyr::select(-Marka_pojazdu)%>%droplevels()
ofertyV<-ofertyV%>%dplyr::select(-Liczba_miejsc,-Napęd)
ofertyV<-na.omit(ofertyV)
ofertyV<-ofertyV%>%dplyr::filter(Rodzaj_paliwa=="Benzyna" | Rodzaj_paliwa=="Diesel")
ofertyV%>%droplevels()

View(mixed_assoc(ofertyV))
library(VIM)
aggr(ofertyV, numbers=TRUE, 
     sortVars=TRUE, 
     labels=names(data), 
     cex.axis=.7)
library(mice)
View(md.pattern(ofertyV))
levels(ofertyV$Napęd)
#install.packages(c("regclass"))
#install.packages(c("regclass","caTools","randomForest","rpart","e1071","kknn","caret","mlr"))
library(caTools)
library(randomForest)
library(rpart)
library(e1071)
library(kknn)
library(caret)

set.seed(123)
sample<-sample.split(Y=ofertyV,SplitRatio = .75)
trains<-subset(ofertyV,sample==TRUE)
tests<-subset(ofertyV,sample==FALSE)
?randomForest

#regrRF<-randomForest(cena~.,data =trains )
save(regrRF,file="regrRF.RData")
save(ofertyV,file="ofertyV.RData")
predictionsRF<-predict(regrRF,tests)
myMAE<- mean( abs(tests$cena-predictionsRF) )

trains<-trains%>%dplyr::mutate(Liczba_drzwi=factor(Liczba_drzwi,levels=(levels(ofertyV$Liczba_drzwi)) ) )
tests<-tests%>%dplyr::mutate(Liczba_drzwi=factor(Liczba_drzwi,levels=(levels(ofertyV$Liczba_drzwi)) ) )

levels(tests$Liczba_drzwi)
levels(trains$Liczba_drzwi)


ofertyV%>%dplyr::group_by(Liczba_drzwi)%>%dplyr::summarise(n=n())%>%dplyr::arrange(desc(n))
ofertyV%>%dplyr::group_by(Kolor)%>%dplyr::summarise(n=n())%>%dplyr::arrange(desc(n))
ofertyBezMalychKat<-ofertyV%>%dplyr::select(-Liczba_drzwi)
set.seed(123)
sample<-sample.split(Y=ofertyBezMalychKat,SplitRatio = .75)
trains<-subset(ofertyV,sample==TRUE)
tests<-subset(ofertyV,sample==FALSE)
nrow(trains)
nrow(tests)

linearR1<- lm(cena~Rok_produkcji,data=trains)
predictionslinearR1<-predict(linearR1,tests)
myMAElinearR1<- mean( abs(tests$cena-predictionslinearR1) )

linearR<- lm(cena ~Przebieg+Rok_produkcji+Moc+Model_pojazdu+Skrzynia_biegów+Oferta_od,data=trains)
predicttionsLinearR <- predict(linearR, tests) 
myMAELinear<- mean( abs(tests$cena-predicttionsLinearR) )

regrKNN<-train.kknn(cena~.,data=trains,kmax=15)
predictionsKNN<-predict(regrKNN,tests)
myMAEKNN<-mean(abs(tests$cena-predictionsKNN))

partR <- rpart(cena~.,
               method="anova", data=trains)


predicttionsPartR<-predict(partR,tests)
myMAEppartR<- mean( abs(tests$cena-predicttionsPartR) )

plotcp(partR)
cp=partR$cptable[which.min(partR $cptable[,"xerror"]),"CP"]
?kknn
plot(partR, uniform=TRUE,
     main="Prune Regression Tree")
text(partR, use.n=TRUE, all=TRUE)


summary(linearR1)

sampleSmall<-sample.split(Y=trains,SplitRatio= .1)
trainsSmall<-subset(trains,sample==TRUE)

#modelCaretRF<- caret::train(Stan~.,data=trains,method="rf",metric="Accuracy")
#modelCaretRFSmall<- caret::train(Stan~.,data=trainsSmall,method="rf",metric="Accuracy")

modelLM<-caret::train(cena~.,data=trains,method="lm")
control<-trainControl(method = "cv",10)


modelLM<-caret::train(cena~.,data=trains,method="lm",trControl=control)
predictedLM<-predict(modelLM,tests)

summary(modelLM)
postResample(predictedLM,tests$cena)



#carrpred<-predict(modelLM,newdata=tests)
#mycf<-confusionMatrix(data=tests[,'Stan'],reference = carrpred,mode="prec_recall")
#mycf$overall
#mycf$byClass
#acc<-mycf$overall[1]
#precision<-mycf$byClass['Precision']
#recall<-mycf$byClass['Recall']
#f1<-mycf$byClass['F1']

library(mlr)

sample<-sample.split(Y=ofertyV,SplitRatio = .2)
maleV<-subset(ofertyV,sample==TRUE)
maleV<-maleV%>%droplevels()
nrow(maleV) 
colnames(maleV)
names(maleV)<-gsub("ść","sc",names(maleV))
names(maleV)<-gsub("ó","o",names(maleV))
zadanieRegresja<-mlr::makeRegrTask(id="regresja",maleV,target="cena")
zadanieRegresja<- mergeSmallFactorLevels(zadanieRegresja,min.perc=0.01,new.level = ".merged")
levels( (getTaskData(zadanieRegresja)$Model_pojazdu ) )


n<-getTaskSize(zadanieRegresja)
set.seed(123)
train.set<-sample(n, size=(n*3/4) )
'%ni%'<-Negate('%in%')
test.set<-seq(1,n,by=1)
test.set<-test.set[test.set%ni%train.set]

learner_rf<-makeLearner("regr.randomForest",id="rf",fix.factors.prediction = TRUE)
cost_rf<-train(learner_rf,zadanieRegresja,subset = train.set)
fitted_rf<-predict(cost_rf,zadanieRegresja,subset=test.set)
maeRF<- mean( abs(fitted_rf$data$truth-fitted_rf$data$response) )

learner_kknn<-makeLearner("regr.kknn",id="kknn",fix.factors.prediction = TRUE)
cost_kknn<-train(learner_kknn,zadanieRegresja,subset = train.set)
fitted_kknn<-predict(cost_kknn,zadanieRegresja,subset=test.set)
maeKKNN<- mean( abs(fitted_kknn$data$truth-fitted_kknn$data$response) )

learner_svm<-makeLearner("regr.svm",id="svm",fix.factors.prediction = TRUE)
cost_svm<-train(learner_svm,zadanieRegresja,subset = train.set)
fitted_svm<-predict(cost_svm,zadanieRegresja,subset=test.set)
maeSVM<- mean( abs(fitted_svm$data$truth-fitted_svm$data$response) )

learner_lm<-makeLearner("regr.lm",id="lm",fix.factors.prediction = TRUE)
cost_lm<-train(learner_lm,zadanieRegresja,subset = train.set)
fitted_lm<-predict(cost_lm,zadanieRegresja,subset=test.set)
maeLM<- mean( abs(fitted_lm$data$truth-fitted_lm$data$response) )

cost_benchmark<- mlr::benchmark(list(learner_rf,learner_lm,learner_svm, learner_kknn),resamplings = cv10,measures = mae,tasks = zadanieRegresja)
plotBMRBoxplots(cost_benchmark)


require(rcompanion)
# Calculate a pairwise association between all variables in a data-frame. In particular nominal vs nominal with Chi-square, numeric vs numeric with Pearson correlation, and nominal vs numeric with ANOVA.
# Adopted from https://stackoverflow.com/a/52557631/590437
mixed_assoc = function(df, cor_method="spearman", adjust_cramersv_bias=TRUE){
  df_comb = expand.grid(names(df), names(df),  stringsAsFactors = F) %>% set_names("X1", "X2")
  
  is_nominal = function(x) class(x) %in% c("factor", "character")
  # https://community.rstudio.com/t/why-is-purr-is-numeric-deprecated/3559
  # https://github.com/r-lib/rlang/issues/781
  is_numeric <- function(x) { is.integer(x) || is_double(x)}
  
  f = function(xName,yName) {
    x =  pull(df, xName)
    y =  pull(df, yName)
    
    result = if(is_nominal(x) && is_nominal(y)){
      # use bias corrected cramersV as described in https://rdrr.io/cran/rcompanion/man/cramerV.html
      cv = cramerV(as.character(x), as.character(y), bias.correct = adjust_cramersv_bias)
      data.frame(xName, yName, assoc=cv, type="cramersV")
      
    }else if(is_numeric(x) && is_numeric(y)){
      correlation = cor(x, y, method=cor_method, use="complete.obs")
      data.frame(xName, yName, assoc=correlation, type="correlation")
      
    }else if(is_numeric(x) && is_nominal(y)){
      # from https://stats.stackexchange.com/questions/119835/correlation-between-a-nominal-iv-and-a-continuous-dv-variable/124618#124618
      r_squared = summary(lm(x ~ y))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else if(is_nominal(x) && is_numeric(y)){
      r_squared = summary(lm(y ~x))$r.squared
      data.frame(xName, yName, assoc=sqrt(r_squared), type="anova")
      
    }else {
      warning(paste("unmatched column type combination: ", class(x), class(y)))
    }
    
    # finally add complete obs number and ratio to table
    result %>% dplyr::mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}











