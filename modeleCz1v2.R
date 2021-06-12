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
names(wszystkieOferty)
nrow(wszystkieOferty)

#mutate
wszystkieOferty <- wszystkieOferty080621%>%mutate(cena=   as.numeric(cena%>%str_replace_all("[^\\d,]", "")%>%str_replace_all(",",".")) ) 

wszystkieOferty<- wszystkieOferty%>%mutate(Przebieg=as.numeric(Przebieg%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<- wszystkieOferty%>%mutate(Rok_produkcji=as.numeric(Rok_produkcji%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<- wszystkieOferty%>%mutate(Moc=as.numeric(Moc%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
wszystkieOferty<-wszystkieOferty%>%mutate(Pojemność_skokowa= substr(Pojemność_skokowa,1,str_length(Pojemność_skokowa)-1) )
wszystkieOferty<- wszystkieOferty%>%mutate(Pojemność_skokowa=as.numeric(Pojemność_skokowa%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))
#wszystkieOferty<- wszystkieOferty%>%mutate(Pojemność_skokowa=as.numeric(Pojemność_skokowa%>%str_replace_all("cm3","")%>%str_replace_all("[^\\d,]","")%>%str_replace_all(",",".")))

#wszystkieOfertyTest<- wszystkieOferty%>%filter(waluta=="PLN", (Bezwypadkowy=='Tak' | is.na(Bezwypadkowy))  )
wszystkieOferty<-wszystkieOferty%>%filter(waluta=="PLN",Uszkodzony!="Tak" | is.na(Uszkodzony) )
str(wszystkieOferty)
summary(wszystkieOferty)

nrow(wszystkieOferty)


#select
wszystkieOferty<-wszystkieOferty%>%select(-waluta,-Kategoria)
wszystkieOferty<-wszystkieOferty%>%select(cena:Kolor,Stan)
summary(wszystkieOferty)
str(ofertyV)
ofertyV<-wszystkieOferty%>%filter(Marka_pojazdu=='Volkswagen')

#arrange
#group_by
#summarise
wszystkieOferty%>%group_by(Marka_pojazdu)%>%summarise(n=n())%>%arrange(desc(n))
str(ofertyV)
summary(ofertyV)
levels(ofertyV$Model_pojazdu)
ofertyV<-ofertyV%>%select(-Marka_pojazdu)%>%droplevels()
ofertyV<-ofertyV%>%select(-Liczba_miejsc,-Napęd)
ofertyV<-na.omit(ofertyV)
ofertyV%>%droplevels()

View(mixed_assoc(ofertyV))
#install.packages("VIM")
library(VIM)
aggr(ofertyV, numbers=TRUE, 
     sortVars=TRUE, 
     labels=names(data), 
     cex.axis=.7)
#install.packages("mice")
library(mice)
View(md.pattern(ofertyV))
levels(ofertyV$Napęd)

#install.packages(c("caTools","randomForest","rpart","e1071","kknn","caret","mlr"))
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

regrRF<-randomForest(cena~.,data =trains )
predictionsRF<-predict(regrRF,tests)
myMAE<- mean( abs(tests$cena-predictionsRF) )

mean(ofertyV$cena)


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
    result %>% mutate(complete_obs_pairs=sum(!is.na(x) & !is.na(y)), complete_obs_ratio=complete_obs_pairs/length(x)) %>% rename(x=xName, y=yName)
  }
  
  # apply function to each variable combination
  map2_df(df_comb$X1, df_comb$X2, f)
}











