#arule 패키지를 이용해 연관규칙분석(association rule analysis) 
#분석 데이터는 단어 간의 관계를 생성하는 것이 의미
#단어 간 중요도는 TF-IDF가 중심이 될 것 본 과정은 단어간의 연관도를 찾았다는 것 자체가 의미
#지지도와 
###########################################################################################
install.packages("ggplot2")
install.packages("topicmodels")
install.packages("arules")
install.packages("arulesViz")

library(ggplot2)
library(topicmodels)
library(arules)
library(arulesViz)



#word matrix 불러오기
TV_tma<-read.csv("LearnEnglishTV_comments_word_t.csv",header = T,stringsAsFactors = F)

TVlen<-length(TV_tma$help)

#행 이름 지정 
rownames(TV_tma)<-paste0("cmt",1:TVlen)
#필요없는 X열 제거
TV_tma_ed<-TV_tma[,-1]
comment_tma_ed
#숫자 값을 논리 연산으로 변환 
comment_tma_ed1<-as.data.frame(sapply(TV_tma_ed,as.logical)) 

#dataframe transations으로 변환
comment_tma_ed.trans<-as(comment_tma_ed1,'transactions')
comment_tma_ed.trans
#arule패키지를 이용하요 규칙 생성(단어 연관도)
rule <- apriori(comment_tma_ed.trans, parameter=list(support=0.01, confidence=0.6, minlen=2, maxlen=6))
rule
summary(rule)
inspect(rule)
plot(rule, method = "graph", control = list(type="items"))


#word matrix 불러오기
BBC_tma<-read.csv("BBC_comments_word_t.csv",header = T,stringsAsFactors = F)
BBC_tma
BBClen<-length(BBC_tma$lone)
BBClen
#행 이름 지정 
rownames(BBC_tma)<-paste0("cmt",1:BBClen)
#필요없는 X열 제거
BBC_tma_ed<-BBC_tma[,-1]
BBC_tma_ed
#숫자 값을 논리 연산으로 변환 
BBC_tma_ed1<-as.data.frame(sapply(BBC_tma_ed,as.logical)) 

#dataframe transations으로 변환
BBC_tma_ed1.trans<-as(BBC_tma_ed1,'transactions')
BBC_tma_ed1.trans
#arule패키지를 이용하요 규칙 생성(단어 연관도)
rule <- apriori(BBC_tma_ed1.trans, parameter=list(support=0.01, confidence=0.6, minlen=2, maxlen=6))
rule
summary(rule)
inspect(rule)

plot(rule, method = "graph")


#word matrix 불러오기
Emma_tma<-read.csv("Emma_comments_word_t.csv",header = T,stringsAsFactors = F)
Emma_tma1<-Emma_tma[1:10000,]
Emmalen<-length(Emma_tma1$emma)
Emmalen
#행 이름 지정 
rownames(Emma_tma1)<-paste0("cmt",1:Emmalen)
#필요없는 X열 제거
Emma_tma_ed<-Emma_tma1[,-1]
Emma_tma_ed
#숫자 값을 논리 연산으로 변환 
Emma_tma_ed1<-as.data.frame(sapply(Emma_tma_ed,as.logical)) 
Emma_tma_ed1

#dataframe transations으로 변환
Emma_tma_ed1.trans<-as(Emma_tma_ed1,'transactions')
Emma_tma_ed1.trans
#arule패키지를 이용하요 규칙 생성(단어 연관도)
rule <- apriori(Emma_tma_ed1.trans, parameter=list(support=0.01, confidence=0.6, minlen=2, maxlen=6))
rule
summary(rule)
inspect(rule)

#word matrix 불러오기
BBC_tma<-read.csv("EnglishCoach_comments_word_t.csv",header = T,stringsAsFactors = F)
BBC_tma
BBClen<-length(BBC_tma$lone)
BBClen
#행 이름 지정 
rownames(BBC_tma)<-paste0("cmt",1:BBClen)
#필요없는 X열 제거
BBC_tma_ed<-BBC_tma[,-1]
BBC_tma_ed
#숫자 값을 논리 연산으로 변환 
BBC_tma_ed1<-as.data.frame(sapply(BBC_tma_ed,as.logical)) 

#dataframe transations으로 변환
BBC_tma_ed1.trans<-as(BBC_tma_ed1,'transactions')
BBC_tma_ed1.trans
#arule패키지를 이용하요 규칙 생성(단어 연관도)
rule <- apriori(BBC_tma_ed1.trans, parameter=list(support=0.01, confidence=0.6, minlen=2, maxlen=6))
rule
summary(rule)
inspect(rule)

plot(rule, method = "graph")
