#각 채널에 대한 TF-IDF 구하기
#감정사전의 값을 곱해 TF-IDF를 확장해서 보다 결과를 명확히 한다. 
#수치 데이터를 저장 후 연관규칙분석(association rule analysis)의 데이터와 합쳐 최종 네트워크형 그래프 작성
###########################################################################################################

#필요 패키지 
install.packages("tidytext")
install.packages("stringr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("scales")
install.packages("Brewer")
install.packages("viridis")

library(tidytext)
library(tibble)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(RColorBrewer)
library(viridis)


#learn english with englishclass101.com 채널 
LearnEnglishTV_csv
#비디오 내부 단어별 빈도수 구하기 
comments_words1 <- LearnEnglishTV_csv %>%
  #문장부호를 지워주고 소문자로 자동 변환 해 tidy text의 형태로 변환
  unnest_tokens(word, Comments) %>%anti_join(stop_words)%>% 
  #단어 개수를 구해서 정렬
  count(videoId, word, sort = TRUE) %>%
  #그룹화 x
  ungroup()

comments_words1
data("stop_words")
comments_words1 <- comments_words1 %>%
  anti_join(stop_words)

comments_words1

#Video 별 전체 단어수 구하기
total_words <- comments_words1 %>% 
  #VideoId별로 그룹화
  group_by(videoId) %>%
  #n의 합을 total로 저장
  summarize(total = sum(n))
total_words
#comments_words에  영상별 total_words 추가
comments_words1 <- left_join(comments_words1, total_words)


comments_words1


#TF-IDF구하기 (stopword같이 모든 영상의 등장하는 단어는 0으로 표기됨)
comments_words1 <- comments_words1 %>%
  bind_tf_idf(word, videoId, n)

comments_words1

#TF-IDF 기준으로 내림차순 정리
b<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))
b<-head(b,30)
b<-b[-c(16:17,19,26,21,24),]
b<-b[-7,]
b
b[11:30,]

a<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf))
a[11:30,]
a<-head(a,30)


#감정사전 값 추가 입력
word_sentiments<-comments_words %>%inner_join(get_sentiments("afinn")) %>%group_by(videoId)
word_sentiments
#tf-idf순서로 정렬
word_sentiments1<-word_sentiments%>%
  arrange(desc(tf_idf))
word_sentiments1

#가중치 적용 값을 저장할 tible 생성
word_TFIDF_se <- tibble( word_sentiments1$videoId,word_sentiments1$tf_idf,(word_sentiments1$tf_idf* as.double(word_sentiments1$score))) 
names(word_TFIDF_se)<-c("videoId","tf_idf","weight")
word_TFIDF_se

#가중치 값 그래프에 추가
word_sentiments_TFIDF<-left_join(word_sentiments1,word_TFIDF_se)  
word_sentiments_TFIDF
write.csv(word_sentiments_TFIDF,file = "englishclass101.com_TFIDF.csv")

#tf순서로 정렬
word_sentiments_TF<-word_sentiments_TFIDF%>%
  arrange(desc(tf))

#TF-IDF 기준으로 내림차순 정리
b<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))
b<-head(b,30)

a<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf))
a<-head(a,30)
#TF 기준 
ggplot(data = a, aes(x=word, y=tf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
a
#TF_IDF
a<-head(word_sentiments_TFIDF,30)
a
ggplot(data = b, aes(x=word, y=tf_idf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
b
###############################################################################################################
#bbc learning english 채널 
BBC_csv
#비디오 내부 단어별 빈도수 구하기 
comments_words1 <- BBC_csv %>%
  #문장부호를 지워주고 소문자로 자동 변환 해 tidy text의 형태로 변환
  unnest_tokens(word, Comments) %>%anti_join(stop_words)%>% 
  #단어 개수를 구해서 정렬
  count(videoId, word, sort = TRUE) %>%
  #그룹화 x
  ungroup()

comments_words1
data("stop_words")
comments_words1 <- comments_words1 %>%
  anti_join(stop_words)

comments_words1<-comments_words1[-1,]
comments_words1

#Video 별 전체 단어수 구하기
total_words <- comments_words1 %>% 
  #VideoId별로 그룹화
  group_by(videoId) %>%
  #n의 합을 total로 저장
  summarize(total = sum(n))
total_words
#comments_words에  영상별 total_words 추가
comments_words1 <- left_join(comments_words1, total_words)


comments_words1


#TF-IDF구하기 (stopword같이 모든 영상의 등장하는 단어는 0으로 표기됨)
comments_words1 <- comments_words1 %>%
  bind_tf_idf(word, videoId, n)

comments_words1

#TF-IDF 기준으로 내림차순 정리
comments_words13<-comments_words1 %>%
  arrange(desc(tf))
comments_words13
comments_words13<-comments_words13[-c(2:4,7:8),]
comments_words13<-comments_words13[-10,]
comments_words13
a<-head(comments_words13,30)

#TF-IDF 기준으로 내림차순 정리
comments_words1<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#전처리
comments_words1<-comments_words1[-1,]
comments_words12<-comments_words1[-c(1:5),]
comments_words12<-comments_words12[-c(2,7),]
b<- head(comments_words12,30)

#TF 기준 
ggplot(data = a, aes(x=word, y=tf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
a
#TF_IDF
a<-head(word_sentiments_TFIDF,30)
a
ggplot(data = b, aes(x=word, y=tf_idf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
b


#################################################################################################




#Learn English with Emma 채널 
Emma_csv$Comments
#비디오 내부 단어별 빈도수 구하기 
comments_words1 <- Emma_csv %>%
  #문장부호를 지워주고 소문자로 자동 변환 해 tidy text의 형태로 변환
  unnest_tokens(word, Comments) %>%anti_join(stop_words)%>% 
  #단어 개수를 구해서 정렬
  count(videoId, word, sort = TRUE) %>%
  #그룹화 x
  ungroup()

comments_words1
data("stop_words")
comments_words1 <- comments_words1 %>%
  anti_join(stop_words)

comments_words1

#Video 별 전체 단어수 구하기
total_words <- comments_words1 %>% 
  #VideoId별로 그룹화
  group_by(videoId) %>%
  #n의 합을 total로 저장
  summarize(total = sum(n))
total_words
#comments_words에  영상별 total_words 추가
comments_words1 <- left_join(comments_words1, total_words)


comments_words1


#TF-IDF구하기 (stopword같이 모든 영상의 등장하는 단어는 0으로 표기됨)
comments_words1 <- comments_words1 %>%
  bind_tf_idf(word, videoId, n)

comments_words1

#TF-IDF 기준으로 내림차순 정리
comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))



#TF 기준으로 내림차순 정리
comments_words13<-comments_words1 %>%
  arrange(desc(tf))
comments_words13<-comments_words13[-11,]
comments_words13[13:30,]
comments_words13[c(18,24),]
comments_words13<-comments_words13[-c(18,24),]
comments_words13[c(28:30),]

comments_words13<-comments_words13[-c(2:4,7:8),]
comments_words13<-comments_words13[-6,]
comments_words13
a<-head(comments_words13,30)

#TF-IDF 기준으로 내림차순 정리
comments_words12<-comments_words1 %>%
  select(-total) %>%
  arrange(desc(tf_idf))
comments_words12[c(11:30),]
comments_words12<-comments_words12[-30,]
comments_words12[30,]
#전처리
comments_words1<-comments_words1[-1,]
comments_words12<-comments_words1[-c(1:5),]
comments_words12<-comments_words12[-c(2,7),]
b<- head(comments_words12,30)

#TF 기준 
ggplot(data = a, aes(x=word, y=tf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
a
#TF_IDF

ggplot(data = b, aes(x=word, y=tf_idf,fill=videoId) )+geom_bar(stat='identity', position = 'dodge') 
b

