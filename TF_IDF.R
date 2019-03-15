install.packages("tuber")
install.packages("httpuv")
install.packages("dplyr")
install.packages("stringr")
install.packages("tibble")
install.packages("KoNLP")
install.packages("tm")
install.packages("wordcloud2")
install.packages("RColorBrewer") 
install.packages("wordcloud")
install.packages("janeaustenr")
install.packages("tidytext")
install.packages("ggplot2")


library(dplyr)
library(janeaustenr)
library(tidytext) 
library(tuber)
library(httpuv)
library(dplyr)
library(stringr)
library(tibble)
library(tm)
library(KoNLP)
library(wordcloud2)
useNIADic()
library(RColorBrewer)
library(wordcloud)
library(ggplot2)

#댓글 정보를 모아둔 파일 불러오기 
comments_data_csv<-read.csv(file = "english_coach.csv",header = T, stringsAsFactors = F )

#비디오 내부 단어별 빈도수 구하기 
comments_words <- comments_data_csv %>%
  #문장부호를 지워주고 소문자로 자동 변환 해 tidy text의 형태로 변환
  unnest_tokens(word, Comments) %>%
  #단어 개수를 구해서 정렬
  count(VideoId, word, sort = TRUE) %>%
  #그룹화 x
  ungroup()

typeof(comments_words)

#Video 별 전체 단어수 구하기
total_words <- comments_words %>% 
  #VideoId별로 그룹화
  group_by(VideoId) %>%
  #n의 합을 total로 저장
  summarize(total = sum(n))

#comments_words에  total_words 추가
comments_words <- left_join(comments_words, total_words)

comments_words


#TF-IDF구하기 (stopword같이 모든 영상의 등장하는 단어는 0으로 표기됨)
comments_words <- comments_words %>%
  bind_tf_idf(word, VideoId, n)

comments_words

#TF-IDF 기준으로 내림차순 정리
comments_words %>%
  select(-total) %>%
  arrange(desc(tf_idf))

#TF-IDF 파일로 저장
write.csv(comments_words,file = "comments_words.csv")

#TF-IDF 시각화
comments_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(VideoId) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = VideoId)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~VideoId, ncol = 2, scales = "free") +
  coord_flip()

#감정사전 값 추가 입력
word_sentiments<-comments_words %>%inner_join(get_sentiments("afinn")) %>%group_by(VideoId)
#tf-idf순서로 정렬
word_sentiments1<-word_sentiments%>%
  arrange(desc(tf_idf))
word_sentiments1


#가중치 적용 값을 저장할 tible 생성
word_TFIDF_se <- tibble( word_sentiments1$VideoId,(word_sentiments1$tf_idf* as.double(word_sentiments1$score))) 
names(word_TFIDF_se)<-c("VideoId","weight")
word_TFIDF_se

#가중치 값 그래프에 추가
word_sentiments_TFIDF<-left_join(word_sentiments1,word_TFIDF_se)  
word_sentiments_TFIDF
