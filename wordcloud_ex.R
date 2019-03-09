install.packages("tuber")
install.packages("httpuv")
install.packages("dplyr")
install.packages("stringr")
install.packages("tibble")
install.packages("KoNLP")
install.packages("tm")
install.packages("wordcloud2")
install.packages("arules")
install.packages("igraph")
install.packages("combinat")
install.packages("SnowballC")
install.packages("RColorBrewer") 
install.packages("wordcloud")


library(tuber)
library(httpuv)
library(dplyr)
library(stringr)
library(tibble)
library(tm)
library(KoNLP)
library(wordcloud2)
useNIADic()
library(arules)
library(igraph)
library(combinat)
library(SnowballC)
library(RColorBrewer)
library(wordcloud)



app_id <-"152593731585-3fu2n5m15ib2m2hpajjf7i78qvudsb9v.apps.googleusercontent.com"
app_secret<-"SYnhxexEduwYRLMjILt2vzFk"
yt_oauth(app_id,app_secret,token = "")

chanel_if<-yt_search(term="", type="video", channel_id = "UC-g0gSStENkYPXFRsKrlvyA")

video_cmt_bind= rbind()
#한 채널에 있는 동영상의 댓글 정보 중, 댓글 원본, 동영상 아이디,그리고 댓글 단 사용자 아이디를 데이터프레임으로 추출. 
for (i in 1:5){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_f_comments<-get_all_comments(chanel_if$video_id[i])
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  
  video_cmt_bind[[i]]= data.frame(video_f_comments$authorDisplayName, video_f_comments$textOriginal, video_f_comments$videoId)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("AuthorName", "Comments", "VideoId")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "english_coach.csv")
}
#데이터 불러오기
english_txt<-read.csv(file = "english_coach.csv",header = T, stringsAsFactors = F )
english_txt
typeof(english_txt)

english_txt_ed<-as.vector(english_txt$Comments)
english_txt_ed
typeof(english_txt_ed)
english_ed_cor<- Corpus(VectorSource(english_txt_ed))
str(english_ed_cor)

#공백제거
english_ed_cor <- tm_map(english_ed_cor, stripWhitespace)

# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
english_ed_cor<- tm_map(english_ed_cor, tolower)
# Remove numbers(숫자제거)
english_ed_cor <- tm_map(english_ed_cor, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
english_ed_cor <- tm_map(english_ed_cor, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
english_ed_cor <- tm_map(english_ed_cor, removePunctuation)
# Text stemming(어근만 추출한다)
english_ed_cor <- tm_map(english_ed_cor, stemDocument)


#정제된 데이터 TermDocumentMatrix를 사용 
english_tdm <- TermDocumentMatrix(english_ed_cor)
#문장 별 단어 빈도수  
english_tdm_ma <- as.matrix(english_tdm)

english_tdm
english_tdm_ma
write.csv(english_tdm_ma, file = "english_coach_tdm_ma.csv")

#term document matrix의 결과를  합해서 내림차순으로 정렬
english_sort<- sort(rowSums(english_tdm_ma),decreasing=TRUE) 
english_df <- data.frame(word = names(english_sort),freq=english_sort)
head(english_sort, 10)
head(english_df, 10)
typeof(english_sort)
typeof(english_df)

#워드클라우드 생성  
wordcloud(words = english_df$word,
          freq = english_df$freq,
          min.freq = 1,
          max.words=200,
          random.order=FALSE,
          rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


#TF-IDF 시작 (초기버젼 문장 별 IDF 구하기)

#문장 별 단어 빈도수 행의 합 구하기  
english_tdm_ma_rowSum<-rowSums(english_tdm_ma)
write.csv(english_tdm_ma_rowSum, file = "english_tdm_ma_rowSum.csv")

typeof(english_tdm_ma)
english_tdm_ma_df<-as.data.frame(english_tdm_ma)
english_tdm_ma_df
write.csv(english_tdm_ma_df, file = "english_tdm_ma_df.csv")
english_tdm_ma_df[1,]
write.csv(english_tdm_ma_df[1,], file = "english_tdm_ma_df[1,].csv")
english_tdm_ma_df[19,]

zxc<-duplicated(english_tdm_ma_df)
write.csv(zxc, file = "zxc.csv")


