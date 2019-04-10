#전처리 코드 
#각 영상에서 수집한 데이터를 단어별로 전처리
#arule패키지를 이용해 연관규칙분석(association rule analysis)에 이용하기 위한 전처리
######################################################################################
rm(list=ls())
#필요 패키지 
install.packages("KoNLP")
install.packages("tm")
library(KoNLP)
library(tm)
useNIADic()


#LearnEnglishTV 채널에 대한전처리 
LearnEnglishTV_csv<-read.csv(file = "LearnEnglishTV.csv",header = T, stringsAsFactors = F )
#불러온 데이터 벡터화 
comments_data_vec<-as.vector(englishclass101_csv$Comments)
#데이터 corpus로 저장 
comments_data_cor<- Corpus(VectorSource(comments_data_vec))

#공백제거
comments_data_pre <- tm_map(comments_data_cor, stripWhitespace)


# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
comments_data_pre<- tm_map(comments_data_pre, tolower)
# Remove numbers(숫자제거)
comments_data_pre <- tm_map(comments_data_pre, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
comments_data_pre <- tm_map(comments_data_pre, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
comments_data_pre <- tm_map(comments_data_pre, removePunctuation)
# Text stemming(어근만 추출한다)
comments_data_pre <- tm_map(comments_data_pre, stemDocument)

#정제된 데이터 TermDocumentMatrix를 사용 
comments_data_tdm <- TermDocumentMatrix(comments_data_pre)
#문장 별 단어 빈도수  
comments_word_ma <- as.matrix(comments_data_tdm)

#문장 별 단어 빈도수 matrix csv파일로 저장
comments_word_ma_t<-t(comments_word_ma)
write.csv(comments_word_ma_t, file = "LearnEnglishTV_comments_word_t.csv")


#####################################################################################################


#EnglishCoach 채널에 대한전처리 
englishclass101_csv<-read.csv(file = "EnglishCoach.csv",header = T, stringsAsFactors = F )
#불러온 데이터 벡터화 
comments_data_vec<-as.vector(englishclass101_csv$Comments)
#데이터 corpus로 저장 
comments_data_cor<- Corpus(VectorSource(comments_data_vec))

#공백제거
comments_data_pre <- tm_map(comments_data_cor, stripWhitespace)


# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
comments_data_pre<- tm_map(comments_data_pre, tolower)
# Remove numbers(숫자제거)
comments_data_pre <- tm_map(comments_data_pre, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
comments_data_pre <- tm_map(comments_data_pre, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
comments_data_pre <- tm_map(comments_data_pre, removePunctuation)
# Text stemming(어근만 추출한다)
comments_data_pre <- tm_map(comments_data_pre, stemDocument)

#정제된 데이터 TermDocumentMatrix를 사용 
comments_data_tdm <- TermDocumentMatrix(comments_data_pre)
#문장 별 단어 빈도수  
comments_word_ma <- as.matrix(comments_data_tdm)

#문장 별 단어 빈도수 matrix csv파일로 저장
comments_word_ma_t<-t(comments_word_ma)
write.csv(comments_word_ma_t, file = "EnglishCoach_comments_word_t.csv")


#####################################################################################################
#EnglishNative 채널에 대한전처리 
englishclass101_csv<-read.csv(file = "EnglishNative.csv",header = T, stringsAsFactors = F )
#불러온 데이터 벡터화 
comments_data_vec<-as.vector(englishclass101_csv$Comments)
#데이터 corpus로 저장 
comments_data_cor<- Corpus(VectorSource(comments_data_vec))

#공백제거
comments_data_pre <- tm_map(comments_data_cor, stripWhitespace)


# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
comments_data_pre<- tm_map(comments_data_pre, tolower)
# Remove numbers(숫자제거)
comments_data_pre <- tm_map(comments_data_pre, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
comments_data_pre <- tm_map(comments_data_pre, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
comments_data_pre <- tm_map(comments_data_pre, removePunctuation)
# Text stemming(어근만 추출한다)
comments_data_pre <- tm_map(comments_data_pre, stemDocument)

#정제된 데이터 TermDocumentMatrix를 사용 
comments_data_tdm <- TermDocumentMatrix(comments_data_pre)
#문장 별 단어 빈도수  
comments_word_ma <- as.matrix(comments_data_tdm)

#문장 별 단어 빈도수 matrix csv파일로 저장
comments_word_ma_t<-t(comments_word_ma)
write.csv(comments_word_ma_t, file = "EnglishNative_comments_word_t.csv")


#####################################################################################################


#bbc learning english  채널에 대한전처리 
BBC_csv<-read.csv(file = "BBC.csv",header = T, stringsAsFactors = F )
#불러온 데이터 벡터화 
comments_data_vec<-as.vector(BBC_csv$Comments)
#데이터 corpus로 저장 
comments_data_cor<- Corpus(VectorSource(comments_data_vec))

#공백제거
comments_data_pre <- tm_map(comments_data_cor, stripWhitespace)


# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
comments_data_pre<- tm_map(comments_data_pre, tolower)
# Remove numbers(숫자제거)
comments_data_pre <- tm_map(comments_data_pre, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
comments_data_pre <- tm_map(comments_data_pre, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
comments_data_pre <- tm_map(comments_data_pre, removePunctuation)
# Text stemming(어근만 추출한다)
comments_data_pre <- tm_map(comments_data_pre, stemDocument)

#정제된 데이터 TermDocumentMatrix를 사용 
comments_data_tdm <- TermDocumentMatrix(comments_data_pre)
#문장 별 단어 빈도수  
comments_word_ma <- as.matrix(comments_data_tdm)

#문장 별 단어 빈도수 matrix csv파일로 저장
comments_word_ma_t<-t(comments_word_ma)
write.csv(comments_word_ma_t, file = "BBC_comments_word_t.csv")

##################################################################################


#Learn English with Emma  채널에 대한전처리 
Emma_csv<-read.csv(file = "Emma.csv",header = T, stringsAsFactors = F )
#불러온 데이터 벡터화 
comments_data_vec<-as.vector(Emma_csv$Comments)
#데이터 corpus로 저장 
comments_data_cor<- Corpus(VectorSource(comments_data_vec))

#공백제거
comments_data_pre <- tm_map(comments_data_cor, stripWhitespace)

# Convert the text to lower case(소문자변경-사전에 있는 내용과 비교하기 위해)
comments_data_pre<- tm_map(comments_data_pre, tolower)
# Remove numbers(숫자제거)
comments_data_pre <- tm_map(comments_data_pre, removeNumbers)
# Remove english common stopwords(뛰어쓰기와 시제 제거 )
comments_data_pre <- tm_map(comments_data_pre, removeWords, stopwords("english"))

# Remove punctuations(구두점제거)
comments_data_pre <- tm_map(comments_data_pre, removePunctuation)
# Text stemming(어근만 추출한다)
comments_data_pre <- tm_map(comments_data_pre, stemDocument)

#정제된 데이터 TermDocumentMatrix를 사용 
comments_data_tdm <- TermDocumentMatrix(comments_data_pre)
#문장 별 단어 빈도수  
comments_word_ma <- as.matrix(comments_data_tdm)

#문장 별 단어 빈도수 matrix csv파일로 저장
comments_word_ma_t<-t(comments_word_ma)
write.csv(comments_word_ma_t, file = "Emma_comments_word_t.csv")
