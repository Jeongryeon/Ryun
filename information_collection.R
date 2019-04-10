#논문 데이터 수집 코드
#cocomelon (education categories)
#movieclips (film categories)
#MAria Clara &JP
#LUCCAS NETO- LUNES
#mmoshaya 
#각 영상별 20개의 영상 수집 예정 
###################################################

#필요 패키지 
install.packages("tuber")
install.packages("httpuv")
library(tuber)
library(httpuv)

#데이터 수집 접근 api 인증 
app_id <-"964922740842-7bhbetu2mn2sfe47blv34v19cmshcb9j.apps.googleusercontent.com"
app_secret<-"BqzLo2cKIGdFcRlcWSVUaXtb"
yt_oauth(app_id,app_secret,token = "")

#TV learning english 댓글 수집 
LearnEnglishTV<-yt_search(term="", type="video", channel_id = "UCKgpamMlm872zkGDcBJHYDg")
EnglishwithLucy1$video_id[1]

#TV learning english 채널 20개 영상에 대한 댓글 수집 
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(LearnEnglishTV$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "LearnEnglishTV.csv")
}
#TV learning english 채널 20개 영상에 대한 댓글 수집 
EnglishCoach<-yt_search(term="", type="video", channel_id = "UC-g0gSStENkYPXFRsKrlvyA")
EnglishCoach[1]
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(EnglishCoach$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "EnglishCoach.csv")
}
#TV learning english 채널 20개 영상에 대한 댓글 수집 
EnglishNative<-yt_search(term="", type="video", channel_id = "UC0Hg2Ks00kCekyjZG_LxOmg")
EnglishCoversation[1]
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(EnglishNative$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "EnglishNative.csv")
}

#TV learning english 채널 20개 영상에 대한 댓글 수집 
EnglishVOA<-yt_search(term="", type="video", channel_id = "UCKyTokYo0nK2OA-az-sDijA")
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(EnglishVOA$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "EnglishVOA.csv")
}






#bbc learning english 댓글 수집 
bbc<-yt_search(term="", type="video", channel_id = "UCHaHD477h-FeBbVh9Sh7syA")

#bbc learning english 채널 20개 영상에 대한 댓글 수집 
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(bbc$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "BBC.csv")
}




#Learn English with Emma 댓글 수집 
Emma<-yt_search(term="", type="video", channel_id = "UCVBErcpqaokOf4fI5j73K_w")

#Learn English with Emma 채널 20개 영상에 대한 댓글 수집 
video_cmt_bind= rbind()
for (i in 1:20){
  #한 채널에 있는 모든 영상(i개)의 댓글 크롤링하여 새로운 변수에 저장
  video_comments<-get_all_comments(Emma$video_id[i])
  
  #아래와 같은 정보를 추출하여 데이터프레임으로 만들고, 각각 적용
  video_cmt_bind[[i]]= data.frame(video_comments$videoId, video_comments$textOriginal)
  #리스트 형태를 rbind를 통해 matrix로 만듦
  video_to_matrix<-rbind(video_cmt_bind)
  #데이터프레임으로 만듦
  video_to_dataframe<-data.frame(do.call(rbind,video_to_matrix))
  #각각의 열 이름 변경
  names(video_to_dataframe) <- c("videoId", "Comments")
  #csv파일로 저장
  write.csv(video_to_dataframe, file = "Emma.csv")
}













