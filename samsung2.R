install.packages("base64enc") # 한글 인코딩 관련 라이브러리
install.packages(c("RCurl","twitteR","ROAuth")) # twitteR 포함 인증관련 라이브러리
install.packages("KoNLP") # 한글처리를 위한 라이브러리
install.packages("plyr")
install.packages("tm")
install.packages("devtools")
devtools::install_github("lchiffon/wordcloud2")
library(base64enc)
library(RCurl)
library(twitteR)
library(ROAuth)
library(KoNLP)
library(wordcloud)
library(plyr)
library(tm)
library(wordcloud2)

reqURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authroize"

consumerKey <- 
consumerSecret <- 
accesstoken <- 
accesstokensecret <- 

cred <- OAuthFactory$new(consumerKey=consumerKey,
                         consumerSecret=consumerSecret,
                         requestURL="https://api.twitter.com/oauth/request_token",
                         accessURL="https://api.twitter.com/oauth/access_token",
                         authURL="https://api.twitter.com/oauth/authroize")

cred$handshake(cainfo = system.file("CurlSSL","cacert.pem", package = "RCurl"))
# 인증처리
setup_twitter_oauth(consumerKey,consumerSecret,accesstoken,accesstokensecret)

# 해당 단어를 언급한 글을 가지고 오고 싶을 때 사용
#keyword <- enc2utf8("빕스") # 한글처리
#tweets <- searchTwitter(keyword,n=10) # 가지고 오고 싶은 내용 검색
#tmp <- unlist(tweets, use.names = TRUE)
#head(tweets)

# 트위터에 키워드로 검색, 시작날짜, 지역코드(우리나라만 적용), 가져올 개수를 옵션에 대입
# 지역코드를 넣을려면 geocode='35.874,128.246,400km' 해당 부분을 넣으면 된다.

keyword <- enc2utf8("이벤트")
# si트nce 와 until로 날짜 지정해줘봤자 7일분 지정
twitter <- searchTwitter(keyword,
                         since='2017-10-01',
                         until='2017-12-31',
                         lang = "ko",
                         n=1500)
twitter.df <- twListToDF(twitter) # data.frame 형태로 해당 트위터의 정보를 추출한다
names(twitter.df)
twitter.df <- twitter.df[twitter.df$retweetCount!=0,]
twitter.text <- twitter.df$text # mention text만 추출
# gsub(지울글자, 어떤글자로 , 데이터)
twitter.text <- gsub("ㅠㅠ", "", twitter.text)
twitter.text <- gsub("\n", "", twitter.text)
twitter.text <- gsub("\r", "", twitter.text)
twitter.text <- gsub("RT", "", twitter.text)
twitter.text <- gsub("http","", twitter.text)
twitter.text <- gsub("co", "", twitter.text)
twitter.text <- gsub("ㅋ","", twitter.text)
twitter.text <- gsub("10","",twitter.text)
twitter.text <- gsub("12","",twitter.text)
twitter.text <- gsub("예전","",twitter.text)
twitter.text <- gsub("알티없으먄","",twitter.text)
twitter.text <- gsub("당발은","",twitter.text)
twitter.text <- gsub("개정안","",twitter.text)
twitter.text <- gsub("베스트셀러","",twitter.text)
twitter.text <- gsub("알티","",twitter.text)
twitter.text <- gsub("전안법","",twitter.text)
twitter.text <- gsub("알티추첨","",twitter.text)
twitter.text <- gsub("방탄소","",twitter.text)
twitter.text <- gsub("알티이벤트","",twitter.text)
twitter.text <- gsub("알티만","",twitter.text)
twitter.text <- gsub("VTFUL","",twitter.text)
twitter.text <- gsub("포기","",twitter.text)
twitter.text <- gsub("태형","",twitter.text)
twitter.text <- gsub("해주싱붕","",twitter.text)
twitter.text <- gsub("BTS","",twitter.text)
twitter.text <- gsub("해서","맛있다",twitter.text)
twitter.text <- gsub("준비","맛집",twitter.text)
twitter.text <- gsub("이후","크리스마스",twitter.text)
twitter.text <- gsub("제외","존맛",twitter.text)
# 문자분리 extracNoun:KoNLP 패키의 함수로 명사를 추출한다.
result_nouns <- Map(extractNoun, twitter.text)
# 쓸모없는 문자들을 제거한다. 특히 영자의 경우 tm의 stopwords를 활용한다.
result_wordsvec <- unlist(result_nouns, use.name = F)
# 영어 삭제(이거 하면 데이터 다날아감 이유를 모르겠음)
#result_wordsvec <- result_wordsvec[-which(result_wordsvec %in% stopwords("english"))]
# ?????
result_wordsvec <- gsub("[[:punct:]]","",result_wordsvec)
# 2개이상 글자만
result_wordsvec <- Filter(function(x){nchar(x) >=2}, result_wordsvec)
result_wordsvec <- Filter(function(x) { nchar(x) < 6 }, result_wordsvec)


# 단어별 카운팅
twitter_count <- table(result_wordsvec)
tmp <- head(sort(twitter_count,decreasing = T),100)
# 컬러세팅
pal <- brewer.pal(12,"Paired")
# 폰트세팅
#windowsFonts(malgun=windowsFont("Arial"))
# 그리기
#wordcloud(names(tmp),
#          freq=tmp,
#          scale=c(3,0.5),          
#          min.freq = 7,
#          random.order = F,
#          random.color = T,
#          rot.per = 0.4,
#          colors = brewer.pal(8,"Dark2"),
#          family="malgun"
#          )
wordcloud2(data = tmp,
           size = 3,
           color = "random-light",
           backgroundColor = "black",
           rotateRatio = 0.8,
           shape = 'pentagon'
)

